# sponsor_utils.R
# Settings and helper functions for the sponsored-videos dataset
# (sponsor_pipeline.R and poll_views.R both source this file).
# Run everything from the project folder, the one containing this file.
#
# The YouTube API key is read from YT_API_KEY (put it in ~/.Renviron).
# It is sent as a request header, never in the URL, and it is scrubbed from
# every error message, so it should never end up on screen or in a file.

suppressPackageStartupMessages({
  library(data.table)
  library(httr2)
  library(jsonlite)
})
# fwrite would otherwise write 1000000 views as "1e+06".
options(scipen = 999)
# Descriptions are UTF-8; make sure regexes treat them that way.
if (!isTRUE(l10n_info()$`UTF-8`)) invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))

# ---- Paths ------------------------------------------------------------------
# data/            : every table we build (CSV)
# data/raw/        : raw API responses, gzipped JSON, so tables can be rebuilt
#                    without calling the APIs again
# data/cache/      : per-call caches (SponsorBlock, Wikipedia, Trends) that let
#                    a script be stopped and resumed
# SPONSOR_DATA_DIR lets the 5-channel test run write to its own folder.
DATA_DIR  <- Sys.getenv("SPONSOR_DATA_DIR", "data")
RAW_DIR   <- file.path(DATA_DIR, "raw")
CACHE_DIR <- file.path(DATA_DIR, "cache")
for (d in c(RAW_DIR, CACHE_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
dpath <- function(...) file.path(DATA_DIR, ...)

# Hand-edited inputs live next to the scripts (data/ is not in git).
SEEDS_FILE      <- "channel_seeds.csv"
CANDIDATES_FILE <- "channels_candidates.csv"
CHANNELS_FILE   <- "channels.csv"
BRANDS_FILE     <- "brands.csv"

# ---- Rules ------------------------------------------------------------------
SEED            <- 20564
SINCE_DATE      <- as.Date("2025-01-01")   # uploads from this date on
MIN_DURATION    <- 60      # seconds; videos this short or shorter are Shorts
MIN_AGE_LABEL   <- 14      # days; only label videos at least this old
POLL_WINDOW     <- 60      # days; poll_views.R follows uploads this recent
SUBS_RANGE      <- c(1e5, 2e6)
N_PER_CATEGORY  <- 50      # 5 categories x 50 = about 250 channels
CATEGORIES      <- c("tech", "science_education", "gaming", "lifestyle", "commentary")
EN_COUNTRIES    <- c("US", "GB", "CA", "AU", "IE", "NZ")
EVENT_WINDOW    <- 28      # days before and after upload
RARE_MAX        <- 3       # rare_brand if the brand has this many events or fewer
MAX_COMMENTS    <- 500     # top-level comments per video (5 pages of 100)
TRENDS_GEO      <- Sys.getenv("SPONSOR_TRENDS_GEO", "GB")
TRENDS_SLEEP    <- c(8, 15)  # seconds between Trends calls
SB_SLEEP        <- as.numeric(Sys.getenv("SPONSOR_SB_SLEEP", "0.5"))  # between SponsorBlock calls

# ---- Contact and user agent -------------------------------------------------
CONTACT_EMAIL <- Sys.getenv("SPONSOR_CONTACT_EMAIL", "marta.carli@studbocconi.it")
USER_AGENT    <- sprintf("BocconiStudentProject-20564/1.0 (%s) R-httr2", CONTACT_EMAIL)

check_user_agent <- function() {
  ok <- grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", USER_AGENT) &&
    !grepl("example\\.(com|org)", USER_AGENT)
  if (!ok) stop("USER_AGENT has no real email address. Set SPONSOR_CONTACT_EMAIL.", call. = FALSE)
  invisible(TRUE)
}

# ---- YouTube API key (never printed) ----------------------------------------
yt_key <- function() {
  k <- Sys.getenv("YT_API_KEY")
  if (!nzchar(k)) stop("YT_API_KEY is not set. Add it to ~/.Renviron and restart R.", call. = FALSE)
  k
}
has_yt_key <- function() nzchar(Sys.getenv("YT_API_KEY"))

# Remove the key from any text before it is shown or written.
scrub <- function(x) {
  k <- Sys.getenv("YT_API_KEY")
  if (nzchar(k)) x <- gsub(k, "<KEY>", x, fixed = TRUE)
  gsub("key=[A-Za-z0-9_-]{20,}", "key=<KEY>", x)
}

# ---- Quota ------------------------------------------------------------------
# YouTube quota resets at midnight Pacific time, so "today" is a Pacific date.
DAILY_QUOTA  <- as.integer(Sys.getenv("SPONSOR_DAILY_QUOTA", "8000"))
QUOTA_LOG    <- "quota_log.csv"     # in DATA_DIR
YT_ENDPOINTS <- c("videos", "playlistItems", "channels", "commentThreads")  # nothing else, ever

quota_day <- function() format(Sys.time(), "%Y-%m-%d", tz = "America/Los_Angeles")

quota_used_today <- function() {
  f <- dpath(QUOTA_LOG)
  if (!file.exists(f)) return(0L)
  q <- fread(f, colClasses = c(quota_day = "character"))
  as.integer(q[quota_day == quota_day(), sum(units)])
}

log_quota <- function(endpoint, units, script) {
  fwrite(data.table(quota_day = quota_day(), time_utc = format(Sys.time(), tz = "UTC"),
                    endpoint = endpoint, units = units, script = script),
         dpath(QUOTA_LOG), append = file.exists(dpath(QUOTA_LOG)))
}

# Raised when the next call would go over the daily limit. Callers catch it,
# save what they have and stop; the caches let the next run carry on.
quota_exceeded <- function() {
  structure(class = c("quota_exceeded", "error", "condition"),
            list(message = sprintf("Daily quota limit (%d units) reached. Run again tomorrow.",
                                   DAILY_QUOTA), call = NULL))
}

SCRIPT_NAME <- "interactive"

# ---- Raw JSON storage -------------------------------------------------------
raw_path <- function(subdir, name) {
  d <- file.path(RAW_DIR, subdir); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, paste0(gsub("[^A-Za-z0-9_.@-]", "_", name), ".json.gz"))
}
save_raw <- function(x, path) {
  con <- gzfile(path, "w"); on.exit(close(con))
  writeLines(toJSON(x, auto_unbox = TRUE, null = "null", digits = NA), con, useBytes = TRUE)
}
read_raw <- function(path) {
  con <- gzfile(path, "r"); on.exit(close(con))
  fromJSON(paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
           simplifyVector = FALSE)
}

# ---- YouTube Data API call --------------------------------------------------
# One call = 1 unit for all four endpoints we use. Every response is saved to
# data/raw/<raw_subdir>/<raw_name>.json.gz. If that file already exists and
# use_raw is TRUE, it is read back instead of calling the API (no quota).
# raw_subdir = NULL means "do not save" (used for comments, which hold text
# and user names we must not keep).
yt_get <- function(endpoint, params, raw_subdir = NULL, raw_name = NULL, use_raw = TRUE) {
  stopifnot(endpoint %in% YT_ENDPOINTS)
  rp <- if (is.null(raw_subdir)) NULL else raw_path(raw_subdir, raw_name)
  if (use_raw && !is.null(rp) && file.exists(rp)) return(read_raw(rp))
  if (quota_used_today() + 1L > DAILY_QUOTA) stop(quota_exceeded())

  req <- request("https://www.googleapis.com/youtube/v3") |>
    req_url_path_append(endpoint) |>
    req_url_query(!!!params) |>
    req_headers(`X-Goog-Api-Key` = yt_key(), .redact = "X-Goog-Api-Key") |>
    req_user_agent(USER_AGENT) |>
    req_error(is_error = function(resp) FALSE) |>
    req_retry(max_tries = 4, is_transient = function(resp) resp_status(resp) %in% c(429, 500, 503),
              backoff = function(i) 2^i)
  resp <- tryCatch(req_perform(req), error = function(e) stop(scrub(conditionMessage(e)), call. = FALSE))
  log_quota(endpoint, 1L, SCRIPT_NAME)
  body <- tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) list())
  if (resp_status(resp) >= 400) {
    reason <- tryCatch(body$error$errors[[1]]$reason, error = function(e) NULL)
    msg <- tryCatch(body$error$message, error = function(e) NULL)
    if (identical(reason, "quotaExceeded")) stop(quota_exceeded())
    # Comments switched off or video gone: not an error for our purposes.
    return(list(.error = TRUE, status = resp_status(resp),
                reason = if (is.null(reason)) NA_character_ else reason,
                message = scrub(if (is.null(msg)) "" else msg)))
  }
  if (!is.null(rp)) save_raw(body, rp)
  body
}

# ---- Small helpers ----------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
chunks <- function(x, n = 50) split(x, ceiling(seq_along(x) / n))
uploads_playlist <- function(channel_id) sub("^UC", "UU", channel_id)

# ISO 8601 duration ("PT1H2M3S", "P1DT2H") to seconds.
parse_duration <- function(x) {
  part <- function(re) {
    m <- regmatches(x, regexec(re, x))
    vapply(m, function(v) if (length(v) == 2) as.numeric(v[2]) else 0, 0)
  }
  out <- part("P([0-9]+)D") * 86400 + part("T.*?([0-9]+)H") * 3600 +
    part("T.*?([0-9]+)M") * 60 + part("T.*?([0-9]+)S")
  out[is.na(x) | !grepl("^P", x)] <- NA_real_
  out
}

parse_iso <- function(x) as.POSIXct(sub("Z$", "", sub("T", " ", x)), tz = "UTC",
                                    format = "%Y-%m-%d %H:%M:%OS")

# ---- Channels ---------------------------------------------------------------
CHANNEL_PARTS <- "snippet,statistics,contentDetails,status,topicDetails"

channel_row <- function(it) {
  sn <- it$snippet; st <- it$statistics
  data.table(
    channel_id        = it$id,
    title             = chr(sn$title),
    handle            = chr(sn$customUrl),
    country           = chr(sn$country),
    default_language  = chr(sn$defaultLanguage),
    subscriber_count  = num(st$subscriberCount),
    hidden_subs       = isTRUE(st$hiddenSubscriberCount),
    view_count        = num(st$viewCount),
    video_count       = num(st$videoCount),
    made_for_kids     = isTRUE(it$status$madeForKids),
    topics            = paste(sub(".*/wiki/", "", unlist(it$topicDetails$topicCategories)), collapse = ";"),
    published_at      = chr(sn$publishedAt)
  )
}

# Look up one handle (with or without @). Returns NULL if it does not exist.
channel_by_handle <- function(handle) {
  h <- sub("^@?", "@", trimws(handle))
  r <- yt_get("channels", list(part = CHANNEL_PARTS, forHandle = h),
              "channels_by_handle", tolower(h))
  if (isTRUE(r$.error) || !length(r$items)) return(NULL)
  channel_row(r$items[[1]])
}

# Look up channel ids, 50 per call. raw_tag separates e.g. daily polls.
channels_by_id <- function(ids, raw_subdir = "channels_by_id", use_raw = TRUE, raw_tag = "") {
  ids <- unique(ids[!is.na(ids)])
  rbindlist(lapply(chunks(ids), function(b) {
    r <- yt_get("channels", list(part = CHANNEL_PARTS, id = paste(b, collapse = ","), maxResults = 50),
                raw_subdir, paste0(raw_tag, digest_ids(b)), use_raw = use_raw)
    if (isTRUE(r$.error)) return(NULL)
    rbindlist(lapply(r$items, channel_row))
  }), fill = TRUE)
}

# Short stable name for a batch of ids (for raw file names): first id, size
# and an md5 of the whole sorted batch, so a changed batch never reuses a file.
digest_ids <- function(ids) {
  f <- tempfile(); on.exit(unlink(f))
  writeLines(paste(sort(ids), collapse = ","), f)
  sprintf("%s_%d_%s", ids[1], length(ids), substr(unname(tools::md5sum(f)), 1, 10))
}

# mostPopular chart for one region and category: a cheap (1 unit per page)
# way to find channels, since search is not allowed.
most_popular_channels <- function(region, category_id, pages = 4) {
  out <- list(); token <- NULL
  for (p in seq_len(pages)) {
    params <- list(part = "snippet", chart = "mostPopular", regionCode = region,
                   videoCategoryId = category_id, maxResults = 50)
    if (!is.null(token)) params$pageToken <- token
    r <- yt_get("videos", params, "most_popular", sprintf("%s_%s_p%d_%s", region, category_id, p,
                                                           format(Sys.Date(), "%Y%m%d")))
    if (isTRUE(r$.error) || !length(r$items)) break
    out[[p]] <- data.table(
      channel_id = vapply(r$items, function(i) i$snippet$channelId, ""),
      video_category = category_id, region = region,
      audio_language = vapply(r$items, function(i) chr(i$snippet$defaultAudioLanguage), ""))
    token <- r$nextPageToken
    if (is.null(token)) break
  }
  rbindlist(out)
}

# ---- Uploads ----------------------------------------------------------------
# Walk a channel's uploads playlist (newest first) until videos are older
# than `since`. Returns video ids and their publish times.
channel_uploads <- function(channel_id, since = SINCE_DATE, use_raw = TRUE, raw_tag = "",
                            max_pages = 200) {
  pl <- uploads_playlist(channel_id)
  out <- list(); token <- NULL
  for (p in seq_len(max_pages)) {
    params <- list(part = "contentDetails", playlistId = pl, maxResults = 50)
    if (!is.null(token)) params$pageToken <- token
    r <- yt_get("playlistItems", params, file.path("playlistItems", channel_id),
                sprintf("%sp%03d", raw_tag, p), use_raw = use_raw)
    if (isTRUE(r$.error) || !length(r$items)) break
    d <- data.table(
      video_id     = vapply(r$items, function(i) i$contentDetails$videoId, ""),
      published_at = parse_iso(vapply(r$items, function(i) chr(i$contentDetails$videoPublishedAt), "")))
    out[[p]] <- d
    token <- r$nextPageToken
    # The playlist is newest first; stop once a whole page is older than `since`.
    if (is.null(token) || all(is.na(d$published_at) | as.Date(d$published_at) < since)) break
  }
  res <- rbindlist(out)
  if (!nrow(res)) return(data.table(channel_id = character(), video_id = character(), published_at = as.POSIXct(character())))
  res[, channel_id := channel_id][as.Date(published_at) >= since | is.na(published_at)]
}

# ---- Video details ----------------------------------------------------------
video_row <- function(it, retrieved_at) {
  sn <- it$snippet; st <- it$statistics; cd <- it$contentDetails
  data.table(
    video_id      = it$id,
    channel_id    = chr(sn$channelId),
    channel_title = chr(sn$channelTitle),
    title         = chr(sn$title),
    description   = chr(sn$description),
    published_at  = chr(sn$publishedAt),
    category_id   = chr(sn$categoryId),
    language      = chr(sn$defaultAudioLanguage %||% sn$defaultLanguage),
    duration_sec  = parse_duration(chr(cd$duration)),
    live          = chr(sn$liveBroadcastContent),
    views         = num(st$viewCount),
    likes         = num(st$likeCount),
    comments      = num(st$commentCount),
    retrieved_at  = retrieved_at
  )
}

# Details for many videos, 50 per call.
video_details <- function(ids, raw_subdir = "videos", use_raw = TRUE, raw_tag = "") {
  ids <- unique(ids)
  rbindlist(lapply(chunks(ids), function(b) {
    name <- paste0(raw_tag, digest_ids(b))
    rp <- raw_path(raw_subdir, name)
    fresh <- !(use_raw && file.exists(rp))
    r <- yt_get("videos", list(part = "snippet,contentDetails,statistics", id = paste(b, collapse = ","),
                               maxResults = 50), raw_subdir, name, use_raw = use_raw)
    if (isTRUE(r$.error)) return(NULL)
    # retrieved_at = when the raw file was written, so rebuilds keep the real time
    when <- format(if (fresh) Sys.time() else file.mtime(rp), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    rbindlist(lapply(r$items, video_row, retrieved_at = when))
  }), fill = TRUE)
}

# ---- SponsorBlock -----------------------------------------------------------
# One call per video, cached as raw JSON in data/cache/sponsorblock/. A 404
# means "no sponsor segments submitted", which we cache as an empty list.
sb_segments <- function(video_id) {
  f <- file.path(CACHE_DIR, "sponsorblock", paste0(video_id, ".json"))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(f)) return(fromJSON(f, simplifyVector = FALSE))
  req <- request("https://sponsor.ajay.app/api/skipSegments") |>
    req_url_query(videoID = video_id, category = "sponsor",
                  actionTypes = '["skip","mute","full"]') |>
    req_user_agent(USER_AGENT) |>
    req_error(is_error = function(resp) FALSE) |>
    req_retry(max_tries = 5, is_transient = function(resp) resp_status(resp) %in% c(429, 502, 503, 504),
              backoff = function(i) 5 * 2^i)
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  Sys.sleep(SB_SLEEP)
  if (is.null(resp)) return(NULL)                       # network trouble: not cached, retried next run
  s <- resp_status(resp)
  if (s == 404) { writeLines("[]", f); return(list()) }
  if (s != 200) return(NULL)
  txt <- resp_body_string(resp)
  writeLines(txt, f, useBytes = TRUE)
  fromJSON(txt, simplifyVector = FALSE)
}

# Merge overlapping segments (several users often submit the same read with
# slightly different times) and summarise one video.
sb_summary <- function(segs, duration) {
  na <- data.table(sb_sponsored = NA, sb_full_video = NA, sb_n_segments = NA_integer_,
                   sb_ad_seconds = NA_real_, sb_first_start = NA_real_)
  if (is.null(segs)) return(na)
  if (!length(segs)) return(data.table(sb_sponsored = FALSE, sb_full_video = FALSE, sb_n_segments = 0L,
                                       sb_ad_seconds = 0, sb_first_start = NA_real_))
  d <- rbindlist(lapply(segs, function(s) data.table(
    start = as.numeric(s$segment[[1]]), end = as.numeric(s$segment[[2]]),
    action = chr(s$actionType), category = chr(s$category))))
  d <- d[category == "sponsor"]
  full <- any(d$action == "full")
  d <- d[action != "full" & end > start][order(start)]
  if (nrow(d)) {
    d[, grp := cumsum(c(1L, as.integer(start[-1] > cummax(end)[-.N])))]
    m <- d[, .(start = min(start), end = max(end)), by = grp]
  } else m <- data.table(start = numeric(), end = numeric())
  data.table(sb_sponsored = full || nrow(m) > 0, sb_full_video = full, sb_n_segments = nrow(m),
             sb_ad_seconds = sum(m$end - m$start),
             sb_first_start = if (nrow(m)) m$start[1] else NA_real_)
}

ad_position <- function(first_start, duration) {
  share <- first_start / duration
  fifelse(is.na(share), NA_character_,
          fifelse(share < 0.15, "early", fifelse(share < 0.60, "middle", "late")))
}

# ---- Description text -------------------------------------------------------
# Case-insensitive. ".{0,3}" stands in for the apostrophe so curly and
# straight quotes both match whatever the locale.
SPONSOR_PATTERNS <- c(
  sponsored_by   = "sponsored by",
  thanks_for     = "thanks? (you )?to .{1,80}? for sponsoring",
  todays_sponsor = "today.{0,3}s sponsor",
  sponsor_of     = "(this|today.{0,3}s) video is sponsored",
  use_code       = "use (my |our |the )?code",
  promo_code     = "(promo|coupon|discount) code",
  paid_promotion = "paid promotion",
  hashtag_ad     = "#ad\\b",
  hashtag_spon   = "#sponsored\\b"
)
desc_sponsor_flags <- function(desc) {
  d <- tolower(fifelse(is.na(desc), "", desc))
  m <- vapply(SPONSOR_PATTERNS, function(p) grepl(p, d, perl = TRUE), logical(length(d)))
  if (!is.matrix(m)) m <- matrix(m, nrow = length(d), dimnames = list(NULL, names(SPONSOR_PATTERNS)))
  m
}

# First promo code in a description ("use code TECH20", "promo code: abc").
extract_promo_code <- function(desc) {
  m <- regmatches(desc, regexec("(?i)(?:use|promo|coupon|discount)\\s+(?:my\\s+|our\\s+|the\\s+)?code[^A-Za-z0-9_]{0,6}([A-Za-z0-9_-]{3,24})",
                                desc, perl = TRUE))
  code <- vapply(m, function(v) if (length(v) == 2) v[2] else NA_character_, "")
  # "use code at checkout" is not a code
  code[tolower(code) %in% c("at", "for", "and", "the", "below", "link", "here", "when", "checkout")] <- NA
  code
}

# ---- Links ------------------------------------------------------------------
EXCLUDED_DOMAINS <- c(
  # social media and video platforms
  "youtube.com", "youtu.be", "twitter.com", "x.com", "instagram.com", "facebook.com", "fb.com",
  "fb.me", "tiktok.com", "twitch.tv", "discord.gg", "discord.com", "reddit.com", "threads.net",
  "snapchat.com", "linkedin.com", "pinterest.com", "tumblr.com", "bsky.app", "mastodon.social",
  "vimeo.com", "kick.com", "t.me", "telegram.me", "whatsapp.com", "steamcommunity.com",
  "music.apple.com", "open.spotify.com", "soundcloud.com", "goo.gl",
  # link hubs
  "linktr.ee", "beacons.ai", "bio.link", "campsite.bio", "solo.to", "lnk.bio", "linkin.bio",
  "allmylinks.com", "msha.ke", "hoo.be", "carrd.co", "komi.io", "stan.store", "taplink.cc",
  # fan funding
  "patreon.com", "ko-fi.com", "buymeacoffee.com", "paypal.me", "paypal.com", "streamlabs.com",
  "streamelements.com", "subscribestar.com", "gofundme.com", "throne.com", "fourthwall.com",
  "memberful.com", "gumroad.com", "substack.com"
)
LINK_SHORTENERS <- c("bit.ly", "geni.us", "amzn.to", "tinyurl.com", "ow.ly", "rebrand.ly",
                     "shorturl.at", "cutt.ly", "t.co", "lnk.to", "go.magik.ly", "tidd.ly",
                     "amzlink.to", "is.gd", "buff.ly", "dub.sh", "spoti.fi", "apple.co")

url_domain <- function(url) {
  h <- tolower(sub("^[a-z]+://([^/?#:]+).*$", "\\1", url, ignore.case = TRUE))
  sub("^(www|m|mobile)\\.", "", h)
}
domain_excluded <- function(domain) {
  vapply(domain, function(d) any(d == EXCLUDED_DOMAINS | endsWith(d, paste0(".", EXCLUDED_DOMAINS))), TRUE)
}

extract_links <- function(video_id, desc) {
  m <- regmatches(desc, gregexpr("https?://[^\\s<>\"'()\\[\\]{}]+", desc, perl = TRUE))
  d <- data.table(video_id = rep(video_id, lengths(m)), url = unlist(m))
  if (!nrow(d)) return(data.table(video_id = character(), url = character(), domain = character()))
  d[, url := sub("[.,;:!?*]+$", "", url)]
  d[, domain := url_domain(url)]
  unique(d[!domain_excluded(domain) & grepl("\\.", domain)])
}
