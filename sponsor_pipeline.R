# sponsor_pipeline.R
# Builds the sponsored-videos dataset, one stage at a time. Nothing here fits
# a model. Run from the project folder:
#
#   Rscript sponsor_pipeline.R check             # packages, API key, user agent
#   Rscript sponsor_pipeline.R candidates        # -> channels_candidates.csv  (STOP: review)
#   Rscript sponsor_pipeline.R channels          # selected candidates -> channels.csv
#   Rscript sponsor_pipeline.R videos            # -> data/videos.csv
#   Rscript sponsor_pipeline.R label             # -> data/videos_labelled.csv, data/description_links.csv
#   Rscript sponsor_pipeline.R handcheck         # -> data/handcheck_unsponsored.csv
#   Rscript sponsor_pipeline.R brand_candidates  # -> data/brand_candidates.csv  (then draft brands.csv, STOP)
#   Rscript sponsor_pipeline.R events            # -> data/sponsor_events.csv
#   Rscript sponsor_pipeline.R panel             # -> data/brand_event_panel.csv (Wikipedia + Trends)
#   Rscript sponsor_pipeline.R comments          # -> data/comment_mentions.csv
#   Rscript sponsor_pipeline.R summary           # counts for the README log
#
# Test run on 5 channels, written to data/test/ so it never mixes with the
# real data:
#   SPONSOR_TEST_N=5 SPONSOR_DATA_DIR=data/test Rscript sponsor_pipeline.R videos
#
# Every stage can be stopped and re-run: API responses are kept in data/raw/
# and data/cache/, and a stage reuses them instead of calling the API again.

source("sponsor_utils.R")
SCRIPT_NAME <- "sponsor_pipeline"
set.seed(SEED)

args  <- commandArgs(trailingOnly = TRUE)
STAGE <- if (length(args)) args[1] else "check"
TEST_N <- as.integer(Sys.getenv("SPONSOR_TEST_N", "0"))

say <- function(...) message(format(Sys.time(), "%H:%M:%S "), ...)
read_csv <- function(f, ...) fread(f, encoding = "UTF-8", ...)
need <- function(f) if (!file.exists(f)) stop("Missing ", f, ". Run the earlier stage first.", call. = FALSE)

# Run a step; if the daily quota runs out, keep what is saved and stop cleanly.
with_quota <- function(expr) {
  tryCatch(expr, quota_exceeded = function(e) {
    say(conditionMessage(e), " Units used today: ", quota_used_today())
    FALSE
  })
}

# ---- Stage: check -----------------------------------------------------------
stage_check <- function() {
  pk <- c("data.table", "httr2", "jsonlite", "gtrendsR")
  model_pk <- c("ggplot2", "fixest", "survival", "quanteda", "caret", "tidymodels", "flexmix")
  missing <- pk[!vapply(pk, requireNamespace, TRUE, quietly = TRUE)]
  if (length(missing)) {
    say("Installing from CRAN: ", paste(missing, collapse = ", "))
    try(install.packages(missing, repos = "https://cloud.r-project.org"))
  }
  for (p in c(pk, model_pk)) say(sprintf("  %-11s %s", p, if (requireNamespace(p, quietly = TRUE)) "ok" else "MISSING"))
  say("YT_API_KEY: ", if (has_yt_key()) "set" else "NOT SET (add it to ~/.Renviron)")
  ok <- tryCatch(check_user_agent(), error = function(e) FALSE)
  say("USER_AGENT has a real email: ", if (isTRUE(ok)) paste("yes,", CONTACT_EMAIL) else "NO")
  say("Quota used today (Pacific day ", quota_day(), "): ", quota_used_today(), " / ", DAILY_QUOTA)
}

# ---- Stage: candidates (item 1) ---------------------------------------------
# Search is not allowed (100 units per call), so channels come from two cheap
# sources: (a) channel_seeds.csv, hand-picked handles, 1 unit each, and
# (b) the mostPopular chart by category in four English-speaking regions,
# 1 unit per page of 50 videos. Both are then filtered by the same rule.
MP_CATEGORY_MAP <- c("28" = "tech", "27" = "science_education", "20" = "gaming",
                     "26" = "lifestyle", "22" = "lifestyle", "24" = "commentary")
MP_REGIONS <- c("US", "GB", "CA", "AU")

stage_candidates <- function() {
  check_user_agent(); yt_key()
  seeds <- read_csv(SEEDS_FILE)
  say("Looking up ", nrow(seeds), " seed handles")
  seed_rows <- with_quota(rbindlist(lapply(seq_len(nrow(seeds)), function(i) {
    r <- channel_by_handle(seeds$handle[i])
    if (is.null(r)) { say("  not found: @", seeds$handle[i]); return(NULL) }
    r[, `:=`(category = seeds$category[i], source = "seed", seed_handle = seeds$handle[i])]
  }), fill = TRUE))
  if (isFALSE(seed_rows)) return(invisible())

  say("Reading mostPopular charts")
  mp <- with_quota(rbindlist(lapply(MP_REGIONS, function(r) rbindlist(lapply(names(MP_CATEGORY_MAP),
          function(cat) most_popular_channels(r, cat))))))
  if (isFALSE(mp)) return(invisible())
  # A channel's category is the one most of its chart videos had.
  mp_ch <- mp[, .(n = .N, en_audio = mean(grepl("^en", audio_language), na.rm = TRUE)),
              by = .(channel_id, video_category)][order(-n)][, .SD[1], by = channel_id]
  mp_ch <- mp_ch[!channel_id %in% seed_rows$channel_id]
  mp_rows <- with_quota(channels_by_id(mp_ch$channel_id))
  if (isFALSE(mp_rows)) return(invisible())
  mp_rows <- merge(mp_rows, mp_ch[, .(channel_id, video_category, en_audio)], by = "channel_id")
  mp_rows[, `:=`(category = MP_CATEGORY_MAP[video_category], source = "mostPopular")]

  cand <- rbindlist(list(seed_rows, mp_rows), fill = TRUE)
  cand <- unique(cand, by = "channel_id")
  cand[, english := fifelse(source == "seed", is.na(country) | country %in% EN_COUNTRIES,
                            country %in% EN_COUNTRIES | (is.na(country) & grepl("^en", default_language)) |
                              (is.na(country) & !is.na(en_audio) & en_audio > 0.5))]
  cand[, eligible := !hidden_subs & between(subscriber_count, SUBS_RANGE[1], SUBS_RANGE[2]) &
         english & !made_for_kids & video_count >= 20]
  cand[is.na(eligible), eligible := FALSE]
  # Up to N_PER_CATEGORY per category; if a category has more, draw at random.
  cand[, r := sample(.N), by = .(category, eligible)]
  cand[, selected := eligible & r <= N_PER_CATEGORY]
  cand[, r := NULL]
  setorder(cand, -selected, category, -subscriber_count)
  keep <- c("channel_id", "title", "handle", "subscriber_count", "category", "selected", "eligible",
            "source", "country", "default_language", "video_count", "view_count", "made_for_kids", "topics")
  fwrite(cand[, ..keep], CANDIDATES_FILE)
  say("Wrote ", CANDIDATES_FILE, ": ", nrow(cand), " channels looked up, ", cand[, sum(eligible)],
      " eligible, ", cand[, sum(selected)], " selected")
  print(cand[, .(looked_up = .N, eligible = sum(eligible), selected = sum(selected)), by = category])
  say("Quota used today: ", quota_used_today())
}

# ---- Stage: channels --------------------------------------------------------
stage_channels <- function() {
  need(CANDIDATES_FILE)
  cand <- read_csv(CANDIDATES_FILE)
  ch <- cand[selected == TRUE, .(channel_id, title, handle, subscriber_count, category)]
  fwrite(ch, CHANNELS_FILE)
  say("Wrote ", CHANNELS_FILE, " with ", nrow(ch), " channels")
}

panel_channels <- function() {
  need(CHANNELS_FILE)
  ch <- read_csv(CHANNELS_FILE)
  if (TEST_N > 0) ch <- ch[sample(.N, min(TEST_N, .N))]   # random, set.seed(SEED) above
  ch
}

# ---- Stage: videos (item 2) -------------------------------------------------
stage_videos <- function() {
  yt_key()
  ch <- panel_channels()
  say("Uploads since ", SINCE_DATE, " for ", nrow(ch), " channels")
  # Uploads lists are refreshed per run day so new videos are picked up;
  # a re-run on the same day reuses the saved pages.
  tag <- format(Sys.Date(), "%Y%m%d_")
  up <- list()
  ok <- with_quota({
    for (i in seq_len(nrow(ch))) {
      up[[i]] <- channel_uploads(ch$channel_id[i], raw_tag = tag)
      if (i %% 25 == 0) say("  ", i, " channels, ", sum(vapply(up, nrow, 0L)), " uploads")
    }
    TRUE
  })
  up <- rbindlist(up)
  say("Uploads found: ", nrow(up))
  vids <- with_quota(video_details(up$video_id, raw_tag = tag))
  if (isFALSE(vids) || isFALSE(ok)) { say("Stopped on quota; run again tomorrow."); return(invisible()) }
  n0 <- nrow(vids)
  vids <- vids[!is.na(duration_sec) & duration_sec > MIN_DURATION & live == "none"]
  vids[, live := NULL]
  setorder(vids, channel_id, published_at)
  fwrite(vids, dpath("videos.csv"))
  say("Wrote data/videos.csv: ", nrow(vids), " videos (dropped ", n0 - nrow(vids),
      " Shorts, live or upcoming streams). Quota used today: ", quota_used_today())
}

# ---- Stage: label (item 4) --------------------------------------------------
stage_label <- function() {
  check_user_agent()
  need(dpath("videos.csv"))
  v <- read_csv(dpath("videos.csv"), colClasses = c(description = "character"))
  if (TEST_N > 0) v <- v[channel_id %in% panel_channels()$channel_id]

  # Outbound links for every video (not only labelled ones).
  links <- rbindlist(Map(extract_links, v$video_id, v$description))
  fwrite(links, dpath("description_links.csv"))
  say("Wrote data/description_links.csv: ", nrow(links), " links from ",
      uniqueN(links$video_id), " videos")

  v[, age_days := as.numeric(Sys.Date() - as.Date(substr(published_at, 1, 10)))]
  lab <- v[age_days >= MIN_AGE_LABEL]
  say("Labelling ", nrow(lab), " videos at least ", MIN_AGE_LABEL, " days old (",
      nrow(v) - nrow(lab), " too recent)")

  # SponsorBlock, one call per video, cached; safe to stop and re-run.
  todo <- lab$video_id[!file.exists(file.path(CACHE_DIR, "sponsorblock", paste0(lab$video_id, ".json")))]
  say("SponsorBlock: ", length(lab$video_id) - length(todo), " cached, ", length(todo), " to fetch")
  for (i in seq_along(todo)) {
    sb_segments(todo[i])
    if (i %% 200 == 0) say("  SponsorBlock ", i, "/", length(todo))
  }
  sb <- rbindlist(lapply(seq_len(nrow(lab)), function(i) {
    f <- file.path(CACHE_DIR, "sponsorblock", paste0(lab$video_id[i], ".json"))
    segs <- if (file.exists(f)) fromJSON(f, simplifyVector = FALSE) else NULL
    cbind(video_id = lab$video_id[i], sb_summary(segs, lab$duration_sec[i]))
  }))
  lab <- merge(lab, sb, by = "video_id", all.x = TRUE)
  lab[, `:=`(sb_ad_share = sb_ad_seconds / duration_sec,
             sb_ad_position = ad_position(sb_first_start, duration_sec))]
  lab[sb_full_video == TRUE & sb_n_segments == 0, sb_ad_position := "full_video"]

  flags <- desc_sponsor_flags(lab$description)
  lab[, desc_sponsor_text := rowSums(flags) > 0]
  lab[, desc_patterns := apply(flags, 1, function(r) paste(names(r)[r], collapse = ";"))]
  lab[, promo_code := extract_promo_code(description)]

  lab[, sponsored := fifelse(is.na(sb_sponsored), desc_sponsor_text, sb_sponsored | desc_sponsor_text)]
  lab[, label_source := fcase(sb_sponsored & desc_sponsor_text, "both",
                              sb_sponsored & !desc_sponsor_text, "sponsorblock_only",
                              !sb_sponsored & desc_sponsor_text, "description_only",
                              !sb_sponsored & !desc_sponsor_text, "neither",
                              default = NA_character_)]   # NA = SponsorBlock call failed
  lab[, `:=`(age_days = NULL, labelled_on = as.character(Sys.Date()))]
  setcolorder(lab, c(names(v)[names(v) %in% names(lab)]))
  fwrite(lab, dpath("videos_labelled.csv"))
  say("Wrote data/videos_labelled.csv: ", nrow(lab), " videos")
  print(lab[, .N, by = label_source][order(-N)])
}

# ---- Stage: handcheck (item 5) ----------------------------------------------
stage_handcheck <- function() {
  need(dpath("videos_labelled.csv"))
  lab <- read_csv(dpath("videos_labelled.csv"), select = c("video_id", "title", "label_source"))
  pool <- lab[label_source == "neither"]
  set.seed(SEED)
  hc <- pool[sample(.N, min(100, .N)), .(video_id, url = paste0("https://www.youtube.com/watch?v=", video_id),
                                          title, ad_found = "")]
  f <- dpath("handcheck_unsponsored.csv")
  if (file.exists(f) && any(nzchar(read_csv(f, colClasses = c(ad_found = "character"))$ad_found)))
    stop(f, " already has your answers in ad_found; not overwriting it.", call. = FALSE)
  fwrite(hc, f)
  say("Wrote ", f, " with ", nrow(hc), " videos")
}

# ---- Stage: brand_candidates (item 6) ---------------------------------------
stage_brand_candidates <- function() {
  need(dpath("videos_labelled.csv")); need(dpath("description_links.csv"))
  lab <- read_csv(dpath("videos_labelled.csv"), select = c("video_id", "channel_id", "sponsored"))
  links <- read_csv(dpath("description_links.csv"))
  l <- merge(links, lab, by = "video_id")
  bc <- l[sponsored == TRUE, .(n_videos = uniqueN(video_id), n_channels = uniqueN(channel_id),
                               example_url = url[1]), by = domain]
  # How often the domain also appears in unsponsored videos: a link in every
  # video of one channel is usually the creator's own shop, not a sponsor.
  uns <- l[sponsored == FALSE, .(n_videos_unsponsored = uniqueN(video_id)), by = domain]
  bc <- merge(bc, uns, by = "domain", all.x = TRUE)
  bc[is.na(n_videos_unsponsored), n_videos_unsponsored := 0L]
  bc[, shortener := domain %in% LINK_SHORTENERS]
  setorder(bc, -n_channels, -n_videos)
  fwrite(bc, dpath("brand_candidates.csv"))
  say("Wrote data/brand_candidates.csv: ", nrow(bc), " domains")
  print(head(bc, 30))
}

# ---- Stage: events (item 7) -------------------------------------------------
read_brands <- function() {
  need(BRANDS_FILE)
  b <- read_csv(BRANDS_FILE, colClasses = "character")
  b[, unsure := toupper(unsure) %in% c("TRUE", "T", "1")]
  b[, domain := tolower(trimws(domain))]
  b
}

stage_events <- function() {
  need(dpath("videos_labelled.csv")); need(dpath("description_links.csv"))
  brands <- read_brands()
  lab <- read_csv(dpath("videos_labelled.csv"), colClasses = c(description = "character"))
  links <- read_csv(dpath("description_links.csv"))
  # A link matches a brand if its domain is the brand domain or a subdomain of it.
  hits <- rbindlist(lapply(seq_len(nrow(brands)), function(i) {
    d <- brands$domain[i]
    links[domain == d | endsWith(domain, paste0(".", d)), .(video_id, brand = brands$brand[i])]
  }))
  ev <- merge(unique(hits), lab[sponsored == TRUE], by = "video_id")
  ev <- merge(ev, brands[, .(brand, wiki_article, trends_term, unsure)], by = "brand")
  ev[, upload_day := as.IDate(substr(published_at, 1, 10))]
  ev[, brand_n_events := .N, by = brand]
  ev[, rare_brand := brand_n_events <= RARE_MAX]
  ev[, n_overlapping := vapply(seq_len(.N), function(i)
        sum(abs(as.integer(upload_day - upload_day[i])) <= EVENT_WINDOW) - 1L, 0L), by = brand]
  setorder(ev, upload_day, video_id, brand)
  ev[, event_id := sprintf("E%05d", .I)]
  cols <- c("event_id", "video_id", "brand", "channel_id", "channel_title", "title", "published_at",
            "upload_day", "views", "likes", "comments", "duration_sec", "sb_ad_seconds", "sb_ad_share",
            "sb_ad_position", "label_source", "promo_code", "brand_n_events", "rare_brand",
            "n_overlapping", "wiki_article", "trends_term", "unsure")
  fwrite(ev[, ..cols], dpath("sponsor_events.csv"))
  say("Wrote data/sponsor_events.csv: ", nrow(ev), " events, ", uniqueN(ev$brand), " brands, ",
      ev[rare_brand & n_overlapping == 0, .N], " rare and isolated")
}

# ---- Stage: panel (item 8) --------------------------------------------------
# Wikipedia: one call per brand covering all its events, cached.
wiki_views <- function(article, from, to) {
  f <- file.path(CACHE_DIR, "wiki", paste0(gsub("[^A-Za-z0-9_.-]", "_", article), "_",
                                           format(from, "%Y%m%d"), "_", format(to, "%Y%m%d"), ".json"))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(f)) {
    url <- sprintf("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/user/%s/daily/%s/%s",
                   utils::URLencode(gsub(" ", "_", article), reserved = TRUE),
                   format(from, "%Y%m%d"), format(to, "%Y%m%d"))
    resp <- tryCatch(request(url) |> req_user_agent(USER_AGENT) |>
                       req_error(is_error = function(r) FALSE) |>
                       req_retry(max_tries = 4, backoff = function(i) 5 * 2^i) |> req_perform(),
                     error = function(e) NULL)
    Sys.sleep(0.2)
    if (is.null(resp) || !resp_status(resp) %in% c(200, 404)) return(NULL)
    writeLines(if (resp_status(resp) == 404) '{"items":[]}' else resp_body_string(resp), f, useBytes = TRUE)
  }
  it <- fromJSON(f)$items
  if (!length(it)) return(data.table(date = as.IDate(character()), wiki_views = numeric()))
  data.table(date = as.IDate(as.Date(substr(it$timestamp, 1, 8), "%Y%m%d")), wiki_views = as.numeric(it$views))
}

# Google Trends: one query per event window and property, cached as .rds.
# Returns "ok", "too_recent", "rate_limited" or "error".
trends_fetch <- function(event_id, term, from, to, gprop) {
  f <- file.path(CACHE_DIR, "trends", sprintf("%s_%s_%s.rds", event_id, gprop, if (nzchar(TRENDS_GEO)) TRENDS_GEO else "world"))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(f)) return("ok")
  if (to > Sys.Date() - 3) return("too_recent")   # Trends daily data lags by a few days
  for (attempt in 1:3) {
    Sys.sleep(runif(1, TRENDS_SLEEP[1], TRENDS_SLEEP[2]))
    res <- tryCatch(gtrendsR::gtrends(keyword = term, geo = TRENDS_GEO, gprop = gprop,
                                      time = paste(from, to), onlyInterest = TRUE),
                    error = function(e) e)
    if (!inherits(res, "error")) {
      iot <- res$interest_over_time
      d <- if (is.null(iot) || !nrow(iot)) data.table(date = as.IDate(character()), hits = numeric())
           else data.table(date = as.IDate(as.Date(iot$date)),
                           hits = suppressWarnings(as.numeric(ifelse(iot$hits == "<1", "0.5", iot$hits))))
      saveRDS(list(term = term, geo = TRENDS_GEO, gprop = gprop, from = from, to = to,
                   retrieved_at = Sys.time(), data = d), f)
      return("ok")
    }
    msg <- conditionMessage(res)
    if (!grepl("429", msg)) { say("  Trends error for ", event_id, " ", gprop, ": ", msg); return("error") }
    wait <- 120 * 2^(attempt - 1)
    say("  Trends returned 429; waiting ", wait, " s (attempt ", attempt, ")")
    Sys.sleep(wait)
  }
  "rate_limited"
}

trends_available <- function() requireNamespace("gtrendsR", quietly = TRUE)

stage_panel <- function() {
  check_user_agent()
  need(dpath("sponsor_events.csv"))
  ev <- read_csv(dpath("sponsor_events.csv"), colClasses = c(wiki_article = "character", trends_term = "character"))
  ev[, upload_day := as.IDate(upload_day)]
  yesterday <- Sys.Date() - 1

  # Wikipedia: one request per brand, covering all of its events.
  wk <- ev[!is.na(wiki_article) & nzchar(wiki_article),
           .(from = min(upload_day) - EVENT_WINDOW, to = min(max(upload_day) + EVENT_WINDOW, yesterday)),
           by = .(brand, wiki_article)]
  wiki <- rbindlist(lapply(seq_len(nrow(wk)), function(i) {
    w <- wiki_views(wk$wiki_article[i], as.Date(wk$from[i]), as.Date(wk$to[i]))
    if (is.null(w)) { say("  Wikipedia failed for ", wk$brand[i]); return(NULL) }
    # The API leaves out days with no views; fill them with 0 inside the range.
    full <- data.table(date = seq(as.IDate(wk$from[i]), as.IDate(wk$to[i]), by = 1))
    w <- merge(full, w, by = "date", all.x = TRUE)
    if (nrow(w) && any(!is.na(w$wiki_views))) w[is.na(wiki_views), wiki_views := 0]
    w[, brand := wk$brand[i]]
  }))
  say("Wikipedia: ", uniqueN(wiki$brand), " of ", nrow(wk), " brands with an article")

  # Trends: rare and isolated events first, so a rate limit hurts them least.
  tr_ev <- ev[!is.na(trends_term) & nzchar(trends_term)]
  tr_ev[, priority := !(rare_brand & n_overlapping == 0)]
  setorder(tr_ev, priority, upload_day)
  if (!trends_available()) {
    say("gtrendsR is not installed; skipping Trends (run the check stage).")
  } else {
    set.seed(SEED)
    stopped <- FALSE
    for (i in seq_len(nrow(tr_ev))) {
      if (stopped) break
      for (gp in c("web", "youtube")) {
        st <- trends_fetch(tr_ev$event_id[i], tr_ev$trends_term[i], as.Date(tr_ev$upload_day[i]) - EVENT_WINDOW,
                           as.Date(tr_ev$upload_day[i]) + EVENT_WINDOW, gp)
        if (st == "rate_limited") {
          say("Google Trends keeps refusing (429). Stopping here; run the panel stage again later. ",
              "Everything fetched so far is cached.")
          stopped <- TRUE; break
        }
      }
      if (i %% 20 == 0) say("  Trends: ", i, "/", nrow(tr_ev), " events done")
    }
  }

  # Build the event x day panel.
  panel <- ev[, .(rel_day = -EVENT_WINDOW:EVENT_WINDOW), by = event_id]
  panel <- merge(panel, ev, by = "event_id")
  panel[, date := upload_day + rel_day]
  panel <- merge(panel, wiki, by = c("brand", "date"), all.x = TRUE)
  panel[date > yesterday, wiki_views := NA]
  read_tr <- function(eid, gp) {
    f <- file.path(CACHE_DIR, "trends", sprintf("%s_%s_%s.rds", eid, gp, if (nzchar(TRENDS_GEO)) TRENDS_GEO else "world"))
    if (!file.exists(f)) return(NULL)
    d <- readRDS(f)$data
    if (!nrow(d)) return(NULL)   # Trends had no data for the term
    d[, event_id := eid][]
  }
  for (gp in c("web", "youtube")) {
    t <- rbindlist(lapply(ev$event_id, read_tr, gp = gp))
    col <- paste0("trends_", gp)
    if (nrow(t)) { setnames(t, "hits", col); panel <- merge(panel, t, by = c("event_id", "date"), all.x = TRUE) }
    else panel[, (col) := NA_real_]
  }
  cols <- c("event_id", "video_id", "brand", "date", "rel_day", "wiki_views", "trends_web", "trends_youtube",
            "channel_id", "upload_day", "views", "likes", "comments", "duration_sec", "sb_ad_seconds",
            "sb_ad_share", "sb_ad_position", "label_source", "brand_n_events", "rare_brand", "n_overlapping")
  setorder(panel, event_id, rel_day)
  fwrite(panel[, ..cols], dpath("brand_event_panel.csv"))
  say("Wrote data/brand_event_panel.csv: ", nrow(panel), " rows for ", uniqueN(panel$event_id), " events")
}

# ---- Stage: comments (item 9) -----------------------------------------------
# Up to MAX_COMMENTS top-level comments per video (relevance order), 1 unit per
# page of 100. Only counts are kept: the comment text is matched in memory and
# thrown away, and the raw responses are NOT saved for this endpoint.
count_comment_mentions <- function(video_id, terms) {
  f <- file.path(CACHE_DIR, "comments", paste0(video_id, ".rds"))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(f)) return(readRDS(f))
  pats <- vapply(terms, function(t) paste0("\\b", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", tolower(t)), "\\b"), "")
  n <- 0L; hits <- setNames(integer(length(terms)), names(terms)); any_hit <- list()
  token <- NULL; status <- "ok"
  repeat {
    params <- list(part = "snippet", videoId = video_id, order = "relevance", maxResults = 100,
                   textFormat = "plainText")
    if (!is.null(token)) params$pageToken <- token
    r <- yt_get("commentThreads", params)   # not saved
    if (isTRUE(r$.error)) { status <- if (is.na(r$reason)) paste0("http_", r$status) else r$reason; break }
    txt <- tolower(vapply(r$items, function(i) chr(i$snippet$topLevelComment$snippet$textDisplay), ""))
    m <- vapply(pats, function(p) grepl(p, txt, perl = TRUE), logical(length(txt)))
    if (!is.matrix(m)) m <- matrix(m, ncol = length(pats), dimnames = list(NULL, names(pats)))
    any_hit[[length(any_hit) + 1]] <- m
    n <- n + length(txt)
    token <- r$nextPageToken
    if (is.null(token) || n >= MAX_COMMENTS) break
  }
  m <- if (length(any_hit)) do.call(rbind, any_hit) else matrix(FALSE, 0, length(pats), dimnames = list(NULL, names(pats)))
  res <- list(status = status, n = n, m = m)   # m is a logical matrix: comment x term, no text
  saveRDS(res, f)
  res
}

stage_comments <- function() {
  yt_key()
  need(dpath("sponsor_events.csv"))
  ev <- read_csv(dpath("sponsor_events.csv"), colClasses = c(promo_code = "character"))
  ev[, priority := !(rare_brand & n_overlapping == 0)]
  setorder(ev, priority, -upload_day)
  vids <- unique(ev$video_id)
  say("Comments for ", length(vids), " sponsored videos (",
      uniqueN(ev[priority == FALSE, video_id]), " rare and isolated first). Quota used today: ", quota_used_today())
  done <- with_quota({
    for (i in seq_along(vids)) {
      e <- ev[video_id == vids[i]]
      terms <- c(setNames(e$brand, paste0("brand:", e$brand)),
                 if (!is.na(e$promo_code[1]) && nchar(e$promo_code[1]) >= 3) c(code = e$promo_code[1]))
      count_comment_mentions(vids[i], terms)
      if (i %% 50 == 0) say("  ", i, "/", length(vids), " videos; quota used today ", quota_used_today())
    }
    TRUE
  })
  out <- rbindlist(lapply(seq_len(nrow(ev)), function(i) {
    f <- file.path(CACHE_DIR, "comments", paste0(ev$video_id[i], ".rds"))
    if (!file.exists(f)) return(data.table(video_id = ev$video_id[i], brand = ev$brand[i],
                                           n_comments_fetched = NA_integer_, n_mentions = NA_integer_,
                                           comments_status = "not_fetched"))
    r <- readRDS(f)
    cols <- intersect(c(paste0("brand:", ev$brand[i]), "code"), colnames(r$m))
    hit <- if (length(cols) && nrow(r$m)) rowSums(r$m[, cols, drop = FALSE]) > 0 else logical(0)
    data.table(video_id = ev$video_id[i], brand = ev$brand[i], n_comments_fetched = r$n,
               n_mentions = if (r$status == "ok") sum(hit) else NA_integer_, comments_status = r$status)
  }))
  out[, share_mentions := fifelse(n_comments_fetched > 0, n_mentions / n_comments_fetched, NA_real_)]
  setcolorder(out, c("video_id", "brand", "n_comments_fetched", "n_mentions", "share_mentions"))
  fwrite(out, dpath("comment_mentions.csv"))
  say("Wrote data/comment_mentions.csv: ", nrow(out), " rows, ", out[comments_status != "not_fetched", .N],
      " fetched. Quota used today: ", quota_used_today())
  if (isFALSE(done)) say("Quota ran out; run this stage again tomorrow to fetch the rest.")
}

# ---- Stage: summary ---------------------------------------------------------
stage_summary <- function() {
  f <- function(x) if (file.exists(dpath(x))) read_csv(dpath(x)) else NULL
  say("Data folder: ", DATA_DIR)
  if (file.exists(CHANNELS_FILE)) say("channels.csv: ", nrow(read_csv(CHANNELS_FILE)), " channels")
  for (x in c("videos.csv", "videos_labelled.csv", "description_links.csv", "sponsor_events.csv",
              "brand_event_panel.csv", "comment_mentions.csv", "views_panel.csv", "channel_panel.csv")) {
    d <- f(x); if (!is.null(d)) say(sprintf("  %-24s %8d rows", x, nrow(d)))
  }
  lab <- f("videos_labelled.csv")
  if (!is.null(lab)) print(lab[, .N, by = label_source][order(-N)])
  ev <- f("sponsor_events.csv")
  if (!is.null(ev)) {
    say("Events: ", nrow(ev), "; brands: ", uniqueN(ev$brand),
        "; rare_brand & n_overlapping == 0: ", ev[rare_brand == TRUE & n_overlapping == 0, .N])
    key <- ev[rare_brand == TRUE & n_overlapping == 0, event_id]
    p <- f("brand_event_panel.csv"); cm <- f("comment_mentions.csv")
    if (!is.null(p) && length(key)) {
      s <- p[event_id %in% key, .(wiki = any(!is.na(wiki_views)), web = any(!is.na(trends_web)),
                                  yt = any(!is.na(trends_youtube))), by = event_id]
      say("  of those, with any wiki_views: ", s[, sum(wiki)], ", trends_web: ", s[, sum(web)],
          ", trends_youtube: ", s[, sum(yt)])
    }
    if (!is.null(cm) && length(key)) {
      k <- merge(ev[event_id %in% key, .(video_id, brand)], cm, by = c("video_id", "brand"))
      say("  of those, with comment counts: ", k[!is.na(n_mentions), .N])
    }
  }
  q <- if (file.exists(dpath(QUOTA_LOG))) read_csv(dpath(QUOTA_LOG), colClasses = c(quota_day = "character")) else NULL
  if (!is.null(q)) print(q[, .(units = sum(units)), by = .(quota_day, script)])
}

stages <- list(check = stage_check, candidates = stage_candidates, channels = stage_channels,
               videos = stage_videos, label = stage_label, handcheck = stage_handcheck,
               brand_candidates = stage_brand_candidates, events = stage_events, panel = stage_panel,
               comments = stage_comments, summary = stage_summary)
# SPONSOR_NO_RUN=TRUE loads the stages without running one (used by the test).
if (identical(Sys.getenv("SPONSOR_NO_RUN"), "TRUE")) STAGE <- "none"
if (STAGE != "none") {
  if (!STAGE %in% names(stages)) stop("Unknown stage '", STAGE, "'. Use one of: ", paste(names(stages), collapse = ", "))
  invisible(tryCatch(stages[[STAGE]](), error = function(e) {
    message("Error: ", scrub(conditionMessage(e))); quit(status = 1)
  }))
}
