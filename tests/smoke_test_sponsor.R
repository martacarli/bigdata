# tests/smoke_test_sponsor.R
# Runs the sponsor pipeline end to end on fake API responses in a temp
# folder and checks the outputs. No network and no API key needed:
#   Rscript tests/smoke_test_sponsor.R

suppressPackageStartupMessages(library(data.table))
proj <- normalizePath(".")
tmp <- tempfile("sponsor_test_"); dir.create(tmp)
for (f in c("sponsor_utils.R", "sponsor_pipeline.R", "poll_views.R")) file.copy(file.path(proj, f), tmp)
old <- setwd(tmp)
Sys.setenv(SPONSOR_NO_RUN = "TRUE", YT_API_KEY = "FAKEKEY_abcdefghijklmnopqrstuvwxyz")
source("sponsor_pipeline.R")
source("poll_views.R")

check <- function(ok, what) { if (!isTRUE(ok)) stop("FAILED: ", what, call. = FALSE); cat("ok  ", what, "\n") }

# ---- Fake world --------------------------------------------------------------
fwrite(data.table(channel_id = c("UCaaa", "UCbbb"), title = c("Chan A", "Chan B"),
                  handle = c("@a", "@b"), subscriber_count = c(2e5, 9e5), category = c("tech", "gaming")),
       CHANNELS_FILE)
day <- function(n) format(Sys.time() - n * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
vids <- data.table(
  id = c("v1", "v2", "v3", "v4", "v5", "v6"),
  ch = c("UCaaa", "UCaaa", "UCaaa", "UCbbb", "UCbbb", "UCbbb"),
  pub = c(day(100), day(70), day(5), day(90), day(40), day(30)),
  dur = c("PT10M", "PT45S", "PT12M", "PT20M5S", "PT8M", "PT1H"),
  desc = c("Thanks to NordVPN for sponsoring! Get it at https://nordvpn.com/chana use code CHANA\nhttps://twitter.com/a",
           "short", "new video https://nordvpn.com/x sponsored by NordVPN",
           "Today’s sponsor: https://www.squarespace.com/b. My merch https://chanb.shop",
           "no ads here https://patreon.com/b https://chanb.shop",
           "Check out https://ridge.com/B (paid promotion)"))
fake_yt <- function(endpoint, params, raw_subdir = NULL, raw_name = NULL, use_raw = TRUE) {
  log_quota(endpoint, 1L, SCRIPT_NAME)
  if (endpoint == "playlistItems") {
    d <- vids[ch == sub("^UU", "UC", params$playlistId)]
    return(list(items = lapply(seq_len(nrow(d)), function(i)
      list(contentDetails = list(videoId = d$id[i], videoPublishedAt = d$pub[i])))))
  }
  if (endpoint == "videos") {
    d <- vids[id %in% strsplit(params$id, ",")[[1]]]
    return(list(items = lapply(seq_len(nrow(d)), function(i) list(
      id = d$id[i], snippet = list(channelId = d$ch[i], channelTitle = d$ch[i], title = paste("Title", d$id[i]),
                                   description = d$desc[i], publishedAt = d$pub[i], categoryId = "28",
                                   defaultAudioLanguage = "en", liveBroadcastContent = "none"),
      contentDetails = list(duration = d$dur[i]),
      statistics = list(viewCount = "1000000", likeCount = "50", commentCount = "10")))))
  }
  if (endpoint == "channels") {
    ids <- strsplit(params$id, ",")[[1]]
    return(list(items = lapply(ids, function(x) list(id = x, snippet = list(title = x),
      statistics = list(subscriberCount = "200000", viewCount = "5000000", videoCount = "300"),
      status = list(madeForKids = FALSE)))))
  }
  if (endpoint == "commentThreads") {
    txt <- c("Great video", "I already use NordVPN", "code chana worked!", "nordvpnx is not a match", "lol")
    return(list(items = lapply(txt, function(t) list(snippet = list(topLevelComment = list(snippet = list(textDisplay = t)))))))
  }
}
yt_get <- fake_yt
sb_fake <- list(
  v1 = list(list(segment = list(30, 90), actionType = "skip", category = "sponsor"),
            list(segment = list(35, 92), actionType = "skip", category = "sponsor")),
  v4 = list(list(segment = list(1000, 1060), actionType = "skip", category = "sponsor")),
  v5 = list(), v6 = list())
sb_segments <- function(video_id) {
  f <- file.path(CACHE_DIR, "sponsorblock", paste0(video_id, ".json"))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(sb_fake[[video_id]] %||% list(), auto_unbox = TRUE), f)
}
wiki_views <- function(article, from, to) data.table(date = as.IDate(seq(from, to, by = 1)), wiki_views = 100)
trends_available <- function() TRUE
trends_fetch <- function(event_id, term, from, to, gprop) {
  f <- file.path(CACHE_DIR, "trends", sprintf("%s_%s_%s.rds", event_id, gprop, TRENDS_GEO))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(data = data.table(date = as.IDate(seq(from, to, by = 1)), hits = 50)), f); "ok"
}

# ---- Run the stages ------------------------------------------------------------
stage_videos()
v <- fread("data/videos.csv")
check(nrow(v) == 5 && !"v2" %in% v$video_id, "videos.csv drops the 45-second Short")
check(v[video_id == "v4", duration_sec] == 1205, "duration parsed")

stage_label()
lab <- fread("data/videos_labelled.csv")
check(!"v3" %in% lab$video_id && nrow(lab) == 4, "only videos at least 14 days old are labelled")
check(lab[video_id == "v1", label_source] == "both", "v1 labelled both")
check(lab[video_id == "v1", sb_ad_seconds] == 62 && lab[video_id == "v1", sb_n_segments] == 1, "overlapping segments merged")
check(lab[video_id == "v1", sb_ad_position] == "early", "v1 ad early")
check(lab[video_id == "v4", label_source] == "both" && lab[video_id == "v4", sb_ad_position] == "late", "curly apostrophe matched, late ad")
check(lab[video_id == "v5", label_source] == "neither", "v5 neither")
check(lab[video_id == "v6", label_source] == "description_only", "v6 description only")
check(lab[video_id == "v1", promo_code] == "CHANA", "promo code extracted")
links <- fread("data/description_links.csv")
check(!any(links$domain %in% c("twitter.com", "patreon.com")), "social and Patreon links excluded")
check("squarespace.com" %in% links$domain, "www. stripped from domains")

stage_handcheck()
hc <- fread("data/handcheck_unsponsored.csv")
check(nrow(hc) == 1 && hc$video_id == "v5" && "ad_found" %in% names(hc), "handcheck sample")

stage_brand_candidates()
bc <- fread("data/brand_candidates.csv")
check(bc[domain == "chanb.shop", n_videos_unsponsored] == 1, "brand candidates flag links also in unsponsored videos")

fwrite(data.table(domain = c("nordvpn.com", "squarespace.com", "ridge.com"), brand = c("NordVPN", "Squarespace", "Ridge"),
                  wiki_article = c("NordVPN", "Squarespace", ""), trends_term = c("NordVPN", "Squarespace", "Ridge wallet"),
                  youtube_channel_id = "", unsure = c("FALSE", "FALSE", "TRUE")), BRANDS_FILE)
stage_events()
ev <- fread("data/sponsor_events.csv")
check(nrow(ev) == 3 && all(ev$rare_brand), "three events, all rare")
check(all(ev$n_overlapping == 0), "no overlaps across brands")

stage_panel()
p <- fread("data/brand_event_panel.csv")
check(nrow(p) == 3 * 57 && all(range(p$rel_day) == c(-28, 28)), "panel has 57 days per event")
check(p[brand == "Ridge", all(is.na(wiki_views))] && p[brand == "NordVPN" & rel_day == 0, wiki_views] == 100, "wiki views joined")
check(p[brand == "NordVPN" & rel_day == 0, trends_web] == 50, "trends joined")

stage_comments()
cm <- fread("data/comment_mentions.csv")
check(cm[brand == "NordVPN", n_mentions] == 2 && cm[brand == "NordVPN", n_comments_fetched] == 5,
      "brand and code mentions counted, word boundaries respected")
check(!dir.exists(file.path(RAW_DIR, "comments_unused")) && !length(list.files(RAW_DIR, "comment", recursive = TRUE)),
      "no comment text saved")

main()   # poll_views.R
vp <- fread("data/views_panel.csv"); cp <- fread("data/channel_panel.csv")
check(nrow(vp) == 3 && nrow(cp) == 2, "poll keeps videos from the last 60 days, no Shorts")
main()
vp2 <- fread("data/views_panel.csv")
check(nrow(vp2) == 3 && all(vp2$views == 1e6) && !any(grepl("e\\+", readLines("data/views_panel.csv"))),
      "second poll on the same day replaces rows, numbers stay plain")

# Quota guard: a call over the limit must stop cleanly.
yt_get <- function(...) stop(quota_exceeded())
unlink(file.path(CACHE_DIR, "comments"), recursive = TRUE)
stage_comments()
check(all(fread("data/comment_mentions.csv")$comments_status == "not_fetched"), "quota stop leaves rows marked not_fetched")
check(scrub("x key=FAKEKEY_abcdefghijklmnopqrstuvwxyz y") == "x key=<KEY> y", "key scrubbed from messages")

setwd(old)
cat("\nAll checks passed.\n")
