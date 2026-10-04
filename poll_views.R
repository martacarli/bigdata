# poll_views.R
# Run once a day (see README for the cron line). For every panel channel it
#   1. reads the first page(s) of the uploads playlist to catch new videos,
#   2. records views, likes and comments of every video uploaded in the last
#      POLL_WINDOW days            -> data/views_panel.csv   (video x day)
#   3. records subscriber_count and total view_count of each channel
#                                  -> data/channel_panel.csv (channel x day)
# Cost: about 1 unit per channel + 1 per 50 videos + 1 per 50 channels,
# roughly 300 units a day for 250 channels. Raw responses are saved under
# data/raw/poll/<date>/. Running it twice on one day replaces that day's rows.
# Note: the API rounds subscriber counts to 3 significant figures, so
# channel_panel.csv subscriber_count moves in steps (e.g. 1,230,000).

source("sponsor_utils.R")
SCRIPT_NAME <- "poll_views"
set.seed(SEED)

say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S "), ...)

# Fresh uploads list for one channel, saved under data/raw/poll/<date>/.
recent_uploads <- function(channel_id, since, sub) {
  out <- list(); token <- NULL
  for (p in 1:5) {
    params <- list(part = "contentDetails", playlistId = uploads_playlist(channel_id), maxResults = 50)
    if (!is.null(token)) params$pageToken <- token
    r <- yt_get("playlistItems", params, file.path(sub, "playlistItems"), sprintf("%s_p%d", channel_id, p))
    if (isTRUE(r$.error) || !length(r$items)) break
    d <- data.table(video_id = vapply(r$items, function(i) i$contentDetails$videoId, ""),
                    published_at = parse_iso(vapply(r$items, function(i) chr(i$contentDetails$videoPublishedAt), "")))
    out[[p]] <- d
    token <- r$nextPageToken
    if (is.null(token) || all(is.na(d$published_at) | as.Date(d$published_at) < since)) break
  }
  d <- rbindlist(out)
  if (!nrow(d)) return(NULL)
  d[!is.na(published_at) & as.Date(published_at) >= since][, channel_id := channel_id][]
}

# Replace today's rows in a panel file, keep all other days. Old rows are
# read as text and written back unchanged, then today's rows are appended.
append_day <- function(new, file, day) {
  if (file.exists(file)) {
    old <- fread(file, encoding = "UTF-8", colClasses = "character")
    if (!identical(sort(names(old)), sort(names(new)))) stop(file, " has different columns; not appending.")
    fwrite(old[poll_date != as.character(day)], file)
    fwrite(new[, names(old), with = FALSE], file, append = TRUE)
  } else fwrite(new, file)
}

main <- function() {
  yt_key()
  if (!file.exists(CHANNELS_FILE)) stop("Missing ", CHANNELS_FILE, ". Run sponsor_pipeline.R channels first.")
  ch <- fread(CHANNELS_FILE, encoding = "UTF-8")
  day <- format(Sys.Date(), "%Y-%m-%d")
  since <- Sys.Date() - POLL_WINDOW
  sub <- file.path("poll", day)
  say("Polling ", nrow(ch), " channels; quota used today before: ", quota_used_today())

  up <- rbindlist(lapply(ch$channel_id, recent_uploads, since = since, sub = sub))
  say("Videos uploaded in the last ", POLL_WINDOW, " days: ", nrow(up))

  v <- video_details(up$video_id, raw_subdir = file.path(sub, "videos"))
  if (nrow(v)) {
    v <- v[!is.na(duration_sec) & duration_sec > MIN_DURATION & live == "none"]
    vp <- v[, .(poll_date = day, video_id, channel_id, published_at,
                age_days = round(as.numeric(difftime(parse_iso(retrieved_at), parse_iso(published_at), units = "days")), 2),
                views, likes, comments, retrieved_at)]
    append_day(vp, dpath("views_panel.csv"), day)
    say("views_panel.csv: ", nrow(vp), " videos recorded for ", day)
  }

  c <- channels_by_id(ch$channel_id, raw_subdir = file.path(sub, "channels"))
  cp <- c[, .(poll_date = day, channel_id, subscriber_count, view_count, video_count,
              retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))]
  append_day(cp, dpath("channel_panel.csv"), day)
  say("channel_panel.csv: ", nrow(cp), " channels recorded for ", day,
      ". Quota used today: ", quota_used_today())
}

if (!identical(Sys.getenv("SPONSOR_NO_RUN"), "TRUE")) tryCatch(main(),
         quota_exceeded = function(e) { message(conditionMessage(e)); quit(status = 2) },
         error = function(e) { message("Error: ", scrub(conditionMessage(e))); quit(status = 1) })
