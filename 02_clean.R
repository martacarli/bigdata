# 02_clean.R
# Step 2: build the two tables we join in step 3.
#   a) One row per unique US trending video.
#   b) One row per video in SponsorBlock with its sponsor segments summarised.
#
# Inputs : data/interim/us_trending_rows.rds (from 01), data/raw/sponsorTimes.csv,
#          data/raw/videoInfo.csv
# Outputs: data/interim/us_trending_videos.rds
#          data/interim/sb_sponsor_per_video.rds
#          data/interim/sb_videos_any.rds   (ids with any visible SponsorBlock segment)
#          data/interim/sb_video_info.rds

source("00_config.R")

# ---- a) Unique US trending videos -------------------------------------------
rows <- readRDS(file.path(INTERIM_DIR, "us_trending_rows.rds"))
setDT(rows)
for (opt in c("description", "channel_id")) if (!opt %in% names(rows)) rows[, (opt) := NA_character_]

rows[, `:=`(
  video_id      = trimws(video_id),
  snapshot_time = parse_time(snapshot_time),
  rank          = as.integer(rank),
  views         = as.numeric(views)
)]
rows <- rows[!is.na(video_id) & nzchar(video_id)]
n_bad_time <- rows[is.na(snapshot_time), .N]
if (n_bad_time) warning(n_bad_time, " rows have an unreadable snapshot time.")

# Order so that the first row per video is its first appearance on the list.
setorder(rows, video_id, snapshot_time, rank)
videos <- rows[, .(
  first_trending = min(snapshot_time, na.rm = TRUE),
  last_trending  = max(snapshot_time, na.rm = TRUE),
  best_rank      = min(rank, na.rm = TRUE),   # highest position reached (1 = top)
  n_snapshots    = .N,
  views_at_entry = views[1],
  title          = title[1],                  # as shown when it first trended
  description    = description[1],
  channel_title  = channel_title[1],
  channel_id     = channel_id[1],
  category       = category[1]
), by = video_id]

# Category ids -> names (leave names alone if the dataset already has them).
videos[, category_name := fifelse(category %chin% names(YT_CATEGORIES),
                                  unname(YT_CATEGORIES[category]), category)]
videos[is.na(category_name), category_name := "Unknown"]

message(sprintf("US snapshot rows: %s | unique US trending videos: %s",
                format(nrow(rows), big.mark = ","), format(nrow(videos), big.mark = ",")))
saveRDS(videos, file.path(INTERIM_DIR, "us_trending_videos.rds"))
rm(rows); invisible(gc())

# ---- b) SponsorBlock segments ---------------------------------------------------
sb_path <- file.path(RAW_DIR, "sponsorTimes.csv")
sb_header <- names(fread(sb_path, nrows = 0))
wanted <- c("videoID", "startTime", "endTime", "votes", "category", "actionType",
            "service", "hidden", "shadowHidden")
needed <- c("videoID", "startTime", "endTime", "votes", "category")
if (length(setdiff(needed, sb_header))) {
  stop("sponsorTimes.csv lacks: ", paste(setdiff(needed, sb_header), collapse = ", "))
}
sel <- intersect(wanted, sb_header)
message("Reading ", sb_path, " (columns: ", paste(sel, collapse = ", "), ")")
sb <- fread(sb_path, select = sel, showProgress = TRUE,
            colClasses = list(character = intersect(c("videoID", "category", "actionType", "service"), sel),
                              numeric   = intersect(c("startTime", "endTime", "votes",
                                                      "hidden", "shadowHidden"), sel)))
n_raw <- nrow(sb)

# Keep what the SponsorBlock extension itself would show: YouTube only,
# not hidden, not shadow-hidden, and not voted down.
if ("service" %in% sel)      sb <- sb[is.na(service) | service == "YouTube"]
if ("hidden" %in% sel)       sb <- sb[is.na(hidden) | hidden == 0]
if ("shadowHidden" %in% sel) sb <- sb[is.na(shadowHidden) | shadowHidden == 0]
sb <- sb[!is.na(votes) & votes >= 0]
sb[, videoID := trimws(videoID)]

# Videos SponsorBlock users have looked at at all (any category). This is the
# coverage denominator: a trending video missing here tells us nothing.
saveRDS(unique(sb$videoID), file.path(INTERIM_DIR, "sb_videos_any.rds"))

sb <- sb[category == "sponsor"]
message(sprintf("SponsorBlock rows: %s raw, %s visible sponsor rows",
                format(n_raw, big.mark = ","), format(nrow(sb), big.mark = ",")))

# "full" = the whole video is labelled sponsored (no start/end times).
if (!"actionType" %in% sel) sb[, actionType := "skip"]
full_video <- unique(sb[actionType == "full", .(videoID)])[, full_video_label := TRUE]

# Several users often submit the same sponsor read, so raw rows overcount.
# Merge overlapping intervals per video, then count and sum the merged ones.
seg <- sb[actionType != "full" & endTime > startTime & endTime < 1e6,
          .(videoID, startTime, endTime)]
setorder(seg, videoID, startTime, endTime)
# Offset each video onto its own stretch of the number line so one
# vectorised cummax works across all videos without a slow group-by.
off  <- rleid(seg$videoID) * 1e6
s    <- seg$startTime + off
runE <- cummax(seg$endTime + off)
seg[, interval := cumsum(c(TRUE, s[-1] > runE[-length(runE)]))]
merged <- seg[, .(start = min(startTime), end = max(endTime), n_submissions = .N),
              by = .(videoID, interval)]
sponsor <- merged[, .(
  n_sponsor_segments  = .N,
  sponsor_seconds     = sum(end - start),
  first_sponsor_start = min(start),
  n_sb_submissions    = sum(n_submissions)
), by = videoID]
sponsor <- merge(sponsor, full_video, by = "videoID", all = TRUE)
sponsor[is.na(full_video_label), full_video_label := FALSE]
sponsor[is.na(n_sponsor_segments), `:=`(n_sponsor_segments = 0L, sponsor_seconds = 0,
                                        n_sb_submissions = 0L)]
message(sprintf("Videos with visible sponsor data: %s", format(nrow(sponsor), big.mark = ",")))
saveRDS(sponsor, file.path(INTERIM_DIR, "sb_sponsor_per_video.rds"))
rm(sb, seg, merged); invisible(gc())

# ---- c) SponsorBlock video info (only to sanity-check the join) ----------------
vi_path <- file.path(RAW_DIR, "videoInfo.csv")
vi_cols <- intersect(c("videoID", "channelID", "title", "published"), names(fread(vi_path, nrows = 0)))
vinfo <- fread(vi_path, select = vi_cols, colClasses = "character", showProgress = TRUE)
vinfo <- undouble_quotes(unique(vinfo[videoID %chin% videos$video_id], by = "videoID"))
setnames(vinfo, setdiff(vi_cols, "videoID"), paste0("sb_", setdiff(vi_cols, "videoID")))
saveRDS(vinfo, file.path(INTERIM_DIR, "sb_video_info.rds"))

message("Done. Next: Rscript 03_match.R")
