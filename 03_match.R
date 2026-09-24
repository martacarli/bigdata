# 03_match.R
# Step 3: join US trending videos to SponsorBlock sponsor data and report.
#
# Inputs : the four .rds tables written by 02_clean.R
# Outputs: output/us_trending_sponsor_matches.csv  (matched subset, small)
#          output/sponsor_share_by_category.csv
#          output/report.txt                       (headline numbers + 10 examples)

source("00_config.R")

videos  <- readRDS(file.path(INTERIM_DIR, "us_trending_videos.rds"))
sponsor <- readRDS(file.path(INTERIM_DIR, "sb_sponsor_per_video.rds"))
sb_any  <- readRDS(file.path(INTERIM_DIR, "sb_videos_any.rds"))
vinfo   <- readRDS(file.path(INTERIM_DIR, "sb_video_info.rds"))

# Left join: keep every trending video, add sponsor data where it exists.
m <- merge(videos, sponsor, by.x = "video_id", by.y = "videoID", all.x = TRUE)
m[, in_sponsorblock := video_id %chin% sb_any]
m[, has_sponsor := !is.na(n_sponsor_segments) &
      (n_sponsor_segments > 0 | full_video_label %in% TRUE)]

# ---- Headline numbers ---------------------------------------------------------
n_videos  <- nrow(m)
n_in_sb   <- m[in_sponsorblock == TRUE, .N]
n_sponsor <- m[has_sponsor == TRUE, .N]
pct <- function(a, b) if (b > 0) sprintf("%.1f%%", 100 * a / b) else "n/a"

# ---- Share by YouTube category ----------------------------------------------------
by_cat <- m[, .(
  videos            = .N,
  in_sponsorblock   = sum(in_sponsorblock),
  with_sponsor      = sum(has_sponsor),
  share_sponsor     = round(sum(has_sponsor) / .N, 4),
  share_of_covered  = round(fifelse(sum(in_sponsorblock) > 0,
                                    sum(has_sponsor) / sum(in_sponsorblock), NA_real_), 4)
), by = .(category = category_name)][order(-videos)]
fwrite(by_cat, file.path(OUTPUT_DIR, "sponsor_share_by_category.csv"))

# ---- Matched subset ---------------------------------------------------------------
matched <- m[has_sponsor == TRUE]
matched <- merge(matched, vinfo, by.x = "video_id", by.y = "videoID", all.x = TRUE)
# Descriptions are long; trim them so the CSV stays small and opens in Excel.
matched[, description := substr(gsub("[\r\n]+", " ", description), 1, 300)]
setcolorder(matched, c("video_id", "title", "channel_title", "category_name",
                       "first_trending", "last_trending", "best_rank", "n_snapshots",
                       "views_at_entry", "n_sponsor_segments", "sponsor_seconds",
                       "first_sponsor_start", "full_video_label"))
matched[, in_sponsorblock := NULL]
matched[, has_sponsor := NULL]
setorder(matched, first_trending)
fwrite(matched, file.path(OUTPUT_DIR, "us_trending_sponsor_matches.csv"),
       dateTimeAs = "write.csv")

# ---- 10 example titles (random, reproducible) ---------------------------------------
set.seed(SEED)
ex <- matched[sample(.N, min(10L, .N)),
              .(video_id, title, channel_title, category_name, n_sponsor_segments, sponsor_seconds)]

# ---- Report ----------------------------------------------------------------------------
report <- c(
  "US YouTube trending videos with SponsorBlock sponsor segments",
  paste("Run on", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  sprintf("Unique US trending videos:                 %s", format(n_videos, big.mark = ",")),
  sprintf("  ...present in SponsorBlock at all:       %s (%s)", format(n_in_sb, big.mark = ","), pct(n_in_sb, n_videos)),
  sprintf("  ...with >= 1 visible sponsor segment:    %s (%s of all, %s of those in SponsorBlock)",
          format(n_sponsor, big.mark = ","), pct(n_sponsor, n_videos), pct(n_sponsor, n_in_sb)),
  sprintf("  ...of which whole-video sponsor labels:  %s", m[has_sponsor & full_video_label %in% TRUE, .N]),
  sprintf("Median sponsor seconds (matched, timed):   %s",
          if (nrow(matched[n_sponsor_segments > 0])) median(matched[n_sponsor_segments > 0, sponsor_seconds]) else "n/a"),
  "",
  "Share by category (share_sponsor = with_sponsor / videos):",
  capture.output(print(by_cat, row.names = FALSE)),
  "",
  sprintf("10 example titles (set.seed(%d)):", SEED),
  sprintf("  %2d. %s  [%s, %s]", seq_len(nrow(ex)), ex$title, ex$channel_title, ex$category_name)
)
writeLines(report, file.path(OUTPUT_DIR, "report.txt"))
cat(report, sep = "\n")
