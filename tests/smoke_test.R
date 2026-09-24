# tests/smoke_test.R
# Runs 01 (offline), 02 and 03 on small synthetic files in a temp folder and
# checks the numbers. No network needed. Run from the project folder:
#   Rscript tests/smoke_test.R
# The fixtures mimic the awkward parts of the real data: descriptions with
# newlines, quotes and commas, a zipped release, duplicate SponsorBlock
# submissions, hidden and downvoted segments, and whole-video labels.

suppressPackageStartupMessages(library(data.table))
tmp <- tempfile("ytsb_test_"); dir.create(file.path(tmp, "data", "raw"), recursive = TRUE)
raw <- file.path(tmp, "data", "raw")

# ---- Trending fixture ----------------------------------------------------------
tr <- data.table(
  snapshot_time     = c("2023-01-01T00:00:00Z", "2023-01-01T06:00:00Z", "2023-01-02T00:00:00Z",
                        "2023-01-02T00:00:00Z", "2023-01-03T00:00:00Z", "2023-01-01T00:00:00Z"),
  region_code       = c("US", "US", "US", "US", "US", "GB"),
  rank              = c(5, 2, 10, 1, 50, 3),
  video_id          = c("AAAAAAAAAAA", "AAAAAAAAAAA", "BBBBBBBBBBB", "CCCCCCCCCCC", "DDDDDDDDDDD", "EEEEEEEEEEE"),
  video_title       = c("Video A, \"quoted\"", "Video A renamed", "Video B", "Video C", "Video D", "Video E"),
  video_description = c("line one\nline two, with comma\n\"quote\" here", "x", "",
                        "multi\r\nline\r\nUS", "d", "mentions US and\nnewline"),
  channel_title     = c("Chan A", "Chan A", "Chan B", "Chan C", "Chan D", "Chan E"),
  channel_id        = paste0("UC", 1:6),
  video_category_id = c("28", "28", "24", "20", "10", "22"),
  video_tags        = c("a,b,\"c\"", "", "t", "t", "t", "t"),
  video_view_count  = c(100, 500, 20, 7, 9, 1)
)
# Filler rows from other countries, many with multi-line text, to cross chunk edges.
set.seed(1)
filler <- tr[rep(6, 300)][, `:=`(region_code = sample(c("GB", "FR", "DE"), .N, TRUE),
                                 video_id = sprintf("F%010d", .I),
                                 video_description = paste0("filler ", .I, "\nsecond line, \"q\""))]
tr <- rbind(filler[1:150], tr, filler[151:300])
csv <- file.path(tmp, "most_popular.csv")
fwrite(tr, csv, quote = TRUE)
other <- file.path(tmp, "README.txt"); writeLines("readme", other)
old <- setwd(tmp); zip(file.path(raw, "trending_release.zip"), c("most_popular.csv", "README.txt"), flags = "-q"); setwd(old)

# ---- SponsorBlock fixtures (real column order) --------------------------------------
sb_cols <- c("videoID", "startTime", "endTime", "votes", "locked", "incorrectVotes", "UUID",
             "userID", "timeSubmitted", "views", "category", "actionType", "service",
             "videoDuration", "hidden", "reputation", "shadowHidden", "hashedVideoID",
             "userAgent", "description")
mk <- function(v, s, e, votes = 1, cat = "sponsor", act = "skip", hidden = 0, sh = 0) {
  data.table(videoID = v, startTime = s, endTime = e, votes = votes, locked = 0,
             incorrectVotes = 1, UUID = "u", userID = "x", timeSubmitted = 1,
             views = 3, category = cat, actionType = act, service = "YouTube",
             videoDuration = 600, hidden = hidden, reputation = 0, shadowHidden = sh,
             hashedVideoID = "h", userAgent = "ua, with \"comma\"", description = "")
}
sb <- rbind(
  mk("AAAAAAAAAAA", 10, 40), mk("AAAAAAAAAAA", 12, 41), mk("AAAAAAAAAAA", 11, 39),  # 1 merged: 10-41
  mk("AAAAAAAAAAA", 300, 330),                                                        # 2nd segment
  mk("AAAAAAAAAAA", 500, 520, votes = -1),                                            # downvoted
  mk("BBBBBBBBBBB", 5, 50, hidden = 1), mk("BBBBBBBBBBB", 5, 50, sh = 1),              # hidden
  mk("BBBBBBBBBBB", 0, 20, cat = "selfpromo"),                                        # B is in SB
  mk("CCCCCCCCCCC", 0, 0, act = "full"),                                              # whole video
  mk("EEEEEEEEEEE", 1, 30)                                                            # GB only
)
setcolorder(sb, sb_cols)
fwrite(sb, file.path(raw, "sponsorTimes.csv"))
fwrite(data.table(videoID = c("AAAAAAAAAAA", "CCCCCCCCCCC"), channelID = "UCx",
                  title = c("Video A", "Video C"), published = 1),
       file.path(raw, "videoInfo.csv"))

md5_before <- tools::md5sum(list.files(raw, full.names = TRUE))

# ---- Run the pipeline ---------------------------------------------------------------
env <- c(sprintf("YTSB_DATA_DIR=%s", file.path(tmp, "data")),
         sprintf("YTSB_OUTPUT_DIR=%s", file.path(tmp, "output")),
         "YTSB_OFFLINE=TRUE", "YTSB_CHUNK_LINES=7")
for (s in c("01_download.R", "02_clean.R", "03_match.R")) {
  cat("\n---", s, "---\n")
  rc <- system2("Rscript", s, env = env)
  if (rc != 0) stop(s, " failed")
}

# ---- Checks -----------------------------------------------------------------------------
out <- file.path(tmp, "output")
v   <- readRDS(file.path(tmp, "data", "interim", "us_trending_videos.rds"))
mt  <- fread(file.path(out, "us_trending_sponsor_matches.csv"))
rep <- readLines(file.path(out, "report.txt"))
check <- function(ok, what) { cat(if (ok) "PASS" else "FAIL", what, "\n"); if (!ok) quit(status = 1) }
check(nrow(v) == 4, "4 unique US videos (GB and filler rows dropped)")
check(v[video_id == "AAAAAAAAAAA", best_rank == 2 && n_snapshots == 2 && views_at_entry == 100],
      "A: best rank 2, 2 snapshots, views at entry 100")
check(v[video_id == "AAAAAAAAAAA", grepl("line two", description) && title == "Video A, \"quoted\""],
      "multi-line description and quoted title survive streaming")
check(v[video_id == "CCCCCCCCCCC", category_name] == "Gaming", "category id mapped to name")
check(setequal(mt$video_id, c("AAAAAAAAAAA", "CCCCCCCCCCC")), "matched = A and C only")
check(mt[video_id == "AAAAAAAAAAA", n_sponsor_segments == 2 && sponsor_seconds == 61 &&
           first_sponsor_start == 10], "A: overlapping submissions merged (2 segs, 61 s, first at 10)")
check(mt[video_id == "CCCCCCCCCCC", full_video_label], "C: whole-video label counts")
check(any(grepl("present in SponsorBlock at all: +3 ", rep)), "coverage: 3 of 4 videos in SponsorBlock")
check(identical(md5_before, tools::md5sum(list.files(raw, full.names = TRUE))),
      "raw files unchanged (same names and checksums)")
cat("\nAll checks passed. Output in", out, "\n")
