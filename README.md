# How many US trending videos have sponsor segments?

This is a feasibility check for the Big Data for Business Decisions project (20564). It takes every video that was on the US YouTube Trending list between July 2022 and June 2025 and checks how many of them have a sponsor segment in the SponsorBlock database.

## Data

1. **Global YouTube Trending Dataset (2022-2025)**, Ng and Goncalves, Illinois Data Bank, <https://databank.illinois.edu/datasets/IDB-9307654>. There are four snapshots a day for 104 countries, with up to 200 videos per snapshot. We only need `most_popular.csv`, and from that file only the rows where `region_code == "US"`.
2. **SponsorBlock database dump** from the mirror at <https://sb.ltn.fi/database/>. We use `sponsorTimes.csv` (about 5.7 GB) and `videoInfo.csv`.

## Packages

R 4.1 or newer, plus:

```r
install.packages(c("data.table", "curl", "jsonlite"))
```

You also need the command line tool `unzip`, which macOS and Linux already have. If the release comes as `.7z` you need 7-Zip installed as well. On Windows the scripts fall back to R's own `unz()` for zip files, and that should work too.

## Run order

Open a terminal in this folder and run:

```
Rscript 01_download.R   # list files, download, stream out the US rows
Rscript 02_clean.R      # one row per trending video + sponsor summary per video
Rscript 03_match.R      # join, report, write the CSVs
```

`00_config.R` has no steps of its own. Each script loads it, and it holds the paths, limits, column names and helper functions. You can also run the scripts from RStudio as long as the working directory is this folder.

### What each step does

**01_download.R** first reads the list of files in the Illinois release, together with their sizes, and prints it. It then downloads only the file that contains `most_popular.csv`. When that file is an archive, the script reads the CSV straight out of it without unpacking anything to disk, keeps the US rows and only the columns we need, and saves them to `data/interim/us_trending_rows.rds`. After that it downloads the two SponsorBlock files. Every download resumes if it gets interrupted, is skipped if the file is already there, and is made read-only once it finishes.

**If a file is bigger than 30 GB the script stops before downloading it and tells you the size.** To go ahead anyway, run it again with:

```
YTSB_ALLOW_LARGE=TRUE Rscript 01_download.R
```

(on Windows: `set YTSB_ALLOW_LARGE=TRUE` first). You also have to do this if the server doesn't report a size at all.

**02_clean.R** builds two tables:

- *US trending videos*, with one row per video: `first_trending`, `last_trending`, `best_rank` (the highest position the video reached, so 1 means top of the list), `n_snapshots`, `views_at_entry` (views at the first snapshot), `title`, `description` and `channel_title` as they were when the video first trended, and `category_name`.
- *Sponsor data per video*. We keep `category == "sponsor"` on YouTube and drop anything that is hidden, shadow hidden or has negative votes. Then we count segments, add up the sponsor seconds and take the start of the first segment.

**03_match.R** does a left join on video ID and writes three files to `output/`:

- `report.txt` with the headline numbers, the table by category and 10 example titles (picked at random with `set.seed(20564)`)
- `sponsor_share_by_category.csv`
- `us_trending_sponsor_matches.csv`, the matched subset. It is kept small by cutting descriptions to 300 characters.

## Choices worth knowing about (and defending in the presentation)

- **Duplicate submissions are merged.** Several users often submit the same sponsor read with slightly different times. If we just counted rows, one sponsor read could show up as three segments. So segments that overlap within a video are merged first, and the count and the seconds come from the merged intervals. `n_sb_submissions` still has the raw number of rows.
- **Whole-video labels.** SponsorBlock lets users tag an entire video as sponsored (`actionType == "full"`), and those rows have no start or end time. They count as "has sponsor" but add no seconds. The report shows how many of these there are, and the matched CSV has them as `full_video_label`.
- **Missing from SponsorBlock is not the same as no sponsor.** SponsorBlock only knows about videos that its users watched and tagged. That is why the report has a coverage line: how many trending videos appear in SponsorBlock at all, with any category. The share of sponsored videos among the covered ones is probably closer to the truth than the share among all trending videos. Coverage is also likely skewed toward tech and gaming, so read the category shares with that in mind.
- **Views at entry** comes from the dataset's first snapshot of the video. That is the view count when the video was first seen on the list, not when it was uploaded.

## If something doesn't match the real files

I couldn't open the dataset documentation while writing these scripts, so a few things are guesses, and each one fails with a clear message:

- **Column names.** `TRENDING_COLS` in `00_config.R` has a list of likely header names for each field. If 01 stops with "Could not find these columns", it also prints the real header. Add the right name to the list and run it again.
- **Finding the download link.** 01 first tries the dataset's JSON listing and then falls back to scraping the page. If neither works, copy the download link of the file from the dataset page and run `YTSB_TRENDING_URL="<link>" Rscript 01_download.R`.
- **Region values.** 01 prints the region values it finds in the first chunk. If the US is stored as something other than `US`, change `REGION` in `00_config.R`.
- **Mirror file URLs.** The SponsorBlock files are downloaded from `https://sb.ltn.fi/database/<file>`. If the mirror has moved them, put the files in `data/raw/` yourself and run with `YTSB_OFFLINE=TRUE`.

## Test

`Rscript tests/smoke_test.R` builds a small fake release and a fake SponsorBlock dump with the awkward cases in them (descriptions with line breaks, quotes and commas, a zipped archive, overlapping submissions, hidden and downvoted segments, a whole-video label, non-US rows). It then runs all three scripts on them and checks the numbers. It needs no internet connection and takes a few seconds.

## Folder layout

```
00_config.R  01_download.R  02_clean.R  03_match.R
data/raw/        downloaded files (read-only, not in git)
data/interim/    intermediate .rds tables (not in git)
output/          report and small CSVs (goes in the submission)
tests/smoke_test.R
```

## Submission

Following the course protocol, zip this folder as `20564 – Group Project – Group XX.zip` (use two digits, e.g. `Group 05`), or `20564 – Individual Project – Last First.zip` if you are a non-attending student. Leave `data/` out of the zip.
