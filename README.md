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

The big CSV reads go through [DuckDB](https://duckdb.org), because the trending file has messy quoting that `fread` can't handle when it's read in pieces. You don't need to install it: if the `duckdb` program isn't on your computer, `01_download.R` downloads the stand-alone program (about 17 MB) into `tools/` the first time it runs.

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

**01_download.R** first reads the list of files in the Illinois release, together with their sizes, and prints it. It then downloads only the file that contains `most_popular.csv`. The release is a zip holding a `.tar.bz2` holding `most_popular.csv`. The script reads the CSV straight through both layers without unpacking anything to disk, working out from a small sample whether quotes are escaped as `""` or `\"`, keeps the US rows and only the columns we need, and saves them to `data/interim/us_trending_rows.rds`. After that it downloads the two SponsorBlock files. Every download resumes if it gets interrupted, is skipped if the file is already there, and is made read-only once it finishes.

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
- **Downloading the trending file in the browser.** If the script can't download it, download it yourself from the dataset page and put it in `data/raw/` without renaming or unzipping it. 01 then uses that file and still fetches the SponsorBlock files.
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

---

# Part 2: Sponsored videos dataset

Research question: do sponsored YouTube videos keep gaining views for longer than unsponsored videos from the same creators, and do bigger sponsored videos cause bigger short-term jumps in public interest in the sponsoring brand? The scripts below only build the data. No models are fitted here.

## Scripts

- `sponsor_utils.R` has the settings (dates, thresholds, quota limit, user agent) and the helper functions. The other two scripts load it.
- `sponsor_pipeline.R` builds the dataset one stage at a time: `Rscript sponsor_pipeline.R <stage>`.
- `poll_views.R` is the daily job that records view curves.
- `channel_seeds.csv` is my hand-picked list of channel handles (see "Channel selection rule").
- `tests/smoke_test_sponsor.R` runs every stage on fake API responses, so it needs no internet and no key.

## Setup

1. Packages: `install.packages(c("data.table", "httr2", "jsonlite", "gtrendsR"))`. Running `Rscript sponsor_pipeline.R check` installs any that are missing and reports what's there.
2. Put the YouTube key in `~/.Renviron` as `YT_API_KEY=...` and restart R. The scripts send it as a request header (never in the URL), and they remove it from any error message before printing. Nothing writes it to a file.
3. The SponsorBlock and Wikipedia requests send a User-Agent with the contact email from `SPONSOR_CONTACT_EMAIL` (default marta.carli@studbocconi.it).

## Run order

```
Rscript sponsor_pipeline.R check
Rscript sponsor_pipeline.R candidates        # writes channels_candidates.csv, then review it
Rscript sponsor_pipeline.R channels          # channels.csv = the rows with selected == TRUE

# test on 5 random channels first, in a separate folder
SPONSOR_TEST_N=5 SPONSOR_DATA_DIR=data/test Rscript sponsor_pipeline.R videos
SPONSOR_TEST_N=5 SPONSOR_DATA_DIR=data/test Rscript sponsor_pipeline.R label

Rscript sponsor_pipeline.R videos
Rscript sponsor_pipeline.R label
Rscript sponsor_pipeline.R handcheck
Rscript sponsor_pipeline.R brand_candidates  # then write brands.csv by hand, then review it
Rscript sponsor_pipeline.R events
Rscript sponsor_pipeline.R panel             # slow: Google Trends, 8 to 15 s between calls
Rscript sponsor_pipeline.R comments          # may need several days of quota
Rscript sponsor_pipeline.R summary
Rscript poll_views.R                         # every day, from cron
```

You can stop any stage and run it again. YouTube responses are kept in `data/raw/`, and SponsorBlock, Wikipedia, Trends and comment counts are cached in `data/cache/`. A rerun reads those files back and skips the API call. If the daily quota runs out partway through a stage, it saves what it has and stops; running it again the next day continues from there.

## Quota

The scripts only call the `videos`, `playlistItems`, `channels` and `commentThreads` endpoints, and never `search`. Each call costs 1 unit. Every call is logged in `data/quota_log.csv` (quota_day, time_utc, endpoint, units, script). Before each call the script checks that today's total stays under 8,000. "Today" is the Pacific-time date, because that's when YouTube resets the quota. Rough costs:

| step | units |
|---|---|
| candidates | about 250 handles + 96 chart pages + 10 |
| videos | about 1 per 50 uploads per channel, plus 1 per 50 videos: about 1,500 |
| poll_views.R | about 300 a day |
| comments | up to 5 per sponsored video, which is the step that takes several days |

## Channel selection rule

`search` costs 100 units, so channels come from two cheap sources:

1. `channel_seeds.csv`: 242 handles across the five categories that I picked by hand as English-language creators likely to run sponsor reads. Each handle is looked up with `channels?forHandle=`. Handles that don't exist are skipped.
2. The `mostPopular` video chart for categories 28 (tech), 27 (science and education), 20 (gaming), 26 and 22 (lifestyle) and 24 (commentary), in the US, GB, CA and AU, 4 pages each. Each channel gets the category that most of its chart videos had.

A channel is **eligible** if it has 100,000 to 2,000,000 subscribers (not hidden), is not made for kids, has at least 20 videos, and looks English-language. For seeds that means the country is one of US, GB, CA, AU, IE or NZ, or empty. For chart channels it means the country is in that list, or the country is empty and the channel's default language, or most of its chart videos' audio, is English. At most 50 eligible channels per category are **selected**; where a category has more, 50 are drawn at random with `set.seed(20564)`.

## Data files

Everything collected goes into `data/` (this folder is not in git). The hand-edited files `channel_seeds.csv`, `channels_candidates.csv`, `channels.csv` and `brands.csv` live next to the scripts, so they are in git.

| file | one row per | columns |
|---|---|---|
| `channels_candidates.csv` | channel looked up | channel_id, title, handle, subscriber_count, category, selected, eligible, source, country, default_language, video_count, view_count, made_for_kids, topics |
| `channels.csv` | panel channel | channel_id, title, handle, subscriber_count, category |
| `data/videos.csv` | video since 2025-01-01, longer than 60 s | video_id, channel_id, channel_title, title, description, published_at, category_id, language, duration_sec, views, likes, comments, retrieved_at |
| `data/description_links.csv` | link in a description | video_id, url, domain |
| `data/videos_labelled.csv` | video at least 14 days old | the videos.csv columns plus sb_sponsored, sb_full_video, sb_n_segments, sb_ad_seconds, sb_first_start, sb_ad_share, sb_ad_position, desc_sponsor_text, desc_patterns, promo_code, sponsored, label_source, labelled_on |
| `data/handcheck_unsponsored.csv` | sampled "neither" video | video_id, url, title, ad_found |
| `data/brand_candidates.csv` | domain linked from sponsored videos | domain, n_videos, n_channels, example_url, n_videos_unsponsored, shortener |
| `brands.csv` | sponsor brand | domain, brand, wiki_article, trends_term, youtube_channel_id, unsure |
| `data/sponsor_events.csv` | sponsored video x brand | event_id, video_id, brand, channel_id, channel_title, title, published_at, upload_day, views, likes, comments, duration_sec, sb_ad_seconds, sb_ad_share, sb_ad_position, label_source, promo_code, brand_n_events, rare_brand, n_overlapping, wiki_article, trends_term, unsure |
| `data/brand_event_panel.csv` | event x day, rel_day -28 to 28 | event_id, video_id, brand, date, rel_day, wiki_views, trends_web, trends_youtube, channel_id, upload_day, views, likes, comments, duration_sec, sb_ad_seconds, sb_ad_share, sb_ad_position, label_source, brand_n_events, rare_brand, n_overlapping |
| `data/comment_mentions.csv` | event (video x brand) | video_id, brand, n_comments_fetched, n_mentions, share_mentions, comments_status |
| `data/views_panel.csv` | video x poll day | poll_date, video_id, channel_id, published_at, age_days, views, likes, comments, retrieved_at |
| `data/channel_panel.csv` | channel x poll day | poll_date, channel_id, subscriber_count, view_count, video_count, retrieved_at |
| `data/quota_log.csv` | API call | quota_day, time_utc, endpoint, units, script |

### How the labels are built

- **SponsorBlock** (`sponsor.ajay.app/api/skipSegments`, category `sponsor`, action types skip, mute and full). There is one call per video with a pause between calls, and each answer is cached in `data/cache/sponsorblock/`. A 404 means nobody submitted a sponsor segment, so it counts as `sb_sponsored = FALSE`. Overlapping segments are merged before counting, because several users often submit the same read. A whole-video label ("full") makes the video sponsored but adds no seconds, and its `sb_ad_position` is `full_video`. Position: `early` if the first ad starts in the first 15% of the video, `middle` if it starts before 60%, `late` otherwise.
- **Description**: `desc_sponsor_text` is TRUE if the description matches any pattern in `SPONSOR_PATTERNS` in `sponsor_utils.R` ("sponsored by", "thanks to ... for sponsoring", "today's sponsor", "this video is sponsored", "use code", "promo/coupon/discount code", "paid promotion", "#ad", "#sponsored"). `desc_patterns` lists which ones matched.
- `sponsored = sb_sponsored OR desc_sponsor_text`. `label_source` is `both`, `sponsorblock_only`, `description_only` or `neither`, and NA if the SponsorBlock call failed (those are retried on the next run).
- **Links**: every http(s) link in each description, minus social media, link hubs, and Patreon-type pages (the list is `EXCLUDED_DOMAINS`). Link shorteners are kept in `description_links.csv` but flagged in `brand_candidates.csv`.

### Brand attention panel

- `wiki_views`: daily English Wikipedia pageviews (all-access, user agents only), one request per brand covering every one of its event windows. The API leaves out days with zero views, so those are filled with 0. Days after yesterday are NA.
- `trends_web` and `trends_youtube`: Google Trends for `trends_term` in `TRENDS_GEO` (default GB), with one query per event window, because Trends rescales every query to 0 to 100. "<1" is stored as 0.5. Events with rare_brand and no overlap are queried first. On HTTP 429 the script waits 2, 4 and then 8 minutes. If Trends still refuses, it stops and keeps what it has. A window that ends less than 3 days ago is skipped until a later run.
- `comment_mentions.csv`: up to 500 top-level comments per sponsored video, in relevance order. A comment counts if it contains the brand name or the description's promo code as a whole word, case-insensitive. Only the counts are stored. The comment text is matched in memory and never written anywhere, and the commentThreads responses are not saved to `data/raw/`.

## Daily polling with cron (macOS)

Not set up yet. When you want it, run `crontab -e` and add one line (change the path):

```
15 9 * * * cd "/Users/YOU/path/to/bigdata" && /usr/local/bin/Rscript poll_views.R >> data/poll.log 2>&1
```

Use `which Rscript` to get the right path (on Apple Silicon it's usually `/opt/homebrew/bin/Rscript` or `/Library/Frameworks/R.framework/Resources/bin/Rscript`). Cron doesn't read `~/.Renviron` through your shell, but R itself does at startup, so the key still gets picked up. The Mac has to be awake at that time. If it's often asleep, `launchd` with `StartCalendarInterval` catches up on a missed run, while cron just skips it. You also have to give `cron` Full Disk Access in System Settings, Privacy & Security if the project sits in Documents or Desktop.

## Data collection log

| date | what was done | channels | videos | events | brands | quota used | notes |
|---|---|---|---|---|---|---|---|
| 2026-10-04 | Wrote sponsor_utils.R, sponsor_pipeline.R, poll_views.R, channel_seeds.csv and the offline test. Ran the `check` stage. | 0 | 0 | 0 | 0 | 0 | Nothing collected yet. In the cloud sandbox YT_API_KEY was not set, gtrendsR could not be installed (CRAN blocked), and sponsor.ajay.app, wikimedia.org and trends.google.com were blocked by the network policy. The offline test (`tests/smoke_test_sponsor.R`) passes. |
