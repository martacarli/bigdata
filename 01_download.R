# 01_download.R
# Step 1: get the raw data.
#   a) Global YouTube Trending Dataset (2022-2025), Illinois Data Bank.
#      We list the files first, download only the one holding most_popular.csv,
#      then stream US rows out of it. The archive is never unpacked to disk.
#   b) SponsorBlock dump (sponsorTimes.csv, videoInfo.csv) from the sb.ltn.fi mirror.
#
# Any file above MAX_DOWNLOAD_GB (30 GB) stops the script. To go ahead after
# checking, re-run with the environment variable YTSB_ALLOW_LARGE=TRUE.
# Downloads resume if interrupted. Files already in data/raw are not fetched again.
#
# Output: data/interim/us_trending_rows.rds (every US snapshot row, needed columns only)

source("00_config.R")
suppressPackageStartupMessages(library(curl))

# ---- Remote helpers ---------------------------------------------------------

# Name and size of a remote file, asked from the server without downloading.
remote_file_info <- function(url) {
  last_headers <- function(res) {
    blocks <- curl::parse_headers(res$headers, multiple = TRUE)
    h <- blocks[[length(blocks)]][-1]
    setNames(sub("^[^:]+:\\s*", "", h), tolower(sub(":.*$", "", h)))
  }
  size <- NA_real_; name <- NA_character_; final_url <- url
  res <- tryCatch(curl_fetch_memory(url, new_handle(nobody = TRUE, followlocation = TRUE)),
                  error = function(e) NULL)
  if (!is.null(res) && res$status_code < 400) {
    h <- last_headers(res); final_url <- res$url
    if (!is.na(h["content-length"])) size <- as.numeric(h["content-length"])
    if (!is.na(h["content-disposition"])) name <- h["content-disposition"]
  }
  if (is.na(size)) {
    # Some storage backends refuse HEAD; ask for one byte and read the total.
    res <- tryCatch(curl_fetch_memory(url, new_handle(range = "0-0", followlocation = TRUE)),
                    error = function(e) NULL)
    if (!is.null(res)) {
      h <- last_headers(res); final_url <- res$url
      cr <- h["content-range"]
      if (!is.na(cr)) size <- as.numeric(sub(".*/", "", cr))
      if (is.na(name) && !is.na(h["content-disposition"])) name <- h["content-disposition"]
    }
  }
  if (!is.na(name)) {
    name <- sub('.*filename\\*?=(UTF-8\'\')?"?([^";]+)"?.*', "\\2", name)
    name <- utils::URLdecode(name)
  } else {
    name <- basename(sub("\\?.*$", "", final_url))
  }
  data.table(url = url, name = unname(name), size = unname(size))
}

# List the data files of an Illinois Data Bank dataset.
list_databank_files <- function(dataset_url) {
  base <- sub("^(https?://[^/]+).*", "\\1", dataset_url)
  urls <- character(0)
  js <- tryCatch(jsonlite::fromJSON(paste0(dataset_url, ".json")), error = function(e) NULL)
  if (!is.null(js) && !is.null(js$datafiles) && length(js$datafiles)) {
    df <- as.data.table(js$datafiles)
    idcol <- intersect(c("web_id", "id"), names(df))[1]
    if (!is.na(idcol)) urls <- sprintf("%s/datafiles/%s/download", base, df[[idcol]])
  }
  if (!length(urls)) {
    html <- rawToChar(curl_fetch_memory(dataset_url)$content)
    links <- regmatches(html, gregexpr("/datafiles/[A-Za-z0-9_-]+/download", html))[[1]]
    urls <- paste0(base, unique(links))
  }
  if (!length(urls)) {
    stop("No data files found on ", dataset_url,
         ". Open the page, copy the download link of the file that contains ",
         TRENDING_MEMBER, " and set YTSB_TRENDING_URL.", call. = FALSE)
  }
  rbindlist(lapply(urls, remote_file_info))
}

# Stop unless the file is small enough or the user has said yes.
check_size <- function(size, label) {
  if (is.na(size)) {
    if (!ALLOW_LARGE) stop("Size of ", label, " is unknown. Check it by hand, then re-run with ",
                           "YTSB_ALLOW_LARGE=TRUE.", call. = FALSE)
  } else if (size / 1e9 > MAX_DOWNLOAD_GB && !ALLOW_LARGE) {
    stop(sprintf("%s is %s, over the %g GB limit. Nothing downloaded. ",
                 label, fmt_gb(size), MAX_DOWNLOAD_GB),
         "If you want it, re-run with YTSB_ALLOW_LARGE=TRUE.", call. = FALSE)
  }
  invisible(TRUE)
}

# Download to <dest>.part (resuming if present), then rename and lock.
download_resumable <- function(url, dest, size = NA_real_) {
  if (file.exists(dest) && (is.na(size) || file.size(dest) == size)) {
    message("Already have ", dest, ", skipping download.")
    return(invisible(dest))
  }
  part <- paste0(dest, ".part")
  h <- new_handle(followlocation = TRUE, failonerror = TRUE)
  if (file.exists(part)) {
    message("Resuming ", basename(dest), " from ", fmt_gb(file.size(part)))
    handle_setopt(h, resume_from_large = file.size(part))
  }
  # Append the incoming bytes to the .part file ourselves (curl_download
  # would replace the file and lose what we already have).
  out <- file(part, open = "ab")
  got <- file.size(part); t0 <- Sys.time(); last <- t0
  tryCatch(curl_fetch_stream(url, function(x) {
    writeBin(x, out)
    got <<- got + length(x)
    if (difftime(Sys.time(), last, units = "secs") > 10) {
      last <<- Sys.time()
      message(sprintf("  %s: %s of %s", basename(dest), fmt_gb(got), fmt_gb(size)))
    }
  }, handle = h), finally = close(out))
  if (!is.na(size) && file.size(part) != size) {
    stop("Size mismatch for ", dest, ": got ", file.size(part), ", expected ", size,
         ". Re-run to resume.", call. = FALSE)
  }
  file.rename(part, dest)
  Sys.chmod(dest, "0444")   # raw files are read-only from here on
  invisible(dest)
}

# Pick the release file that holds most_popular.csv.
choose_trending_file <- function(files) {
  stem <- sub("\\.csv$", "", TRENDING_MEMBER)
  direct <- files[grepl(stem, name, ignore.case = TRUE)]
  if (nrow(direct) == 1) return(direct)
  archives <- files[grepl("\\.(zip|tar|tgz|gz|bz2|xz|7z)$", name, ignore.case = TRUE)]
  if (nrow(archives) == 1) return(archives)
  print(files)
  stop("Cannot tell which file contains ", TRENDING_MEMBER,
       ". Set YTSB_TRENDING_URL to its download link.", call. = FALSE)
}

# ---- a) Trending dataset ----------------------------------------------------
message("== Global YouTube Trending Dataset ==")
if (OFFLINE) {
  local <- list.files(RAW_DIR, full.names = FALSE)
  local <- local[!grepl("^(sponsorTimes|videoInfo)|\\.part$", local)]
  pick <- choose_trending_file(data.table(url = NA, name = local, size = NA))
} else {
  files <- if (nzchar(TRENDING_URL_OVERRIDE)) remote_file_info(TRENDING_URL_OVERRIDE)
           else list_databank_files(TRENDING_DATASET_URL)
  message("Files in the release:")
  print(files[, .(name, size = fmt_gb(size))])
  pick <- choose_trending_file(files)
  message("Chosen: ", pick$name, " (", fmt_gb(pick$size), ")")
  check_size(pick$size, pick$name)
  download_resumable(pick$url, file.path(RAW_DIR, pick$name), pick$size)
}
trending_path <- file.path(RAW_DIR, pick$name)

keep_cols <- c("snapshot_time", "region", "rank", "video_id", "title", "description",
               "channel_title", "channel_id", "category", "views")
con <- open_member_stream(trending_path, TRENDING_MEMBER)
us_rows <- tryCatch(stream_filter_csv(con, "region", REGION, keep_cols),
                    finally = close(con))
if (!nrow(us_rows)) stop("No rows with region == '", REGION, "'. See the region values printed above.")
message(sprintf("Kept %s %s rows.", format(nrow(us_rows), big.mark = ","), REGION))
saveRDS(us_rows, file.path(INTERIM_DIR, "us_trending_rows.rds"))

# ---- b) SponsorBlock ---------------------------------------------------------
message("== SponsorBlock ==")
for (f in SB_FILES) {
  dest <- file.path(RAW_DIR, f)
  if (OFFLINE) {
    if (!file.exists(dest)) stop("Offline mode but ", dest, " is missing.")
    next
  }
  info <- remote_file_info(paste0(SB_MIRROR_URL, f))
  message(f, ": ", fmt_gb(info$size))
  check_size(info$size, f)
  download_resumable(info$url, dest, info$size)
}

message("Done. Next: Rscript 02_clean.R")
