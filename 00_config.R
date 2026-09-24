# 00_config.R
# Shared settings and helper functions. Every numbered script sources this
# file first, so paths, thresholds and column names live in one place.
# Run all scripts from the project folder (the one containing this file).

suppressPackageStartupMessages({
  library(data.table)
})

# ---- Paths ------------------------------------------------------------------
# data/raw     : downloaded files, never modified (made read-only after download)
# data/interim : intermediate tables written by our scripts
# output       : small results that go into the submission
DATA_DIR    <- Sys.getenv("YTSB_DATA_DIR", "data")
RAW_DIR     <- file.path(DATA_DIR, "raw")
INTERIM_DIR <- file.path(DATA_DIR, "interim")
OUTPUT_DIR  <- Sys.getenv("YTSB_OUTPUT_DIR", "output")
for (d in c(RAW_DIR, INTERIM_DIR, OUTPUT_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- Sources ----------------------------------------------------------------
TRENDING_DATASET_URL <- "https://databank.illinois.edu/datasets/IDB-9307654"
TRENDING_MEMBER      <- "most_popular.csv"   # file we need inside the release
# If automatic discovery of the download link fails, paste the direct link of
# the file that contains most_popular.csv here (copy it from the dataset page).
TRENDING_URL_OVERRIDE <- Sys.getenv("YTSB_TRENDING_URL", "")

SB_MIRROR_URL <- "https://sb.ltn.fi/database/"
SB_FILES      <- c("sponsorTimes.csv", "videoInfo.csv")

# ---- Rules ------------------------------------------------------------------
MAX_DOWNLOAD_GB <- 30      # anything bigger stops and asks (see README)
ALLOW_LARGE     <- isTRUE(as.logical(Sys.getenv("YTSB_ALLOW_LARGE", "FALSE")))
OFFLINE         <- isTRUE(as.logical(Sys.getenv("YTSB_OFFLINE", "FALSE")))
REGION          <- "US"
SEED            <- 20564   # used wherever we sample (example titles)
CHUNK_LINES     <- as.integer(Sys.getenv("YTSB_CHUNK_LINES", "250000"))  # lines per read

# ---- Trending table columns -------------------------------------------------
# I could not open the dataset documentation when writing this, so each field
# lists plausible header names. The first one found in the real header wins
# (case-insensitive). If 01_download.R stops with "missing columns", look at
# the header it prints and add the right name at the front of the list.
TRENDING_COLS <- list(
  snapshot_time = c("snapshot_time", "snapshot_timestamp", "timestamp", "trending_time",
                    "collection_time", "collected_at", "snapshot_date", "trending_date",
                    "datetime", "date"),
  region        = c("region_code", "country_code", "country", "region"),
  rank          = c("rank", "trending_rank", "position"),
  video_id      = c("video_id", "videoid", "video_youtube_id", "id"),
  title         = c("video_title", "title"),
  description   = c("video_description", "description"),
  channel_title = c("channel_title", "channel_name", "video_channel_title", "channel"),
  channel_id    = c("channel_id", "video_channel_id", "channelid"),
  category      = c("video_category_id", "category_id", "categoryid", "video_category",
                    "category"),
  views         = c("video_view_count", "view_count", "views", "viewcount")
)
TRENDING_REQUIRED <- c("snapshot_time", "region", "rank", "video_id", "title",
                       "channel_title", "category", "views")

# YouTube Data API category ids (US region). Used only if the dataset stores
# ids rather than names.
YT_CATEGORIES <- c(
  "1" = "Film & Animation", "2" = "Autos & Vehicles", "10" = "Music",
  "15" = "Pets & Animals", "17" = "Sports", "18" = "Short Movies",
  "19" = "Travel & Events", "20" = "Gaming", "21" = "Videoblogging",
  "22" = "People & Blogs", "23" = "Comedy", "24" = "Entertainment",
  "25" = "News & Politics", "26" = "Howto & Style", "27" = "Education",
  "28" = "Science & Technology", "29" = "Nonprofits & Activism",
  "30" = "Movies", "43" = "Shows", "44" = "Trailers"
)

# ---- Helpers ----------------------------------------------------------------

# Map canonical field names to the actual header names in the file.
resolve_columns <- function(header, candidates = TRENDING_COLS,
                            required = TRENDING_REQUIRED) {
  low <- tolower(trimws(header))
  hit <- vapply(candidates, function(cands) {
    i <- match(tolower(cands), low)
    i <- i[!is.na(i)]
    if (length(i)) header[i[1]] else NA_character_
  }, character(1))
  missing <- setdiff(required, names(hit)[!is.na(hit)])
  if (length(missing)) {
    stop("Could not find these columns: ", paste(missing, collapse = ", "),
         "\nHeader of the file is:\n  ", paste(header, collapse = ", "),
         "\nAdd the right names to TRENDING_COLS in 00_config.R.", call. = FALSE)
  }
  hit[!is.na(hit)]
}

# Parse timestamps whatever format the file uses (ISO text or unix seconds).
parse_time <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  x <- trimws(as.character(x))
  if (all(grepl("^[0-9]+(\\.[0-9]+)?$", x[!is.na(x)]))) {
    return(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"))
  }
  x <- sub("Z$", "", sub("T", " ", x))
  x <- sub("([+-][0-9]{2}:?[0-9]{2})$", "", x)   # drop offsets, data is UTC
  as.POSIXct(x, tz = "UTC",
             tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d"))
}

# fread keeps CSV-escaped quotes doubled ("" instead of "). Undo that in
# every character column so titles and descriptions read correctly.
undouble_quotes <- function(dt) {
  for (j in names(dt)[vapply(dt, is.character, TRUE)]) {
    set(dt, j = j, value = gsub('""', '"', dt[[j]], fixed = TRUE))
  }
  dt
}

# Count double quotes per line; used to find where CSV records end.
count_quotes <- function(x) {
  nchar(x, type = "bytes", allowNA = TRUE) -
    nchar(gsub('"', "", x, fixed = TRUE, useBytes = TRUE), type = "bytes", allowNA = TRUE)
}

# Stream a CSV from any connection and keep only rows where `filter_col`
# equals `filter_value`. Reads `chunk_lines` lines at a time, so memory use
# stays flat however big the file is. Quoted fields containing newlines
# (video descriptions!) are handled: a record is complete only when it has
# an even number of quote characters, so partial records are carried over
# to the next chunk.
stream_filter_csv <- function(con, filter_col, filter_value, keep_cols,
                              chunk_lines = CHUNK_LINES) {
  header_line <- readLines(con, n = 1L, warn = FALSE, encoding = "UTF-8")
  header <- names(fread(text = c(header_line, ""), header = TRUE, sep = ","))
  cols <- resolve_columns(header)
  message("Header found: ", paste(header, collapse = ", "))
  message("Using columns: ", paste(sprintf("%s <- %s", names(cols), cols), collapse = "; "))
  if (!filter_col %in% names(cols)) stop("Filter column not resolved: ", filter_col)
  fcol <- cols[[filter_col]]

  out <- list(); carry <- character(0)
  n_lines <- 0; n_rows_seen <- 0; first_chunk <- TRUE; k <- 0L
  repeat {
    x <- readLines(con, n = chunk_lines, warn = FALSE, encoding = "UTF-8")
    eof <- length(x) == 0L
    lines <- c(carry, x)
    if (!length(lines)) break
    n_lines <- n_lines + length(x)

    ends <- (cumsum(count_quotes(lines)) %% 2L) == 0L   # TRUE where a record ends
    last <- if (any(ends)) max(which(ends)) else 0L
    if (eof && last < length(lines)) {
      warning(length(lines) - last, " trailing lines form an incomplete record; dropped.")
    }
    block <- if (last > 0L) lines[seq_len(last)] else character(0)
    carry <- if (last < length(lines)) lines[(last + 1L):length(lines)] else character(0)

    if (length(block)) {
      # Cheap pre-filter: drop records that do not contain the value at all.
      # The first chunk is parsed in full so we can report what regions exist.
      if (!first_chunk) {
        rec <- cumsum(c(1L, head(ends[seq_len(last)], -1L)))
        keep_rec <- unique(rec[grepl(filter_value, block, fixed = TRUE, useBytes = TRUE)])
        block <- block[rec %in% keep_rec]
      }
      if (length(block)) {
        dt <- fread(text = c(header_line, block), sep = ",", quote = "\"",
                    header = TRUE, colClasses = "character", na.strings = c("", "NA"),
                    encoding = "UTF-8", showProgress = FALSE, fill = TRUE)
        n_rows_seen <- n_rows_seen + nrow(dt)
        if (first_chunk) {
          message("Region values in first chunk: ",
                  paste(head(sort(unique(dt[[fcol]])), 30), collapse = ", "))
        }
        dt <- dt[trimws(get(fcol)) == filter_value]
        keep <- intersect(unname(cols[names(cols) %in% keep_cols]), names(dt))
        dt <- undouble_quotes(dt[, ..keep])
        setnames(dt, keep, names(cols)[match(keep, cols)])
        if (nrow(dt)) { k <- k + 1L; out[[k]] <- dt }
      }
      first_chunk <- FALSE
    }
    message(sprintf("  read %s lines, kept %s rows so far",
                    format(n_lines, big.mark = ","),
                    format(sum(vapply(out, nrow, 0L)), big.mark = ",")))
    if (eof) break
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

# Open a read connection to a CSV that may sit inside an archive, without
# extracting the archive to disk.
open_member_stream <- function(path, member) {
  p <- tolower(path)
  if (grepl("\\.zip$", p)) {
    entries <- utils::unzip(path, list = TRUE)$Name   # reads the index only
    m <- entries[basename(entries) == member]
    if (!length(m)) stop(member, " not in ", path, ". Entries: ", paste(entries, collapse = ", "))
    message("Streaming '", m[1], "' out of ", basename(path))
    # `unzip -p` handles zip64 (>4 GB) archives; fall back to R's unz().
    if (nzchar(Sys.which("unzip"))) {
      return(pipe(paste("unzip -p", shQuote(path), shQuote(m[1])), "r"))
    }
    return(unz(path, m[1], "r"))
  }
  if (grepl("\\.(tar|tar\\.gz|tgz|tar\\.bz2|tar\\.xz)$", p)) {
    entries <- utils::untar(path, list = TRUE)
    m <- entries[basename(entries) == member]
    if (!length(m)) stop(member, " not in ", path)
    return(pipe(paste("tar -xOf", shQuote(path), shQuote(m[1])), "r"))
  }
  if (grepl("\\.7z$", p)) {
    if (!nzchar(Sys.which("7z"))) stop("7z archive: install 7-Zip and put `7z` on PATH.")
    return(pipe(paste("7z e -so", shQuote(path), shQuote(member)), "r"))
  }
  if (grepl("\\.gz$", p))  return(gzfile(path, "r"))
  if (grepl("\\.bz2$", p)) return(bzfile(path, "r"))
  if (grepl("\\.xz$", p))  return(xzfile(path, "r"))
  file(path, "r")
}

fmt_gb <- function(bytes) sprintf("%.2f GB", bytes / 1e9)
