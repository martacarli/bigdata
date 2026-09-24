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
LIST_ONLY       <- isTRUE(as.logical(Sys.getenv("YTSB_LIST_ONLY", "FALSE")))  # show sizes, download nothing
REGION          <- "US"
SEED            <- 20564   # used wherever we sample (example titles)
CHUNK_LINES     <- as.integer(Sys.getenv("YTSB_CHUNK_LINES", "250000"))  # lines per read

# ---- Trending table columns -------------------------------------------------
# I could not open the dataset documentation when writing this, so each field
# lists plausible header names. The first one found in the real header wins
# (case-insensitive). If 01_download.R stops with "missing columns", look at
# the header it prints and add the right name at the front of the list.
TRENDING_COLS <- list(
  snapshot_time = c("collection_date", "snapshot_time", "snapshot_timestamp", "timestamp", "trending_time",
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
  # Try the most detailed format first and only fill what is still missing,
  # so a mix of "date" and "date time" values keeps the times.
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  for (f in c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d")) {
    miss <- is.na(out) & !is.na(x)
    if (!any(miss)) break
    out[miss] <- as.POSIXct(x[miss], format = f, tz = "UTC")
  }
  out
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
  if (!length(header_line) || !nzchar(header_line)) {
    stop("Nothing came out of the archive. The file we need may have another name; ",
         "check dataset_info.txt in the release.", call. = FALSE)
  }
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
# Work out a file's type from its first bytes. Download services sometimes
# hand files out without a name or extension (e.g. ".../get").
sniff_type <- function(path) {
  b <- readBin(path, "raw", 262)
  starts <- function(hex) length(b) >= length(hex) && all(b[seq_along(hex)] == as.raw(hex))
  if (starts(c(0x50, 0x4B, 0x03, 0x04))) return("zip")
  if (starts(c(0x1F, 0x8B)))             return("gz")
  if (starts(c(0x37, 0x7A, 0xBC, 0xAF))) return("7z")
  if (starts(c(0x42, 0x5A, 0x68)))       return("bz2")
  if (starts(c(0xFD, 0x37, 0x7A, 0x58))) return("xz")
  if (length(b) >= 262 && rawToChar(b[258:262]) == "ustar") return("tar")
  "csv"
}

# tar option that decompresses a tar archive given its file name. For .bz2,
# lbzip2 (parallel) is used when installed because plain bzip2 is slow.
tar_decompress_flag <- function(name) {
  n <- tolower(name)
  if (grepl("\\.(tar\\.bz2|tbz2?)$", n)) {
    return(if (nzchar(Sys.which("lbzip2"))) "-I lbzip2" else "-j")
  }
  if (grepl("\\.(tar\\.gz|tgz)$", n)) return(if (nzchar(Sys.which("pigz"))) "-I pigz" else "-z")
  if (grepl("\\.(tar\\.xz|txz)$", n)) return("-J")
  ""
}

# Pull one CSV out of a tar stream in a single pass. Listing a compressed
# tar first would mean decompressing it twice, so we match the member by
# name pattern instead (the member at the top level or inside any folder).
tar_member_cmd <- function(name, member) {
  paste("tar -xO", tar_decompress_flag(name), "-f - --wildcards",
        shQuote(member), shQuote(paste0("*/", member)))
}

is_tar_name <- function(x) grepl("\\.(tar|tar\\.gz|tgz|tar\\.bz2|tbz2?|tar\\.xz|txz)$", tolower(x))

# Shell commands that write the CSV we need to stdout, without unpacking
# anything to disk. Returns list(src, post, total): `src` produces the bytes
# we measure for progress (`total` of them, NA if unknown), `post` (may be
# "") turns them into CSV text, e.g. release.zip -> youtube_trends.tar.bz2
# -> most_popular.csv is src = "unzip -p ... inner", post = "tar -x ...".
member_stream_parts <- function(path, member) {
  q <- shQuote(path)
  p <- tolower(path)
  if (!grepl("\\.(zip|tar|tgz|gz|bz2|xz|7z|csv)$", p)) {
    type <- sniff_type(path)
    message(basename(path), " has no known extension; its contents look like: ", type)
    p <- paste0(p, ".", type)   # only used to pick the reader below
  }
  if (grepl("\\.zip$", p)) {
    if (!nzchar(Sys.which("unzip"))) stop("Needs the `unzip` command line tool.")
    idx <- utils::unzip(path, list = TRUE)   # reads the zip's index only
    hit <- idx[basename(idx$Name) == member, ]
    if (nrow(hit)) {
      message("Streaming '", hit$Name[1], "' out of ", basename(path))
      return(list(src = paste("unzip -p", q, shQuote(hit$Name[1])), post = "",
                  total = hit$Length[1]))
    }
    # Not there directly: look inside a tar archive packed in the zip.
    inner <- idx[is_tar_name(idx$Name), ]
    if (!nrow(inner)) stop(member, " not in ", path, ". Entries: ", paste(idx$Name, collapse = ", "))
    message("Streaming '", member, "' out of '", inner$Name[1], "' inside ", basename(path),
            " (one pass, nothing unpacked to disk)")
    return(list(src = paste("unzip -p", q, shQuote(inner$Name[1])),
                post = tar_member_cmd(inner$Name[1], member), total = inner$Length[1]))
  }
  if (is_tar_name(p)) {
    return(list(src = paste("cat", q), post = tar_member_cmd(p, member), total = file.size(path)))
  }
  if (grepl("\\.7z$", p)) {
    if (!nzchar(Sys.which("7z"))) stop("7z archive: install 7-Zip and put `7z` on PATH.")
    return(list(src = paste("7z e -so", q, shQuote(member)), post = "", total = NA_real_))
  }
  dec <- c(gz = "gzip -dc", bz2 = if (nzchar(Sys.which("lbzip2"))) "lbzip2 -dc" else "bzip2 -dc",
           xz = "xz -dc")
  ext <- sub(".*\\.", "", p)
  list(src = paste("cat", q), post = if (ext %in% names(dec)) dec[[ext]] else "",
       total = file.size(path))
}

stream_cmd <- function(parts) {
  if (nzchar(parts$post)) paste(parts$src, "|", parts$post) else parts$src
}

# Kept for the rare case DuckDB is not available: an R connection to the CSV.
open_member_stream <- function(path, member) {
  pipe(stream_cmd(member_stream_parts(path, member)), "r")
}

# ---- DuckDB (used for the big CSV reads) ------------------------------------
# DuckDB's CSV reader copes with messy quoting and can skip a broken row
# instead of stopping, which fread on text chunks cannot. We use the
# stand-alone DuckDB program (one file, ~17 MB). If it is not installed it is
# downloaded once into tools/ from DuckDB's GitHub releases.
DUCKDB_VERSION <- "v1.3.2"

get_duckdb <- function() {
  if (identical(Sys.getenv("YTSB_ENGINE"), "r")) return(NA_character_)
  p <- Sys.which("duckdb")
  if (nzchar(p)) return(unname(p))
  win <- .Platform$OS.type == "windows"
  exe <- file.path("tools", if (win) "duckdb.exe" else "duckdb")
  if (file.exists(exe)) return(normalizePath(exe))
  sys <- Sys.info()[["sysname"]]; mach <- Sys.info()[["machine"]]
  asset <- if (sys == "Linux" && mach %in% c("x86_64", "amd64")) "duckdb_cli-linux-amd64.zip"
           else if (sys == "Linux" && mach %in% c("aarch64", "arm64")) "duckdb_cli-linux-arm64.zip"
           else if (sys == "Darwin") "duckdb_cli-osx-universal.zip"
           else if (win) "duckdb_cli-windows-amd64.zip" else NA_character_
  if (is.na(asset)) return(NA_character_)
  dir.create("tools", showWarnings = FALSE)
  z <- file.path("tools", asset)
  url <- sprintf("https://github.com/duckdb/duckdb/releases/download/%s/%s", DUCKDB_VERSION, asset)
  message("Getting DuckDB ", DUCKDB_VERSION, " (one-time download into tools/)")
  ok <- tryCatch(utils::download.file(url, z, mode = "wb", quiet = TRUE) == 0,
                 error = function(e) FALSE)
  if (!ok) { message("Could not download DuckDB; falling back to the R reader."); return(NA_character_) }
  utils::unzip(z, exdir = "tools"); file.remove(z)
  Sys.chmod(exe, "0755")
  normalizePath(exe)
}

# Run SQL with DuckDB, optionally feeding it a shell command's output as
# /dev/stdin. Returns the exit status.
run_duckdb <- function(duck, sql, input_cmd = NULL, log = tempfile()) {
  f <- tempfile(fileext = ".sql"); writeLines(sql, f)
  cmd <- paste(shQuote(duck), "-f", shQuote(f))
  if (!is.null(input_cmd)) cmd <- paste(input_cmd, "|", cmd)
  system(paste("bash -c", shQuote(paste(cmd, "2>", shQuote(log)))))
}

sql_str <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
sql_id  <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')

# Pull the rows of one region out of a huge CSV inside an archive.
# 1. Read the first ~20 MB to get the header and see how quotes are escaped
#    ("" or \"), because DuckDB cannot guess that reliably from a pipe.
# 2. Stream everything through DuckDB, keeping only the needed columns and
#    rows. Progress is printed every 30 s from `dd`, which counts the bytes.
extract_rows_duckdb <- function(duck, parts, filter_col, filter_value, keep_cols, out_csv) {
  work <- file.path(INTERIM_DIR, "_extract"); dir.create(work, showWarnings = FALSE)
  sample <- file.path(work, "sample.csv")
  system(paste("bash -c", shQuote(paste(stream_cmd(parts), "2>/dev/null | head -c 20000000 >",
                                        shQuote(sample)))))
  if (!file.exists(sample) || file.size(sample) == 0) {
    stop("Nothing came out of the archive. The file we need may have another name; ",
         "check dataset_info.txt in the release.", call. = FALSE)
  }
  header_line <- readLines(sample, n = 1L, warn = FALSE, encoding = "UTF-8")
  header <- names(fread(text = c(header_line, ""), header = TRUE, sep = ","))
  message("Header found: ", paste(header, collapse = ", "))
  cols <- resolve_columns(header)
  message("Using columns: ", paste(sprintf("%s <- %s", names(cols), cols), collapse = "; "))
  txt <- readChar(sample, file.size(sample), useBytes = TRUE)
  n_match <- function(pat) sum(gregexpr(pat, txt, fixed = TRUE, useBytes = TRUE)[[1]] > 0)
  n_bs <- n_match('\\"')
  n_dq <- n_match('""')
  escape <- if (n_bs > n_dq) "\\" else '"'
  message(sprintf("Quotes inside text are escaped as %s (seen %d times vs %d for the other style).",
                  if (escape == '"') '""' else '\\"', max(n_bs, n_dq), min(n_bs, n_dq)))
  rm(txt); file.remove(sample)

  keep <- cols[names(cols) %in% keep_cols]
  select <- paste(sprintf("%s AS %s", sql_id(keep), sql_id(names(keep))), collapse = ", ")
  rej_csv <- file.path(work, "rejected_count.csv")
  sql <- c(
    sprintf("COPY (SELECT %s FROM read_csv('/dev/stdin', header = true, delim = ',', quote = '\"', escape = %s, all_varchar = true, strict_mode = false, ignore_errors = true, store_rejects = true, max_line_size = 100000000) WHERE trim(%s) = %s) TO %s (HEADER, DELIMITER ',');",
            select, sql_str(escape), sql_id(cols[[filter_col]]), sql_str(filter_value), sql_str(out_csv)),
    sprintf("COPY (SELECT count(*) AS n FROM reject_errors) TO %s (HEADER);", sql_str(rej_csv))
  )
  sql_file <- file.path(work, "extract.sql"); writeLines(sql, sql_file)
  prog <- file.path(work, "progress.log"); done <- file.path(work, "done.txt")
  errlog <- file.path(work, "stderr.log")
  for (f in c(prog, done, errlog)) if (file.exists(f)) file.remove(f)
  gnu_dd <- system("dd status=progress if=/dev/null of=/dev/null 2>/dev/null") == 0
  meter <- if (gnu_dd) paste("| dd bs=4M status=progress 2>", shQuote(prog)) else ""
  pipeline <- paste(parts$src, "2>>", shQuote(errlog), meter,
                    if (nzchar(parts$post)) paste("|", parts$post, "2>>", shQuote(errlog)) else "",
                    "|", shQuote(duck), "-f", shQuote(sql_file), "2>>", shQuote(errlog),
                    "; echo $? >", shQuote(done))
  message("Reading the whole file now. This is the long step; progress every 30 seconds.")
  t0 <- Sys.time(); last <- t0
  system(paste("bash -c", shQuote(pipeline)), wait = FALSE)
  repeat {
    Sys.sleep(2)
    if (file.exists(done)) break
    if (difftime(Sys.time(), last, units = "secs") < 30) next
    last <- Sys.time()
    mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    got <- NA_real_
    if (file.exists(prog)) {
      lg <- readChar(prog, file.size(prog), useBytes = TRUE)
      m <- regmatches(lg, gregexpr("[0-9]+ bytes", lg))[[1]]
      if (length(m)) got <- as.numeric(sub(" bytes", "", m[length(m)]))
    }
    if (!is.na(got) && !is.na(parts$total) && got > 0) {
      left <- mins / (got / parts$total) - mins
      message(sprintf("  %s of %s read (%.0f%%), %.0f min so far, about %.0f min left",
                      fmt_gb(got), fmt_gb(parts$total), 100 * got / parts$total, mins, left))
    } else {
      message(sprintf("  still reading, %.0f min so far", mins))
    }
  }
  status <- as.integer(readLines(done, warn = FALSE)[1])
  if (!identical(status, 0L) || !file.exists(out_csv)) {
    stop("DuckDB stopped with an error. Last messages:\n",
         paste(tail(readLines(errlog, warn = FALSE), 15), collapse = "\n"), call. = FALSE)
  }
  n_rej <- if (file.exists(rej_csv)) fread(rej_csv)$n else NA
  if (!is.na(n_rej) && n_rej > 0) message(n_rej, " malformed rows were skipped (out of the whole file).")
  out <- fread(out_csv, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8")
  unlink(work, recursive = TRUE)
  undouble_quotes(out)
}

# Read selected columns of a big CSV: fread first (fast), DuckDB if fread
# chokes on the file's quoting.
read_csv_robust <- function(path, select) {
  tryCatch(fread(path, select = select, colClasses = "character", showProgress = FALSE),
    error = function(e) {
      duck <- get_duckdb()
      if (is.na(duck)) stop(e)
      message("fread could not read ", basename(path), " (", conditionMessage(e), "); using DuckDB.")
      tmp <- tempfile(fileext = ".csv")
      sql <- sprintf("COPY (SELECT %s FROM read_csv(%s, all_varchar = true, ignore_errors = true)) TO %s (HEADER);",
                     paste(sql_id(select), collapse = ", "), sql_str(normalizePath(path)), sql_str(tmp))
      if (run_duckdb(duck, sql) != 0) stop("DuckDB could not read ", path, call. = FALSE)
      out <- undouble_quotes(fread(tmp, colClasses = "character"))
      file.remove(tmp)
      out
    })
}

fmt_gb <- function(bytes) sprintf("%.2f GB", bytes / 1e9)
