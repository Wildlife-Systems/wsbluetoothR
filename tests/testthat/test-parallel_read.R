# Parallel chunked reading must be invariant to how the input is split into
# byte-range chunks. These tests generate a dataset, compute a reference result
# with a single huge chunk, then re-run with progressively tinier chunk sizes
# (WSBT_CHUNK_BYTES, down to 1 byte = a boundary at every offset) and require the
# results to be identical. That exercises the line-ownership boundary logic and
# the per-thread accumulator merges across every converted reader.

# ---- helpers ----------------------------------------------------------------

# Run `fun()` with WSBT_CHUNK_BYTES forced to `val`.
run_with_chunks <- function(val, fun) {
  old <- Sys.getenv("WSBT_CHUNK_BYTES", unset = NA)
  Sys.setenv(WSBT_CHUNK_BYTES = as.character(val))
  on.exit({
    if (is.na(old)) Sys.unsetenv("WSBT_CHUNK_BYTES") else Sys.setenv(WSBT_CHUNK_BYTES = old)
  })
  fun()
}

# Deterministic synthetic dataset. Timestamps increase with file order (like the
# real time-sorted data), lines vary in length, names are fixed per address so
# category classification is unambiguous.
make_lines <- function(n = 150) {
  devices <- c("16", "17", "201")                     # 2- and 3-char device ids
  addrs   <- sprintf("AA:BB:CC:DD:EE:%02X", 1:8)
  names_by_addr <- c(
    "AA:BB:CC:DD:EE:01" = "iPhone 13",
    "AA:BB:CC:DD:EE:02" = "Galaxy S21 Ultra",
    "AA:BB:CC:DD:EE:03" = "lime-1234",                 # bike
    "AA:BB:CC:DD:EE:04" = "BYD Seal",                  # vehicle
    "AA:BB:CC:DD:EE:05" = "[TV] Samsung",              # beacon
    "AA:BB:CC:DD:EE:06" = "WHOOP 4.0",                 # wearable
    "AA:BB:CC:DD:EE:07" = "",                          # no name
    "AA:BB:CC:DD:EE:08" = "Pixel"
  )

  base <- as.POSIXct("2025-08-15 00:00:00", tz = "UTC")
  ts   <- base + (seq_len(n) - 1L) * 1800             # 30 min apart -> spans days

  # rotate deterministically through devices/addresses
  dev  <- devices[((seq_len(n) - 1L) %% length(devices)) + 1L]
  addr <- addrs[((seq_len(n) - 1L) %% length(addrs)) + 1L]
  pow  <- -40L - ((seq_len(n) - 1L) %% 50L)
  dt   <- format(ts, "%Y%m%d-%H%M%S")
  nm   <- names_by_addr[addr]

  vapply(seq_len(n), function(i) {
    fields <- c(dev[i], dt[i], addr[i], as.character(pow[i]))
    if (nzchar(nm[i])) fields <- c(fields, nm[i])
    paste(fields, collapse = "\t")
  }, character(1))
}

write_lines_file <- function(lines, path) {
  writeLines(lines, path)   # default connection: CRLF on Windows, LF elsewhere
  path
}

# Order-insensitive comparison: sort rows by key columns and rebuild a plain
# data.frame from the column data alone, so that per-call attributes (e.g.
# process_bluetooth's processing_time) and custom classes don't affect equality.
canon <- function(df, keys) {
  df <- as.data.frame(df)
  o <- do.call(order, df[keys])
  cols <- lapply(df, function(col) col[o])
  out <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(out) <- NULL
  out
}

chunk_sizes <- c(1, 2, 3, 7, 16, 64)

# ---- get_address_duration ---------------------------------------------------

test_that("get_address_duration is invariant to chunk boundaries", {
  f <- write_lines_file(make_lines(), tempfile(fileext = ".txt"))
  on.exit(unlink(f))

  keys <- c("device", "date", "address")
  ref <- canon(run_with_chunks(1e9, function()
    get_address_duration(f, verbose = FALSE)), keys)

  for (cb in chunk_sizes) {
    got <- canon(run_with_chunks(cb, function()
      get_address_duration(f, verbose = FALSE)), keys)
    expect_equal(got, ref, info = paste("chunk bytes =", cb))
  }
})

# ---- process_bluetooth ------------------------------------------------------

test_that("process_bluetooth is invariant to chunk boundaries", {
  f <- write_lines_file(make_lines(), tempfile(fileext = ".txt"))
  on.exit(unlink(f))

  keys <- c("device", "datetime")
  ref <- canon(run_with_chunks(1e9, function()
    process_bluetooth(f, verbose = FALSE)), keys)

  for (cb in chunk_sizes) {
    got <- canon(run_with_chunks(cb, function()
      process_bluetooth(f, verbose = FALSE)), keys)
    expect_equal(got, ref, info = paste("chunk bytes =", cb))
  }
})

# ---- get_address_paths ------------------------------------------------------

test_that("get_address_paths is invariant to chunk boundaries", {
  f <- write_lines_file(make_lines(), tempfile(fileext = ".txt"))
  on.exit(unlink(f))

  keys <- c("address")
  ref <- canon(run_with_chunks(1e9, function()
    get_address_paths(f, top_n = 100, verbose = FALSE)), keys)

  for (cb in chunk_sizes) {
    got <- canon(run_with_chunks(cb, function()
      get_address_paths(f, top_n = 100, verbose = FALSE)), keys)
    expect_equal(got, ref, info = paste("chunk bytes =", cb))
  }
})

# ---- classify_addresses (find_matching_addresses) ---------------------------

test_that("classify_addresses is invariant to chunk boundaries", {
  f <- write_lines_file(make_lines(), tempfile(fileext = ".txt"))
  on.exit(unlink(f))

  keys <- c("address")
  ref <- canon(run_with_chunks(1e9, function()
    classify_addresses(f, categories = NULL, verbose = FALSE)), keys)

  for (cb in chunk_sizes) {
    got <- canon(run_with_chunks(cb, function()
      classify_addresses(f, categories = NULL, verbose = FALSE)), keys)
    expect_equal(got, ref, info = paste("chunk bytes =", cb))
  }
})

# ---- low_memory batching matches normal mode --------------------------------

test_that("low_memory matches normal mode for any devices-per-pass", {
  f <- write_lines_file(make_lines(150), tempfile(fileext = ".txt"))
  on.exit(unlink(f))

  keys <- c("device", "date", "address")
  ref <- canon(get_address_duration(f, verbose = FALSE, low_memory = FALSE), keys)

  for (dpp in c(1, 2, 3, 100)) {
    old <- Sys.getenv("WSBT_DEVICES_PER_PASS", unset = NA)
    Sys.setenv(WSBT_DEVICES_PER_PASS = as.character(dpp))
    got <- canon(get_address_duration(f, verbose = FALSE, low_memory = TRUE), keys)
    if (is.na(old)) Sys.unsetenv("WSBT_DEVICES_PER_PASS") else
      Sys.setenv(WSBT_DEVICES_PER_PASS = old)
    expect_equal(got, ref, info = paste("devices_per_pass =", dpp))
  }
})

# ---- multi-file == single concatenated file ---------------------------------

test_that("splitting input across files matches one concatenated file", {
  lines <- make_lines(120)

  one <- write_lines_file(lines, tempfile(fileext = ".txt"))
  parts <- split(lines, (seq_along(lines) - 1L) %% 3L)
  many <- vapply(parts, function(p) write_lines_file(p, tempfile(fileext = ".txt")),
                 character(1))
  on.exit(unlink(c(one, many)))

  keys <- c("device", "date", "address")
  single <- canon(run_with_chunks(7, function()
    get_address_duration(one, verbose = FALSE)), keys)
  multi  <- canon(run_with_chunks(7, function()
    get_address_duration(many, verbose = FALSE)), keys)

  expect_equal(multi, single)
})
