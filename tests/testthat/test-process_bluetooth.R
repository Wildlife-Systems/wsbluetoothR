test_that("process_bluetooth works with test data", {
  # Get path to test data
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  
  # Skip if test data not found
  skip_if(test_file == "", "Test data file not found")
  
  # Process the test data
  result <- process_bluetooth(test_file, progress_interval = 10)
  
  # Check that result is a data.frame
  expect_s3_class(result, "data.frame")
  
  # Check that result has expected columns
  expect_true("device" %in% names(result))
  expect_true("datetime" %in% names(result))
  expect_true("count" %in% names(result))
  
  # Check that datetime is POSIXct
  expect_s3_class(result$datetime[1], "POSIXct")
  
  # Check that we have data
  expect_true(nrow(result) > 0)
  
  # Check that counts are positive integers
  expect_true(all(result$count > 0))
  expect_true(all(result$count == as.integer(result$count)))
})

test_that("process_bluetooth handles include_prefixes correctly", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Process with a prefix that likely won't match anything
  result_filtered <- process_bluetooth(test_file, 
                                       progress_interval = 10000,
                                       include_prefixes = c("XYZ_NoMatch"))
  
  # Should have no data rows (no names match)
  expect_equal(nrow(result_filtered), 0)
})

test_that("process_bluetooth handles exclude_prefixes correctly", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Process without filtering
  result_all <- process_bluetooth(test_file, progress_interval = 10000)
  
  # Process with exclude (this will exclude all rows since names are empty/short)
  result_excluded <- process_bluetooth(test_file, 
                                      progress_interval = 10000,
                                      exclude_prefixes = c(""))
  
  # Excluded result should have fewer or equal rows
  expect_true(nrow(result_excluded) <= nrow(result_all))
})

test_that("process_bluetooth handles non-existent file", {
  expect_error(
    process_bluetooth("nonexistent_file_xyz123.txt"),
    "not found"
  )
})

test_that("process_bluetooth returns correct data types", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  result <- process_bluetooth(test_file, progress_interval = 10000)
  
  # Check column types
  expect_type(result$device, "character")
  expect_s3_class(result$datetime, "POSIXct")
  expect_type(result$count, "integer")
})

test_that("datetime conversion handles correct format", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  result <- process_bluetooth(test_file, progress_interval = 10000)
  
  # Check that there are no NA datetimes
  expect_true(all(!is.na(result$datetime)))
  
  # Check timezone is UTC
  expect_equal(attr(result$datetime, "tzone"), "UTC")
})

test_that("process_bluetooth handles multiple files", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Create two temporary files by splitting the test data
  temp_dir <- tempdir()
  file1 <- file.path(temp_dir, "test_part1.txt")
  file2 <- file.path(temp_dir, "test_part2.txt")
  
  # Read test data
  lines <- readLines(test_file)
  mid <- ceiling(length(lines) / 2)
  
  # Write to two files
  writeLines(lines[1:mid], file1)
  writeLines(lines[(mid+1):length(lines)], file2)
  
  # Process single file
  result_single <- process_bluetooth(test_file, progress_interval = 10000)
  
  # Process multiple files
  result_multi <- process_bluetooth(c(file1, file2), progress_interval = 10000)
  
  # Results should be identical (same rows, same counts)
  expect_equal(nrow(result_single), nrow(result_multi))
  expect_equal(sum(result_single$count), sum(result_multi$count))
  expect_equal(result_single$device, result_multi$device)
  expect_equal(result_single$datetime, result_multi$datetime)
  expect_equal(result_single$count, result_multi$count)
  
  # Check attributes
  expect_true(!is.null(attr(result_multi, "total_lines")))
  expect_true(!is.null(attr(result_multi, "device_names")))
  
  # Clean up
  unlink(c(file1, file2))
})

test_that("multiple files aggregate counts correctly", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Create two temporary files with identical content
  temp_dir <- tempdir()
  file1 <- file.path(temp_dir, "test_dup1.txt")
  file2 <- file.path(temp_dir, "test_dup2.txt")
  
  # Copy same content to both files
  file.copy(test_file, file1, overwrite = TRUE)
  file.copy(test_file, file2, overwrite = TRUE)
  
  # Process single file
  result_single <- process_bluetooth(test_file, progress_interval = 10000)
  
  # Process both duplicate files
  result_double <- process_bluetooth(c(file1, file2), progress_interval = 10000)
  
  # Should have same number of rows
  expect_equal(nrow(result_single), nrow(result_double))
  
  # But counts should be doubled
  expect_equal(result_single$count * 2, result_double$count)
  
  # Total count should be exactly double
  expect_equal(sum(result_single$count) * 2, sum(result_double$count))
  
  # Clean up
  unlink(c(file1, file2))
})

test_that("multiple files with filtering", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Create temporary files
  temp_dir <- tempdir()
  file1 <- file.path(temp_dir, "test_filter1.txt")
  file2 <- file.path(temp_dir, "test_filter2.txt")
  
  # Split test data
  lines <- readLines(test_file)
  mid <- ceiling(length(lines) / 2)
  writeLines(lines[1:mid], file1)
  writeLines(lines[(mid+1):length(lines)], file2)
  
  # Process with filtering (empty names)
  result_filtered <- process_bluetooth(c(file1, file2), 
                                      progress_interval = 10000,
                                      exclude_prefixes = c(""))
  
  # Should get same result as single file with same filtering
  result_single_filtered <- process_bluetooth(test_file,
                                             progress_interval = 10000,
                                             exclude_prefixes = c(""))
  
  expect_equal(nrow(result_single_filtered), nrow(result_filtered))
  
  # Clean up
  unlink(c(file1, file2))
})

test_that("multiple files error handling", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Should error if any file doesn't exist
  expect_error(
    process_bluetooth(c(test_file, "nonexistent_xyz.txt")),
    "not found"
  )
})

test_that("metadata attributes correct for multiple files", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Create temporary files
  temp_dir <- tempdir()
  file1 <- file.path(temp_dir, "test_meta1.txt")
  file2 <- file.path(temp_dir, "test_meta2.txt")
  
  lines <- readLines(test_file)
  mid <- ceiling(length(lines) / 2)
  writeLines(lines[1:mid], file1)
  writeLines(lines[(mid+1):length(lines)], file2)
  
  # Process multiple files
  result <- process_bluetooth(c(file1, file2), progress_interval = 10000)
  
  # Check metadata attributes exist
  expect_true(!is.null(attr(result, "file_name")))
  expect_true(!is.null(attr(result, "file_size")))
  expect_true(!is.null(attr(result, "processing_time")))
  expect_true(!is.null(attr(result, "total_lines")))
  expect_true(!is.null(attr(result, "lines_filtered")))
  expect_true(!is.null(attr(result, "unique_combinations")))
  expect_true(!is.null(attr(result, "device_names")))
  
  # Check file_name indicates multiple files
  expect_match(attr(result, "file_name"), "files")
  
  # Check total_lines is sum of both files
  expect_equal(attr(result, "total_lines"), length(lines))
  
  # Clean up
  unlink(c(file1, file2))
})

test_that("performance benchmark", {
  skip_on_cran()
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  # Benchmark processing time (use high interval to minimize output overhead)
  benchmark <- system.time(
    result <- process_bluetooth(test_file, progress_interval = 100000)
  )
  
  # Get number of lines processed
  total_lines <- attr(result, "total_lines")
  
  # Calculate throughput
  lines_per_sec <- total_lines / benchmark["elapsed"]

  # Throughput is hardware- and build-dependent, so do not assert an absolute
  # speed (that makes the test flaky). Assert only that processing completed and
  # produced output, and report the measured speed for monitoring.
  expect_gt(total_lines, 0)
  expect_gt(nrow(result), 0)

  # Print performance info for monitoring
  message(sprintf("Performance: %.0f lines/sec, %.3f sec total",
                  lines_per_sec, benchmark["elapsed"]))
})
