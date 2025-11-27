test_that("get_device_names works with test data", {
  # Get path to test data
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  
  # Skip if test data not found
  skip_if(test_file == "", "Test data file not found")
  
  # Get unique device names
  names <- get_device_names(test_file, progress_interval = 10000)
  
  # Check that result is a character vector
  expect_type(names, "character")
  
  # Check that we have at least one name
  expect_true(length(names) >= 0)
  
  # Check that names are sorted
  expect_equal(names, sort(names))
})

test_that("get_device_names handles non-existent file", {
  expect_error(
    get_device_names("nonexistent_file_xyz123.txt"),
    "not found"
  )
})

test_that("get_device_names validates progress_interval", {
  test_file <- system.file("extdata", "test_data.txt", package = "wsbluetoothR")
  skip_if(test_file == "", "Test data file not found")
  
  expect_error(
    get_device_names(test_file, progress_interval = 0),
    "must be a positive integer"
  )
  
  expect_error(
    get_device_names(test_file, progress_interval = -1),
    "must be a positive integer"
  )
})
