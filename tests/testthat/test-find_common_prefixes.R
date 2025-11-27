test_that("find_common_prefixes works with simple examples", {
  names <- c("Apple iPhone", "Apple iPad", "Samsung Galaxy", "Samsung Note", "Google Pixel")
  
  result <- find_common_prefixes(names, min_length = 3, min_count = 2)
  
  # Should be a data frame
  expect_s3_class(result, "data.frame")
  
  # Should have expected columns
  expect_true(all(c("prefix", "count") %in% names(result)))
  
  # Should find "Apple" and "Samsung" prefixes
  expect_true("Apple" %in% result$prefix)
  expect_true("Samsung" %in% result$prefix)
  
  # Apple should have count of 2
  apple_row <- result[result$prefix == "Apple", ]
  expect_equal(apple_row$count[1], 2)
})

test_that("find_common_prefixes respects min_length", {
  names <- c("AB1", "AB2", "ABC1", "ABC2")
  
  # With min_length = 2, should find "AB"
  result <- find_common_prefixes(names, min_length = 2, min_count = 2)
  expect_true("AB" %in% result$prefix)
  
  # With min_length = 4, should find nothing (no prefixes that long)
  result <- find_common_prefixes(names, min_length = 4, min_count = 2)
  expect_equal(nrow(result), 0)
})

test_that("find_common_prefixes respects min_count", {
  names <- c("Apple iPhone", "Apple iPad", "Google Pixel")
  
  # With min_count = 2, should find "Apple"
  result <- find_common_prefixes(names, min_length = 3, min_count = 2)
  expect_true("Apple" %in% result$prefix)
  
  # With min_count = 3, should find nothing (no prefix appears 3 times)
  result <- find_common_prefixes(names, min_length = 3, min_count = 3)
  expect_equal(nrow(result), 0)
})

test_that("find_common_prefixes handles empty input", {
  result <- find_common_prefixes(character(0))
  expect_equal(nrow(result), 0)
  expect_true(all(c("prefix", "count") %in% names(result)))
})

test_that("find_common_prefixes handles empty strings", {
  names <- c("", "", "Apple", "Apple iPhone")
  
  result <- find_common_prefixes(names, min_length = 3, min_count = 2)
  
  # Should find "Apple"
  expect_true("Apple" %in% result$prefix)
})

test_that("find_common_prefixes validates inputs", {
  expect_error(
    find_common_prefixes(123),
    "must be a character vector"
  )
  
  expect_error(
    find_common_prefixes(c("test"), min_length = 0),
    "min_length must be at least 1"
  )
  
  expect_error(
    find_common_prefixes(c("test"), min_count = 0),
    "min_count must be at least 1"
  )
  
  expect_error(
    find_common_prefixes(c("test"), stop_char = c("a", "b")),
    "must be a single character string"
  )
})

test_that("find_common_prefixes handles stop_char correctly", {
  names <- c("Apple iPhone", "Apple iPad", "Samsung Galaxy", "Samsung Note")
  
  # With stop_char = " ", should find only "Apple" and "Samsung" (before space)
  result <- find_common_prefixes(names, min_length = 3, min_count = 2, stop_char = " ")
  
  # Should find "Apple" and "Samsung"
  expect_true("Apple" %in% result$prefix)
  expect_true("Samsung" %in% result$prefix)
  
  # Should NOT find longer prefixes that include the space
  expect_false("Apple iPhone" %in% result$prefix)
  expect_false("Apple iPad" %in% result$prefix)
})

test_that("find_common_prefixes sorts results correctly", {
  names <- c("A1", "A2", "A3", "BB1", "BB2", "CCC1", "CCC2")
  
  result <- find_common_prefixes(names, min_length = 1, min_count = 2)
  
  # First row should have highest count
  expect_true(result$count[1] >= result$count[nrow(result)])
  
  # For same counts, longer prefixes should come first
  if (nrow(result) > 1) {
    for (i in 1:(nrow(result) - 1)) {
      if (result$count[i] == result$count[i + 1]) {
        expect_true(nchar(result$prefix[i]) >= nchar(result$prefix[i + 1]))
      }
    }
  }
})


