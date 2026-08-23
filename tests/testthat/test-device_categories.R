cat_file <- function() {
  f <- system.file("extdata", "clean_test_data.txt", package = "wsbluetoothR")
  skip_if(f == "", "clean_test_data.txt not found")
  f
}

# ---- device_category_prefixes -----------------------------------------------

test_that("device_category_prefixes returns default bike/vehicle/beacon prefixes", {
  p <- device_category_prefixes()
  expect_type(p, "character")
  expect_true(all(c("lime-", "BYD", "[TV]") %in% p))
  # wearable/audio are not in the default set
  expect_false("WHOOP" %in% p)
})

test_that("device_category_prefixes(categories = NULL) returns all categories", {
  p <- device_category_prefixes(categories = NULL)
  expect_true("WHOOP" %in% p)   # wearable
  expect_true("JBL" %in% p)     # audio
})

test_that("device_category_prefixes(as_table = TRUE) returns a labelled table", {
  tab <- device_category_prefixes(as_table = TRUE)
  expect_s3_class(tab, "data.frame")
  expect_true(all(c("prefix", "category") %in% names(tab)))
  expect_true(all(tab$category %in% c("bike", "vehicle", "beacon")))
})

test_that("device_category_prefixes errors on unknown category", {
  expect_error(device_category_prefixes(categories = "spaceship"), "Unknown categ")
})

# ---- classify_addresses -----------------------------------------------------

test_that("classify_addresses labels bikes, vehicles and beacons by name", {
  classified <- classify_addresses(cat_file(), verbose = FALSE)

  expect_s3_class(classified, "data.frame")
  expect_true(all(c("address", "category", "matched_prefix", "hits") %in% names(classified)))

  # aa = lime bike, bb = BYD vehicle, cc = [TV] beacon
  expect_setequal(classified$address,
                  c("aa:aa:aa:aa:aa:aa", "bb:bb:bb:bb:bb:bb", "cc:cc:cc:cc:cc:cc"))

  cat_of <- stats::setNames(classified$category, classified$address)
  expect_equal(cat_of[["aa:aa:aa:aa:aa:aa"]], "bike")
  expect_equal(cat_of[["bb:bb:bb:bb:bb:bb"]], "vehicle")
  expect_equal(cat_of[["cc:cc:cc:cc:cc:cc"]], "beacon")

  # ee only advertised WHOOP (wearable) -> not in the default category set
  expect_false("ee:ee:ee:ee:ee:ee" %in% classified$address)
})

test_that("classify_addresses picks up wearables when asked", {
  classified <- classify_addresses(cat_file(), categories = NULL, verbose = FALSE)
  expect_true("ee:ee:ee:ee:ee:ee" %in% classified$address)
  cat_of <- stats::setNames(classified$category, classified$address)
  expect_equal(cat_of[["ee:ee:ee:ee:ee:ee"]], "wearable")
})

test_that("classify_addresses accepts a plain character vector of prefixes", {
  classified <- classify_addresses(cat_file(), patterns = "lime-", verbose = FALSE)
  expect_equal(classified$address, "aa:aa:aa:aa:aa:aa")
  expect_true(is.na(classified$category))
})

# ---- name filtering in get_address_paths ------------------------------------

test_that("get_address_paths gains exclude_names filtering", {
  all_paths  <- get_address_paths(cat_file(), top_n = 100, verbose = FALSE)
  no_lime    <- get_address_paths(cat_file(), top_n = 100, verbose = FALSE,
                                  exclude_names = "lime-")

  expect_true("aa:aa:aa:aa:aa:aa" %in% all_paths$address)
  expect_false("aa:aa:aa:aa:aa:aa" %in% no_lime$address)
})

test_that("get_address_paths exclude_addresses drops all packets of a device", {
  all_paths <- get_address_paths(cat_file(), top_n = 100, verbose = FALSE)
  expect_true("ee:ee:ee:ee:ee:ee" %in% all_paths$address)

  cleaned <- get_address_paths(cat_file(), top_n = 100, verbose = FALSE,
                               exclude_addresses = "ee:ee:ee:ee:ee:ee")
  expect_false("ee:ee:ee:ee:ee:ee" %in% cleaned$address)
})

# ---- address promotion closes the nameless-packet gap -----------------------

test_that("exclude_addresses removes nameless packets that a name filter leaves", {
  # ee is detected 3 times: once named "WHOOP ...", twice with no name.
  base <- get_address_duration(cat_file(), verbose = FALSE)
  ee_rows_base <- base[base$address == "ee:ee:ee:ee:ee:ee", ]
  expect_gt(nrow(ee_rows_base), 0)

  # A per-packet name filter only removes the *named* packet, leaving the
  # nameless detections behind (ee still present).
  by_name <- get_address_duration(cat_file(), verbose = FALSE,
                                  exclude_names = "WHOOP")
  expect_true("ee:ee:ee:ee:ee:ee" %in% by_name$address)

  # Promoting the name hit to an address removes ee entirely.
  classified <- classify_addresses(cat_file(), categories = "wearable", verbose = FALSE)
  by_addr <- get_address_duration(cat_file(), verbose = FALSE,
                                  exclude_addresses = classified$address)
  expect_false("ee:ee:ee:ee:ee:ee" %in% by_addr$address)
})

# ---- remove_addresses -------------------------------------------------------

test_that("remove_addresses filters an address-keyed result", {
  dur        <- get_address_duration(cat_file(), verbose = FALSE)
  classified <- classify_addresses(cat_file(), verbose = FALSE)

  cleaned <- remove_addresses(dur, classified)
  expect_false(any(classified$address %in% cleaned$address))
  # dd and ff (people) survive
  expect_true("dd:dd:dd:dd:dd:dd" %in% cleaned$address)

  # also accepts a bare character vector
  cleaned2 <- remove_addresses(dur, classified$address)
  expect_equal(nrow(cleaned), nrow(cleaned2))
})

test_that("remove_addresses validates input", {
  expect_error(remove_addresses(data.frame(x = 1), "aa"),
               "must be a data.frame with an 'address' column")
})

# ---- process_bluetooth exclude_addresses ------------------------------------

test_that("process_bluetooth exclude_addresses reduces detection counts", {
  full    <- process_bluetooth(cat_file(), verbose = FALSE)
  reduced <- process_bluetooth(cat_file(), verbose = FALSE,
                               exclude_addresses = "cc:cc:cc:cc:cc:cc")
  # cc contributes 3 detections on device 16 at three timestamps
  expect_lt(sum(reduced$count), sum(full$count))
})
