#' Calculate Address Duration per Device and Day
#'
#' Calculates the time duration (in seconds) that each Bluetooth address
#' was detected by each device on each day. The duration is calculated as
#' the time difference between the first and last detection of that address
#' on that device on that day.
#'
#' @param files Character vector of file paths to process. Can be a single file or multiple files.
#' @param progress_interval Integer. How often to print progress messages (0 = no progress). Default is 10000.
#' @param verbose Logical. Whether to print progress messages. Default is TRUE.
#' @param devices Character vector. Filter by specific device IDs. NULL = all devices. Default is NULL.
#' @param min_date Character or Date. Minimum date to include (YYYY-MM-DD or Date object). NULL = no minimum. Default is NULL.
#' @param max_date Character or Date. Maximum date to include (YYYY-MM-DD or Date object). NULL = no maximum. Default is NULL.
#' @param include_names Character vector. Only include records where name starts with one of these prefixes. NULL = include all. Default is NULL.
#' @param exclude_names Character vector. Exclude records where name starts with one of these prefixes. NULL = exclude none. Default is NULL.
#' @param low_memory Logical. If TRUE, processes devices in batches to reduce peak RAM at the cost of re-reading the input once per batch (slower but uses less memory). The batch size defaults to 4 devices per pass and can be set via the \code{WSBT_DEVICES_PER_PASS} environment variable (1 = one device at a time). Default is FALSE.
#' @param exclude_addresses Character vector. Exclude these addresses entirely (all packets, named or not), e.g. the addresses from \code{\link{classify_addresses}}. NULL = exclude none. Default is NULL.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{device}{Device ID}
#'     \item{date}{Date in YYYY-MM-DD format}
#'     \item{address}{Bluetooth MAC address}
#'     \item{first_seen}{Timestamp of first detection (YYYYMMDD-HHMMSS)}
#'     \item{last_seen}{Timestamp of last detection (YYYYMMDD-HHMMSS)}
#'     \item{duration_seconds}{Time duration in seconds between first and last detection}
#'     \item{detection_count}{Number of times this address was detected}
#'   }
#'
#' @details
#' This function processes Bluetooth detection data to calculate how long each
#' device was in range of each sensor on each day. The duration is based on the
#' time span between the first and last detection, not the total time detected.
#' 
#' For example, if a device is detected at 10:00:00 and 10:05:00, the duration
#' is 300 seconds (5 minutes), regardless of how many times it was detected
#' in between.
#' 
#' Filters are applied during data reading for efficiency, so filtering by device
#' or date range on large files will be much faster than filtering the results afterwards.
#'
#' @examples
#' \dontrun{
#' # Single file
#' durations <- get_address_duration("data/combined_sort.txt")
#' 
#' # Multiple files
#' durations <- get_address_duration(c("file1.txt", "file2.txt"))
#' 
#' # Filter by device
#' durations <- get_address_duration("data.txt", devices = c("16", "17"))
#' 
#' # Filter by date range
#' durations <- get_address_duration("data.txt", 
#'                                   min_date = "2025-08-01", 
#'                                   max_date = "2025-08-31")
#' 
#' # Combined filters
#' durations <- get_address_duration("data.txt", 
#'                                   devices = "16",
#'                                   min_date = as.Date("2025-08-15"))
#' 
#' # Filter by name prefix (include only certain devices)
#' durations <- get_address_duration("data.txt", 
#'                                   include_names = c("iPhone", "Galaxy"))
#' 
#' # Exclude certain name prefixes
#' durations <- get_address_duration("data.txt", 
#'                                   exclude_names = c("Unknown", "[TV]"))
#' 
#' # Analyze durations
#' summary(durations$duration_seconds)
#' 
#' # Find addresses detected for longest duration on a specific device
#' library(dplyr)
#' durations %>%
#'   filter(device == "16") %>%
#'   arrange(desc(duration_seconds)) %>%
#'   head(10)
#' }
#'
#' @export
get_address_duration <- function(files, progress_interval = 10000, verbose = TRUE,
                                 devices = NULL, min_date = NULL, max_date = NULL,
                                 include_names = NULL, exclude_names = NULL,
                                 low_memory = FALSE, exclude_addresses = NULL) {
  
  # Validate input
  if (!is.character(files) || length(files) == 0) {
    stop("files must be a non-empty character vector of file paths")
  }
  
  # Check that files exist
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop("File(s) not found: ", paste(missing_files, collapse = ", "))
  }
  
  # Process filter parameters
  device_filter <- if (is.null(devices)) character(0) else as.character(devices)
  include_list <- if (is.null(include_names)) character(0) else as.character(include_names)
  exclude_list <- if (is.null(exclude_names)) character(0) else as.character(exclude_names)
  
  # Convert dates to YYYYMMDD format
  min_date_str <- ""
  if (!is.null(min_date)) {
    if (inherits(min_date, "Date")) {
      min_date_str <- format(min_date, "%Y%m%d")
    } else {
      # Assume character in YYYY-MM-DD format
      min_date_str <- gsub("-", "", min_date)
    }
  }
  
  max_date_str <- ""
  if (!is.null(max_date)) {
    if (inherits(max_date, "Date")) {
      max_date_str <- format(max_date, "%Y%m%d")
    } else {
      # Assume character in YYYY-MM-DD format
      max_date_str <- gsub("-", "", max_date)
    }
  }
  
  # Set progress interval based on verbose
  if (!verbose) {
    progress_interval <- 0
  }
  
  exclude_addr <- if (is.null(exclude_addresses)) NULL else as.character(exclude_addresses)

  # Call C++ function
  result <- calculate_address_duration(files, progress_interval,
                                       device_filter, min_date_str, max_date_str,
                                       include_list, exclude_list, low_memory,
                                       exclude_addr)
  
  # Convert date to Date class
  result$date <- as.Date(result$date, format = "%Y-%m-%d")
  
  # Sort by device, date, and duration (descending)
  result <- result[order(result$device, result$date, -result$duration_seconds), ]
  rownames(result) <- NULL
  
  if (verbose) {
    cat("\nSummary:\n")
    cat("  Total records:", nrow(result), "\n")
    cat("  Devices:", length(unique(result$device)), "\n")
    cat("  Addresses:", length(unique(result$address)), "\n")
    cat("  Date range:", min(result$date), "to", max(result$date), "\n")
    cat("  Duration range:", round(min(result$duration_seconds), 2), "to", 
        round(max(result$duration_seconds), 2), "seconds\n")
  }
  
  return(result)
}
