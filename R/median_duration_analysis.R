#' Calculate Average Address Duration per Device and Time Period
#'
#' Calculates statistics (median, mean, etc.) for address durations across all Bluetooth addresses
#' detected by each device grouped by time period (hour or day). Unlike \code{get_address_duration()},
#' which returns one row per device-date-address combination, this function aggregates
#' across all addresses and returns the median duration for each device-time period.
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
#' @param time_group Character. Time grouping: "day" or "hour". Default is "day".
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{device}{Device ID}
#'     \item{datetime}{Time period as POSIXct (for hour) or Date (for day)}
#'     \item{median_duration_seconds}{Median duration in seconds across all addresses}
#'     \item{mean_duration_seconds}{Mean duration in seconds across all addresses}
#'     \item{min_duration_seconds}{Minimum duration in seconds}
#'     \item{max_duration_seconds}{Maximum duration in seconds}
#'     \item{address_count}{Number of unique addresses detected}
#'     \item{total_detections}{Total number of detections across all addresses}
#'   }
#'
#' @details
#' This function first calculates daily durations using \code{get_address_duration()},
#' then aggregates by device and time period to compute median and other summary statistics
#' across all addresses. This is useful for understanding typical detection patterns
#' per device over time.
#' 
#' When \code{time_group = "hour"}, results are grouped by device and hour (e.g., all detections
#' on device 16 between 14:00 and 14:59 on 2025-08-15).
#' 
#' When \code{time_group = "day"}, results are grouped by device and day (e.g., all detections
#' on device 16 on 2025-08-15).
#'
#' @examples
#' \dontrun{
#' # Daily grouping (default)
#' median_durations <- get_average_address_duration("data/combined_sort.txt")
#' 
#' # Hourly grouping
#' hourly_medians <- get_average_address_duration("data.txt", time_group = "hour")
#' 
#' # Filter by device
#' median_durations <- get_average_address_duration("data.txt", devices = "16")
#' 
#' # Filter by date range with hourly grouping
#' median_durations <- get_average_address_duration("data.txt", 
#'                                                  min_date = "2025-08-01", 
#'                                                  max_date = "2025-08-31",
#'                                                  time_group = "hour")
#' 
#' # Find time periods with highest median duration
#' library(dplyr)
#' median_durations %>%
#'   arrange(desc(median_duration_seconds)) %>%
#'   head(10)
#' }
#'
#' @export
get_average_address_duration <- function(files, progress_interval = 10000, verbose = TRUE,
                                        devices = NULL, min_date = NULL, max_date = NULL,
                                        include_names = NULL, exclude_names = NULL,
                                        low_memory = FALSE, time_group = "day") {
  
  # Validate input
  if (!is.character(files) || length(files) == 0) {
    stop("files must be a non-empty character vector of file paths")
  }
  
  # Validate time_group
  if (!time_group %in% c("day", "hour")) {
    stop("time_group must be either 'day' or 'hour'")
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
  
  # Call C++ function
  result <- calculate_average_address_duration(files, progress_interval, 
                                             device_filter, min_date_str, max_date_str,
                                             include_list, exclude_list, low_memory, time_group)
  
  # Convert datetime column based on time_group
  if (time_group == "hour") {
    # Format: YYYY-MM-DD-HH -> POSIXct
    result$datetime <- as.POSIXct(paste0(result$datetime, ":00:00"), 
                                  format = "%Y-%m-%d-%H:%M:%S", tz = "UTC")
  } else {
    # Format: YYYY-MM-DD -> Date
    result$datetime <- as.Date(result$datetime)
  }
  
  # Sort by device and datetime
  result <- result[order(result$device, result$datetime), ]
  rownames(result) <- NULL
  
  if (verbose) {
    cat("\nMedian Duration Summary (time_group =", time_group, "):\n")
    cat("  Total device-time period combinations:", nrow(result), "\n")
    cat("  Devices:", length(unique(result$device)), "\n")
    cat("  Time range:", as.character(min(result$datetime)), "to", as.character(max(result$datetime)), "\n")
    if (nrow(result) > 0) {
      cat("  Median duration range:", round(min(result$median_duration_seconds), 2), "to", 
          round(max(result$median_duration_seconds), 2), "seconds\n")
      cat("  Address count range:", min(result$address_count), "to", max(result$address_count), "per time period\n")
    }
  }
  
  return(result)
}
