#' Track Address Paths Through Devices
#'
#' For each address, creates a chronological path showing which devices detected
#' the address over time. Returns the top N addresses by detection count, showing
#' how they moved between devices.
#'
#' @param files Character vector of file paths to process. Can be a single file or multiple files.
#' @param top_n Integer. Number of top addresses to return (by detection count). Default is 10.
#' @param progress_interval Integer. How often to print progress messages (0 = no progress). Default is 10000.
#' @param verbose Logical. Whether to print progress messages. Default is TRUE.
#' @param devices Character vector. Filter by specific device IDs. NULL = all devices. Default is NULL.
#' @param min_date Character or Date. Minimum date to include (YYYY-MM-DD or Date object). NULL = no minimum. Default is NULL.
#' @param max_date Character or Date. Maximum date to include (YYYY-MM-DD or Date object). NULL = no maximum. Default is NULL.
#' @param include_names Character vector. Only include records whose name starts with one of these prefixes. NULL = include all. Default is NULL.
#' @param exclude_names Character vector. Exclude records whose name starts with one of these prefixes (e.g. \code{device_category_prefixes()}). NULL = exclude none. Default is NULL.
#' @param exclude_addresses Character vector. Exclude these addresses entirely (all packets, named or not), e.g. the addresses from \code{\link{classify_addresses}}. NULL = exclude none. Default is NULL.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{address}{Bluetooth MAC address}
#'     \item{detection_count}{Total number of detections}
#'     \item{device_count}{Number of unique devices that detected this address}
#'     \item{first_seen}{First detection timestamp (YYYYMMDD-HHMMSS)}
#'     \item{last_seen}{Last detection timestamp (YYYYMMDD-HHMMSS)}
#'     \item{path}{Chronological path through devices (e.g., "16 -> 18 -> 16 -> 21")}
#'   }
#'
#' @details
#' This function tracks how addresses move through the sensor network over time.
#' The path shows device transitions - consecutive detections on the same device
#' are collapsed into a single entry in the path.
#' 
#' For example, if an address is detected on device 16 (100 times), then device 18 
#' (50 times), then back to device 16 (75 times), the path will be "16 -> 18 -> 16".
#' 
#' The function returns the top N addresses by total detection count, which helps
#' identify the most frequently detected devices and their movement patterns.
#'
#' @examples
#' \dontrun{
#' # Get top 10 addresses
#' paths <- get_address_paths("data/combined_sort.txt")
#' 
#' # Get top 50 addresses
#' paths <- get_address_paths("data.txt", top_n = 50)
#' 
#' # Filter by devices
#' paths <- get_address_paths("data.txt", devices = c("16", "18", "21"), top_n = 20)
#' 
#' # Filter by date range
#' paths <- get_address_paths("data.txt", 
#'                           min_date = "2025-08-01",
#'                           max_date = "2025-08-31",
#'                           top_n = 100)
#' 
#' # Analyze paths
#' library(dplyr)
#' paths %>%
#'   arrange(desc(device_count)) %>%  # Most mobile addresses
#'   head(10)
#' }
#'
#' @export
get_address_paths <- function(files, top_n = 10, progress_interval = 10000, verbose = TRUE,
                              devices = NULL, min_date = NULL, max_date = NULL,
                              include_names = NULL, exclude_names = NULL,
                              exclude_addresses = NULL) {
  
  # Validate input
  if (!is.character(files) || length(files) == 0) {
    stop("files must be a non-empty character vector of file paths")
  }
  
  # Check that files exist
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop("File(s) not found: ", paste(missing_files, collapse = ", "))
  }
  
  # Validate top_n
  if (!is.numeric(top_n) || top_n < 1) {
    stop("top_n must be a positive integer")
  }
  top_n <- as.integer(top_n)
  
  # Process filter parameters
  device_filter <- if (is.null(devices)) character(0) else as.character(devices)
  
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
  
  # Name/address filter parameters
  include_list <- if (is.null(include_names)) NULL else as.character(include_names)
  exclude_list <- if (is.null(exclude_names)) NULL else as.character(exclude_names)
  exclude_addr <- if (is.null(exclude_addresses)) NULL else as.character(exclude_addresses)

  # Call C++ function
  result <- calculate_address_paths(files, top_n, progress_interval,
                                   device_filter, min_date_str, max_date_str,
                                   include_list, exclude_list, exclude_addr)
  
  if (verbose && nrow(result) > 0) {
    cat("\nAddress Paths Summary:\n")
    cat("  Addresses returned:", nrow(result), "\n")
    cat("  Total detections:", sum(result$detection_count), "\n")
    cat("  Detection count range:", min(result$detection_count), "to", 
        max(result$detection_count), "\n")
    cat("  Device count range:", min(result$device_count), "to", 
        max(result$device_count), "devices per address\n")
  }
  
  return(result)
}
