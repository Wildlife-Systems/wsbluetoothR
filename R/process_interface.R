#' Process Bluetooth Data Using Rcpp
#'
#' High-performance processing of Bluetooth RSSI data files using C++ implementation.
#' Counts unique device-datetime combinations from input data with optional name filtering.
#'
#' @param input_file Character string. Path to input data file with format:
#'   device datetime address power name
#' @param progress_interval Integer. Report progress every N lines (must be positive).
#'   Default is 1000.
#' @param include_prefixes Character vector or NULL. If provided, only include records
#'   where the name field starts with one of these prefixes. Default is NULL (no filtering).
#' @param exclude_prefixes Character vector or NULL. If provided, exclude records
#'   where the name field starts with one of these prefixes. Default is NULL (no filtering).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{device}{Character. Device identifier (e.g., "pi1", "pi2")}
#'     \item{datetime}{POSIXct. Timestamp of detection}
#'     \item{count}{Integer. Number of detections for this device-datetime combination}
#'   }
#'
#' @examples
#' \dontrun{
#' # Process Bluetooth data file
#' data <- process_bluetooth("data/combined_sort.txt")
#' 
#' # Process with progress reporting every 5000 lines
#' data <- process_bluetooth("data/combined_sort.txt", progress_interval = 5000)
#' 
#' # Include only names starting with "Apple" or "Samsung"
#' data <- process_bluetooth("data/combined_sort.txt", 
#'                          include_prefixes = c("Apple", "Samsung"))
#' 
#' # Exclude names starting with "Unknown"
#' data <- process_bluetooth("data/combined_sort.txt", 
#'                          exclude_prefixes = c("Unknown"))
#' }
#' @importFrom Rcpp evalCpp
#' @export
process_bluetooth <- function(input_file = "data/combined_sort.txt", 
                              progress_interval = 1000,
                              include_prefixes = NULL,
                              exclude_prefixes = NULL) {
  
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop(paste("Error: Input file", input_file, "not found."))
  }
  
  # Validate progress_interval
  if (!is.null(progress_interval) && progress_interval <= 0) {
    stop("Error: progress_interval must be a positive integer.")
  }
  
  # Call Rcpp function
  cat("Processing Bluetooth data with Rcpp...\n")
  result <- process_bluetooth_data(input_file, 
                                  as.integer(progress_interval),
                                  include_prefixes,
                                  exclude_prefixes)
  
  # Convert datetime strings to POSIXct
  result$datetime <- as.POSIXct(result$datetime, format = "%Y%m%d-%H%M%S", tz = "UTC")
  
  # Remove any rows with NA datetime
  result <- result[!is.na(result$datetime), ]
  
  cat("Processing complete:", nrow(result), "unique device-datetime combinations\n")
  
  return(result)
}
