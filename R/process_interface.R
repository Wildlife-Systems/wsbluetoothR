#' Process Bluetooth Data Using Rcpp
#'
#' High-performance processing of Bluetooth RSSI data files using C++ implementation.
#' Counts unique device-datetime combinations from input data.
#'
#' @param input_file Character string. Path to input data file with format:
#'   device datetime address power name
#' @param progress_interval Integer. Report progress every N lines (0 = no progress).
#'   Default is 1000.
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
#' }
#'
#' @export
process_bluetooth <- function(input_file = "data/combined_sort.txt", 
                              progress_interval = 1000) {
  
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop(paste("Error: Input file", input_file, "not found."))
  }
  
  # Call Rcpp function
  cat("Processing Bluetooth data with Rcpp...\n")
  result <- process_bluetooth_data(input_file, as.integer(progress_interval))
  
  # Result is already a data.frame from Rcpp
  # Convert datetime to POSIXct
  result$datetime <- as.POSIXct(result$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  cat("Processing complete:", nrow(result), "unique device-datetime combinations\n")
  
  return(result)
}
