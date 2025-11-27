#' Get Unique Device Names from Bluetooth Data File
#'
#' Scans a Bluetooth data file and returns a sorted vector of unique device names.
#' This is useful for determining what device name prefixes to use for filtering.
#'
#' @param input_file Character string. Path to input data file with format:
#'   device datetime address power name
#' @param progress_interval Integer. Report progress every N lines (must be positive).
#'   Default is 10000.
#'
#' @return A character vector of unique device names found in the file, sorted alphabetically.
#'   Empty names are included as empty strings.
#'
#' @examples
#' \dontrun{
#' # Get all unique device names
#' names <- get_device_names("data/combined_sort.txt")
#' 
#' # See what names are present
#' head(names)
#' 
#' # Get names with less frequent progress updates
#' names <- get_device_names("data/combined_sort.txt", progress_interval = 50000)
#' }
#'
#' @export
get_device_names <- function(input_file, progress_interval = 10000) {
  
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop(paste("Error: Input file", input_file, "not found."))
  }
  
  # Validate progress_interval
  if (!is.null(progress_interval) && progress_interval <= 0) {
    stop("Error: progress_interval must be a positive integer.")
  }
  
  # Call Rcpp function
  cat("Scanning for unique device names...\n")
  result <- get_unique_device_names(input_file, as.integer(progress_interval))
  
  return(result)
}
