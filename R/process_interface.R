#' Process Bluetooth Data Using Rcpp
#'
#' High-performance processing of Bluetooth RSSI data files using C++ implementation.
#' Counts unique device-datetime combinations from input data with optional name filtering.
#' Can process single or multiple files.
#'
#' @param input_file Character string or vector. Path(s) to input data file(s) with format:
#'   device datetime address power name. If a vector is provided, all files will be
#'   processed and combined into a single result.
#' @param progress_interval Integer. Report progress every N lines (must be positive).
#'   Default is 1000.
#' @param include_prefixes Character vector or NULL. If provided, only include records
#'   where the name field starts with one of these prefixes. Default is NULL (no filtering).
#' @param exclude_prefixes Character vector or NULL. If provided, exclude records
#'   where the name field starts with one of these prefixes. Default is NULL (no filtering).
#' @param exclude_addresses Character vector or NULL. If provided, exclude records
#'   for these addresses entirely (all packets, named or not), e.g. the addresses
#'   returned by \code{\link{classify_addresses}}. Default is NULL (no filtering).
#' @param verbose Logical. If TRUE (default), print processing messages to console.
#'   Set to FALSE to suppress output.
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
#' # Process single Bluetooth data file
#' data <- process_bluetooth("data/combined_sort.txt")
#' 
#' # Process multiple files
#' data <- process_bluetooth(c("data/file1.txt", "data/file2.txt"))
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
process_bluetooth <- function(input_file,
                              progress_interval = 1000,
                              include_prefixes = NULL,
                              exclude_prefixes = NULL,
                              exclude_addresses = NULL,
                              verbose = TRUE) {
  
  # Ensure input_file is a character vector
  if (!is.character(input_file)) {
    stop("Error: input_file must be a character string or vector.")
  }
  
  # Check all files exist
  for (file in input_file) {
    if (!file.exists(file)) {
      stop(paste("Error: Input file", file, "not found."))
    }
  }
  
  # Validate progress_interval
  if (!is.null(progress_interval) && progress_interval <= 0) {
    stop("Error: progress_interval must be a positive integer.")
  }
  
  # Process files (handles both single and multiple files)
  if (isTRUE(verbose)) {
    message("Processing ", length(input_file), ifelse(length(input_file) == 1, " file", " files"), "...")
  }
  
  overall_start <- Sys.time()
  
  exclude_addr <- if (is.null(exclude_addresses)) NULL else as.character(exclude_addresses)

  result <- process_bluetooth_files(input_file,
                                   as.integer(progress_interval),
                                   include_prefixes,
                                   exclude_prefixes,
                                   exclude_addr)
  
  overall_end <- Sys.time()
  processing_time <- as.numeric(difftime(overall_end, overall_start, units = "secs"))
  
  # Convert datetime strings to POSIXct
  result$datetime <- as.POSIXct(result$datetime, format = "%Y%m%d-%H%M%S", tz = "UTC")
  
  result <- result[!is.na(result$datetime), ]
  
  # Calculate total file size
  total_size <- sum(sapply(input_file, function(f) file.info(f)$size))
  
  # Add metadata
  if (length(input_file) == 1) {
    attr(result, "file_name") <- basename(input_file)
  } else {
    attr(result, "file_name") <- paste(length(input_file), "files")
  }
  attr(result, "file_size") <- total_size
  attr(result, "processing_time") <- processing_time
  
  if (isTRUE(verbose)) {
    message("\nProcessing complete: ", nrow(result), " unique device-datetime combinations")
    message("Total processing time: ", round(processing_time, 2), " seconds")
  }
  
  # Set class for S3 methods
  class(result) <- c("bluetooth_data", "data.frame")
  
  return(result)
}
