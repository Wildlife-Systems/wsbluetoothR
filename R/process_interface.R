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

#' Summarize Device Detection Counts
#'
#' Generate summary statistics for each device across all timestamps.
#'
#' @param data A data.frame from \code{\link{process_bluetooth}}.
#'
#' @return A data.frame with summary statistics by device.
#' @export
summarize_device_counts <- function(data) {
  
  devices <- unique(data$device)
  summary_stats <- do.call(rbind, lapply(devices, function(dev) {
    dev_data <- data[data$device == dev, ]
    data.frame(
      device = dev,
      total_detections = sum(dev_data$count),
      unique_timestamps = nrow(dev_data),
      mean_count = mean(dev_data$count),
      median_count = median(dev_data$count),
      max_count = max(dev_data$count),
      min_count = min(dev_data$count),
      stringsAsFactors = FALSE
    )
  }))
  
  # Sort by total detections
  summary_stats <- summary_stats[order(-summary_stats$total_detections), ]
  rownames(summary_stats) <- NULL
  
  return(summary_stats)
}

#' Get Time Series for Specific Device
#'
#' Extract and sort time-series data for a specific device.
#'
#' @param data A data.frame from \code{\link{process_bluetooth}}.
#' @param device_name Character string. Device name to extract.
#'
#' @return A data.frame with time-series data for the device, or NULL if not found.
#' @export
get_device_timeseries <- function(data, device_name) {
  
  device_data <- data[data$device == device_name, ]
  
  if (nrow(device_data) == 0) {
    warning(paste("No data found for device:", device_name))
    return(NULL)
  }
  
  # Sort by datetime
  device_data <- device_data[order(device_data$datetime), ]
  rownames(device_data) <- NULL
  
  return(device_data)
}

#' Plot Device Detection Counts Over Time
#'
#' Creates a time-series plot of detection counts for one or all devices.
#'
#' @param data A data.frame from \code{\link{process_bluetooth}}.
#' @param device_name Character string. Device to plot (NULL = all devices).
#' @param main_title Character string. Plot title.
#'
#' @return NULL (creates plot).
#' @export
plot_device_counts <- function(data, device_name = NULL, 
                              main_title = "Device Detection Counts") {
  
  if (!is.null(device_name)) {
    plot_data <- data[data$device == device_name, ]
    if (nrow(plot_data) == 0) {
      stop(paste("No data found for device:", device_name))
    }
    title <- paste(main_title, "-", device_name)
  } else {
    # Plot all devices
    plot_data <- data
    title <- main_title
  }
  
  # Sort by datetime
  plot_data <- plot_data[order(plot_data$datetime), ]
  
  # Create plot
  plot(plot_data$datetime, plot_data$count,
       type = "l", col = "blue",
       xlab = "Time", ylab = "Detection Count",
       main = title)
  
  grid()
}

#' Compare Multiple Devices
#'
#' Creates a comparison plot showing detection counts for multiple devices.
#'
#' @param data A data.frame from \code{\link{process_bluetooth}}.
#' @param device_names Character vector. Device names to compare.
#'
#' @return NULL (creates plot).
#' @export
compare_devices <- function(data, device_names) {
  
  comparison_data <- data[data$device %in% device_names, ]
  
  if (nrow(comparison_data) == 0) {
    stop("No data found for specified devices")
  }
  
  # Create comparison plot
  colors <- rainbow(length(device_names))
  
  plot(NULL, xlim = range(comparison_data$datetime, na.rm = TRUE),
       ylim = range(comparison_data$count, na.rm = TRUE),
       xlab = "Time", ylab = "Detection Count",
       main = "Device Comparison")
  
  for (i in seq_along(device_names)) {
    device_data <- comparison_data[comparison_data$device == device_names[i], ]
    device_data <- device_data[order(device_data$datetime), ]
    lines(device_data$datetime, device_data$count, 
          col = colors[i], lwd = 2)
  }
  
  legend("topright", legend = device_names, 
         col = colors, lwd = 2, cex = 0.8)
  
  grid()
}
