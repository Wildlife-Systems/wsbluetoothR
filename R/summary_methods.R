#' Summary Method for Bluetooth Data
#'
#' Provides a summary of Bluetooth data processing results.
#'
#' @param object A bluetooth_data object from \code{process_bluetooth()}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the object. Prints summary to console.
#' @importFrom stats aggregate median
#' @export
summary.bluetooth_data <- function(object, ...) {
  cat("\n=== Bluetooth Data Summary ===\n\n")
  
  # File information
  if (!is.null(attr(object, "file_name"))) {
    cat("File:", attr(object, "file_name"), "\n")
  }
  if (!is.null(attr(object, "file_size"))) {
    file_size_mb <- round(attr(object, "file_size") / 1024^2, 2)
    cat("File size:", file_size_mb, "MB\n")
  }
  
  # Processing statistics
  if (!is.null(attr(object, "total_lines"))) {
    cat("\nProcessing Statistics:\n")
    cat("  Total lines processed:", attr(object, "total_lines"), "\n")
  }
  if (!is.null(attr(object, "lines_filtered"))) {
    lines_filtered <- attr(object, "lines_filtered")
    total_lines <- attr(object, "total_lines")
    if (!is.null(total_lines) && total_lines > 0) {
      pct_filtered <- round(100 * lines_filtered / total_lines, 2)
      cat("  Lines filtered out:", lines_filtered, 
          paste0("(", pct_filtered, "%)"), "\n")
    } else {
      cat("  Lines filtered out:", lines_filtered, "\n")
    }
  }
  if (!is.null(attr(object, "unique_combinations"))) {
    cat("  Unique device-datetime combinations:", attr(object, "unique_combinations"), "\n")
  }
  if (!is.null(attr(object, "processing_time"))) {
    cat("  Processing time:", round(attr(object, "processing_time"), 2), "seconds\n")
  }
  
  # Data summary
  cat("\nData Summary:\n")
  cat("  Number of rows:", nrow(object), "\n")
  
  # Device summary
  devices <- unique(object$device)
  cat("  Unique devices:", length(devices), "\n")
  if (length(devices) <= 10) {
    cat("    Devices:", paste(devices, collapse = ", "), "\n")
  }
  
  # Time range
  if ("datetime" %in% names(object) && nrow(object) > 0) {
    time_range <- range(object$datetime, na.rm = TRUE)
    cat("  Time range:", format(time_range[1]), "to", format(time_range[2]), "\n")
    duration <- difftime(time_range[2], time_range[1], units = "days")
    cat("  Duration:", round(as.numeric(duration), 2), "days\n")
  }
  
  # Detection summary
  if ("count" %in% names(object)) {
    cat("\nDetection Counts:\n")
    cat("  Total detections:", sum(object$count), "\n")
    cat("  Mean detections per combination:", round(mean(object$count), 2), "\n")
    cat("  Median detections:", median(object$count), "\n")
    cat("  Range:", min(object$count), "-", max(object$count), "\n")
  }
  
  cat("\n")
  invisible(object)
}

#' Print Method for Bluetooth Data
#'
#' @param x A bluetooth_data object.
#' @param ... Additional arguments passed to print.data.frame.
#'
#' @export
print.bluetooth_data <- function(x, ...) {
  cat("Bluetooth Data:", nrow(x), "rows\n")
  if (!is.null(attr(x, "file_name"))) {
    cat("Source:", attr(x, "file_name"), "\n")
  }
  cat("\n")
  NextMethod("print")
}
