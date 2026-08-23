#' Plot Duration Distribution
#'
#' Creates a histogram showing the distribution of detection durations.
#'
#' @param duration_data A data.frame from \code{get_address_duration()}.
#' @param time_unit Character. Unit for x-axis: "seconds", "minutes", or "hours". Default is "hours".
#' @param device Character. Optional device ID to filter by. If NULL, shows all devices.
#' @param bins Integer. Number of bins for histogram. Default is 30.
#'
#' @return A base R histogram showing duration distribution.
#'
#' @examples
#' \dontrun{
#' durations <- get_address_duration("data/combined_sort.txt")
#' plot_duration_distribution(durations)
#' plot_duration_distribution(durations, time_unit = "minutes", device = "16")
#' }
#'
#' @importFrom graphics hist
#' @export
plot_duration_distribution <- function(duration_data, time_unit = "hours", device = NULL, bins = 30) {
  
  # Validate input
  if (!inherits(duration_data, "data.frame")) {
    stop("duration_data must be a data.frame")
  }
  
  # Filter by device if specified
  if (!is.null(device)) {
    duration_data <- duration_data[duration_data$device == device, ]
    if (nrow(duration_data) == 0) {
      stop("No data found for device: ", device)
    }
  }
  
  # Select time column and convert if needed
  values <- switch(time_unit,
    "seconds" = duration_data$duration_seconds,
    "minutes" = duration_data$duration_seconds / 60,
    "hours" = duration_data$duration_seconds / 3600,
    stop("time_unit must be one of: seconds, minutes, hours")
  )
  
  # Create histogram
  title <- if (is.null(device)) {
    paste("Duration Distribution (All Devices)")
  } else {
    paste("Duration Distribution - Device", device)
  }
  
  hist(values, breaks = bins,
       col = "steelblue", border = "white",
       main = title,
       xlab = paste("Duration (", time_unit, ")", sep = ""),
       ylab = "Frequency")
  
  # Add summary statistics
  legend("topright",
         legend = c(
           paste("Mean:", round(mean(values), 2)),
           paste("Median:", round(median(values), 2)),
           paste("Max:", round(max(values), 2)),
           paste("N:", length(values))
         ),
         bty = "n")
}

#' Plot Duration Timeline
#'
#' Creates a time series plot showing detection durations over time.
#'
#' @param duration_data A data.frame from \code{get_address_duration()}.
#' @param time_unit Character. Unit for y-axis: "seconds", "minutes", or "hours". Default is "hours".
#' @param device Character. Optional device ID to filter by. If NULL, shows all devices with colors.
#' @param aggregate_by Character. Aggregate by "day" (mean per day) or "none" (scatter). Default is "day".
#'
#' @return A base R plot showing durations over time.
#'
#' @examples
#' \dontrun{
#' durations <- get_address_duration("data/combined_sort.txt")
#' plot_duration_timeline(durations)
#' plot_duration_timeline(durations, device = "16", aggregate_by = "none")
#' }
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics points
#' @export
plot_duration_timeline <- function(duration_data, time_unit = "hours", device = NULL, aggregate_by = "day") {
  
  # Validate input
  if (!inherits(duration_data, "data.frame")) {
    stop("duration_data must be a data.frame")
  }
  
  # Filter by device if specified
  if (!is.null(device)) {
    duration_data <- duration_data[duration_data$device == device, ]
    if (nrow(duration_data) == 0) {
      stop("No data found for device: ", device)
    }
  }
  
  # Select time column and convert if needed
  if (time_unit == "minutes") {
    duration_data$plot_value <- duration_data$duration_seconds / 60
  } else if (time_unit == "hours") {
    duration_data$plot_value <- duration_data$duration_seconds / 3600
  } else {
    duration_data$plot_value <- duration_data$duration_seconds
  }
  
  # Aggregate if requested
  if (aggregate_by == "day") {
    if (is.null(device)) {
      # Aggregate by device and date
      agg_data <- aggregate(
        plot_value ~ device + date,
        data = duration_data,
        FUN = mean
      )
      names(agg_data)[3] <- "value"
      
      # Plot multiple devices
      devices <- sort(unique(agg_data$device))
      colors <- rainbow(length(devices))
      
      # Set up plot
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mar = c(5, 4, 4, 8), xpd = TRUE)
      
      plot(agg_data$date[agg_data$device == devices[1]],
           agg_data$value[agg_data$device == devices[1]],
           type = "l", col = colors[1], lwd = 2,
           ylim = range(agg_data$value),
           xlab = "Date", ylab = paste("Mean Duration (", time_unit, ")", sep = ""),
           main = "Mean Detection Duration by Day")
      
      if (length(devices) > 1) {
        for (i in 2:length(devices)) {
          device_data <- agg_data[agg_data$device == devices[i], ]
          lines(device_data$date, device_data$value, col = colors[i], lwd = 2)
        }
      }
      
      legend("topright", inset = c(-0.25, 0),
             legend = devices, col = colors, lwd = 2,
             title = "Device", bty = "n")
      
    } else {
      # Single device
      agg_data <- aggregate(
        plot_value ~ date,
        data = duration_data,
        FUN = mean
      )
      names(agg_data)[2] <- "value"
      
      plot(agg_data$date, agg_data$value,
           type = "l", col = "steelblue", lwd = 2,
           xlab = "Date", ylab = paste("Mean Duration (", time_unit, ")", sep = ""),
           main = paste("Mean Detection Duration by Day - Device", device))
    }
    
  } else if (aggregate_by == "none") {
    # Scatter plot
    if (is.null(device)) {
      devices <- sort(unique(duration_data$device))
      colors <- rainbow(length(devices))
      
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mar = c(5, 4, 4, 8), xpd = TRUE)
      
      plot(duration_data$date, duration_data$plot_value,
           type = "n",
           xlab = "Date", ylab = paste("Duration (", time_unit, ")", sep = ""),
           main = "Detection Durations")
      
      for (i in 1:length(devices)) {
        device_data <- duration_data[duration_data$device == devices[i], ]
        points(device_data$date, device_data$plot_value, 
               col = colors[i], pch = 20, cex = 0.5)
      }
      
      legend("topright", inset = c(-0.25, 0),
             legend = devices, col = colors, pch = 20,
             title = "Device", bty = "n")
      
    } else {
      plot(duration_data$date, duration_data$plot_value,
           col = "steelblue", pch = 20, cex = 0.8,
           xlab = "Date", ylab = paste("Duration (", time_unit, ")", sep = ""),
           main = paste("Detection Durations - Device", device))
    }
    
  } else {
    stop("aggregate_by must be 'day' or 'none'")
  }
}

#' Plot Duration Heatmap by Device and Date
#'
#' Creates a heatmap showing detection durations across devices and dates.
#' Works with both detailed data from \code{get_address_duration()} and 
#' aggregated data from \code{get_average_address_duration()}.
#'
#' @param duration_data A data.frame from \code{get_address_duration()} or \code{get_average_address_duration()}.
#' @param time_unit Character. Unit for durations: "seconds", "minutes", or "hours". Default is "hours".
#' @param stat Character. Statistic to display: "mean", "median", "max", or "count". Default is "mean".
#'   For median aggregation data, "count" shows the number of unique addresses per device-date.
#'
#' @return A base R heatmap showing duration patterns.
#'
#' @examples
#' \dontrun{
#' # With detailed data
#' durations <- get_address_duration("data/combined_sort.txt")
#' plot_duration_heatmap(durations)
#' plot_duration_heatmap(durations, stat = "median")
#' 
#' # With median aggregation data
#' median_dur <- get_average_address_duration("data/combined_sort.txt")
#' plot_duration_heatmap(median_dur)
#' plot_duration_heatmap(median_dur, stat = "count")
#' }
#'
#' @importFrom grDevices colorRampPalette
#' @export
plot_duration_heatmap <- function(duration_data, time_unit = "hours", stat = "mean") {
  
  # Validate input
  if (!inherits(duration_data, "data.frame")) {
    stop("duration_data must be a data.frame")
  }
  
  # Check if this is median aggregation data or detailed data
  is_median_data <- "median_duration_seconds" %in% names(duration_data)
  
  # Track what statistic is actually being displayed for the title
  display_stat <- stat
  
  if (is_median_data) {
    # Data from get_average_address_duration - already aggregated by device and datetime
    # Select the appropriate statistic column
    if (stat == "mean") {
      duration_col <- "mean_duration_seconds"
      display_stat <- "mean"
    } else if (stat == "median") {
      duration_col <- "median_duration_seconds"
      display_stat <- "median"
    } else if (stat == "max") {
      duration_col <- "max_duration_seconds"
      display_stat <- "max"
    } else if (stat == "count") {
      duration_col <- "address_count"
      display_stat <- "address count"
    } else {
      stop("stat must be one of: mean, median, max, count")
    }
    
    # Convert to plot units
    if (stat == "count") {
      agg_data <- data.frame(
        device = duration_data$device,
        date = as.Date(duration_data$datetime),
        value = duration_data[[duration_col]]
      )
    } else {
      conversion <- switch(time_unit,
        "minutes" = 60,
        "hours" = 3600,
        1  # seconds
      )
      agg_data <- data.frame(
        device = duration_data$device,
        date = as.Date(duration_data$datetime),
        value = duration_data[[duration_col]] / conversion
      )
    }
    
  } else {
    # Detailed data from get_address_duration - need to aggregate
    # Select time column and convert if needed
    if (time_unit == "minutes") {
      duration_data$plot_value <- duration_data$duration_seconds / 60
    } else if (time_unit == "hours") {
      duration_data$plot_value <- duration_data$duration_seconds / 3600
    } else {
      duration_data$plot_value <- duration_data$duration_seconds
    }
    
    # Select aggregation function
    agg_fun <- switch(stat,
      "mean" = mean,
      "median" = median,
      "max" = max,
      "count" = length,
      stop("stat must be one of: mean, median, max, count")
    )
    
    if (stat == "count") {
      display_stat <- "detection count"
    }
    
    # Aggregate data
    agg_data <- aggregate(
      plot_value ~ device + date,
      data = duration_data,
      FUN = agg_fun
    )
    names(agg_data)[3] <- "value"
  }
  
  # Create matrix
  devices <- sort(unique(agg_data$device))
  dates <- sort(unique(agg_data$date))
  
  mat <- matrix(NA, nrow = length(devices), ncol = length(dates))
  rownames(mat) <- devices
  colnames(mat) <- as.character(dates)
  
  for (i in 1:nrow(agg_data)) {
    row_idx <- match(agg_data$device[i], devices)
    col_idx <- match(agg_data$date[i], dates)
    mat[row_idx, col_idx] <- agg_data$value[i]
  }
  
  # Create color palette
  colors <- colorRampPalette(c("white", "yellow", "orange", "red"))(100)
  
  # Plot heatmap
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(8, 8, 4, 8), xpd = TRUE)
  
  # Determine which columns to show
  n_cols <- ncol(mat)
  show_labels <- if (n_cols <= 30) {
    TRUE
  } else {
    seq(1, n_cols, by = ceiling(n_cols / 30))
  }
  
  image(1:ncol(mat), 1:nrow(mat), t(mat),
        col = colors,
        xlab = "", ylab = "",
        main = paste("Detection Duration Heatmap (", display_stat, " ", time_unit, ")", sep = ""),
        axes = FALSE)
  
  # Add axes
  axis(2, at = 1:nrow(mat), labels = rownames(mat), las = 1, cex.axis = 0.8)
  
  if (is.logical(show_labels)) {
    axis(1, at = 1:ncol(mat), labels = colnames(mat), las = 2, cex.axis = 0.6)
  } else {
    axis(1, at = show_labels, labels = colnames(mat)[show_labels], las = 2, cex.axis = 0.6)
  }
  
  # Add color scale legend
  legend_vals <- pretty(range(mat, na.rm = TRUE), n = 5)
  legend(x = ncol(mat) + 1, y = nrow(mat) / 2,
         legend = legend_vals,
         fill = colorRampPalette(c("white", "yellow", "orange", "red"))(length(legend_vals)),
         title = paste(display_stat, "\n", time_unit, sep = ""),
         bty = "n",
         xjust = 0,
         yjust = 0.5,
         cex = 0.7)
}

#' Plot Top Addresses by Duration
#'
#' Creates a bar chart showing the addresses with longest total detection durations.
#'
#' @param duration_data A data.frame from \code{get_address_duration()}.
#' @param time_unit Character. Unit for x-axis: "seconds", "minutes", or "hours". Default is "hours".
#' @param device Character. Optional device ID to filter by. If NULL, sums across all devices.
#' @param top_n Integer. Number of top addresses to display. Default is 20.
#'
#' @return A base R bar plot showing top addresses.
#'
#' @examples
#' \dontrun{
#' durations <- get_address_duration("data/combined_sort.txt")
#' plot_top_addresses(durations)
#' plot_top_addresses(durations, device = "16", top_n = 10)
#' }
#'
#' @export
plot_top_addresses <- function(duration_data, time_unit = "hours", device = NULL, top_n = 20) {
  
  # Validate input
  if (!inherits(duration_data, "data.frame")) {
    stop("duration_data must be a data.frame")
  }
  
  # Filter by device if specified
  if (!is.null(device)) {
    duration_data <- duration_data[duration_data$device == device, ]
    if (nrow(duration_data) == 0) {
      stop("No data found for device: ", device)
    }
  }
  
  # Select time column and convert if needed
  if (time_unit == "minutes") {
    duration_data$plot_value <- duration_data$duration_seconds / 60
  } else if (time_unit == "hours") {
    duration_data$plot_value <- duration_data$duration_seconds / 3600
  } else {
    duration_data$plot_value <- duration_data$duration_seconds
  }
  
  # Aggregate by address
  agg_data <- aggregate(
    plot_value ~ address,
    data = duration_data,
    FUN = sum
  )
  names(agg_data)[2] <- "total_duration"
  
  # Sort and take top N
  agg_data <- agg_data[order(-agg_data$total_duration), ]
  if (nrow(agg_data) > top_n) {
    agg_data <- agg_data[1:top_n, ]
  }
  
  # Reverse for plotting (highest at top)
  agg_data <- agg_data[nrow(agg_data):1, ]
  
  # Create bar plot
  title <- if (is.null(device)) {
    paste("Top", nrow(agg_data), "Addresses by Total Duration (All Devices)")
  } else {
    paste("Top", nrow(agg_data), "Addresses by Total Duration - Device", device)
  }
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(5, 12, 4, 2))
  
  barplot(agg_data$total_duration,
          names.arg = agg_data$address,
          horiz = TRUE, las = 1,
          col = "steelblue",
          xlab = paste("Total Duration (", time_unit, ")", sep = ""),
          main = title,
          cex.names = 0.7)
}
