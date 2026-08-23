#' Plot Device Timeline
#'
#' Creates a time series plot showing detection counts over time for each device.
#'
#' @param data A bluetooth_data object or data.frame from \code{process_bluetooth()}.
#' @param devices Character vector. Device names to plot. If NULL (default), plots all devices.
#' @param aggregate Character. Time aggregation level: "hour", "day", "week", or "month". Default is "hour".
#'
#' @return A base R plot showing detection patterns over time.
#'
#' @examples
#' \dontrun{
#' data <- process_bluetooth("data/combined_sort.txt")
#' plot_device_timeline(data)
#' plot_device_timeline(data, devices = c("pi1", "pi2"), aggregate = "day")
#' }
#' @importFrom grDevices rainbow
#' @importFrom graphics axis barplot image legend lines par
#' @export
plot_device_timeline <- function(data, devices = NULL, aggregate = "hour") {
  
  # Validate input
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame or bluetooth_data object")
  }
  
  if (!all(c("device", "datetime", "count") %in% names(data))) {
    stop("data must have columns: device, datetime, count")
  }
  
  # Filter devices if specified
  if (!is.null(devices)) {
    data <- data[data$device %in% devices, ]
    if (nrow(data) == 0) {
      stop("No data found for specified devices")
    }
  }
  
  # Aggregate by time period
  data$time_bin <- switch(aggregate,
    "hour" = as.POSIXct(format(data$datetime, "%Y-%m-%d %H:00:00"), tz = "UTC"),
    "day" = as.Date(data$datetime),
    "week" = as.Date(cut(data$datetime, "week")),
    "month" = as.Date(cut(data$datetime, "month")),
    stop("aggregate must be one of: hour, day, week, month")
  )
  
  # Aggregate counts
  agg_data <- aggregate(count ~ device + time_bin, data = data, FUN = sum)
  
  # Get unique devices and colors
  unique_devices <- unique(agg_data$device)
  n_devices <- length(unique_devices)
  colors <- rainbow(n_devices)
  
  # Set up plot and restore par afterwards to avoid side effects
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(5, 4, 4, 8), xpd = TRUE)
  
  # Find y-axis range
  y_max <- max(agg_data$count)
  
  # Plot first device
  first_device <- unique_devices[1]
  first_data <- agg_data[agg_data$device == first_device, ]
  
  plot(first_data$time_bin, first_data$count,
       type = "l", col = colors[1], lwd = 2,
       ylim = c(0, y_max * 1.1),
       xlab = "Time", ylab = "Detection Count",
       main = paste0("Device Timeline (aggregated by ", aggregate,
                     ", n = ", format(sum(data$count), big.mark = ","), ")"))
  
  # Add other devices
  if (n_devices > 1) {
    for (i in 2:n_devices) {
      device_data <- agg_data[agg_data$device == unique_devices[i], ]
      lines(device_data$time_bin, device_data$count, col = colors[i], lwd = 2)
    }
  }
  
  # Add legend
  legend("topright", inset = c(-0.25, 0),
         legend = unique_devices, col = colors, lwd = 2,
         title = "Device", bty = "n")
  
  par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)
}

#' Plot Prefix Distribution
#'
#' Creates a bar chart showing the most common device name prefixes.
#'
#' @param prefix_data A data.frame from \code{find_common_prefixes()}.
#' @param top_n Integer. Number of top prefixes to display. Default is 20.
#'
#' @return A base R bar plot showing prefix frequencies.
#'
#' @examples
#' \dontrun{
#' names <- get_device_names("data/combined_sort.txt")
#' prefixes <- find_common_prefixes(names, stop_char = " ")
#' plot_prefix_distribution(prefixes)
#' plot_prefix_distribution(prefixes, top_n = 10)
#' }
#'
#' @export
plot_prefix_distribution <- function(prefix_data, top_n = 20) {
  
  # Validate input
  if (!inherits(prefix_data, "data.frame")) {
    stop("prefix_data must be a data.frame")
  }
  
  if (!all(c("prefix", "count") %in% names(prefix_data))) {
    stop("prefix_data must have columns: prefix, count")
  }
  
  # Take top N
  if (nrow(prefix_data) > top_n) {
    prefix_data <- prefix_data[1:top_n, ]
  }
  
  # Reverse order for plotting (top at top)
  prefix_data <- prefix_data[nrow(prefix_data):1, ]
  
  # Create bar plot and restore par afterwards to avoid side effects
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(5, 8, 4, 2))
  barplot(prefix_data$count, names.arg = prefix_data$prefix,
          horiz = TRUE, las = 1,
          col = "steelblue",
          xlab = "Number of Device Names",
          main = paste("Top", nrow(prefix_data), "Device Name Prefixes"))
  par(mar = c(5, 4, 4, 2) + 0.1)
}

#' Plot Detection Heatmap
#'
#' Creates a heatmap showing detection patterns across devices and time periods.
#'
#' @param data A bluetooth_data object or data.frame from \code{process_bluetooth()}.
#' @param time_unit Character. Time aggregation: "hour", "day", "week", or "month". Default is "month".
#' @param log Logical. If TRUE, map colors on a log10 scale (log10(count + 1)), revealing
#'   low-count structure at fine time resolutions (e.g. hourly) that a linear scale washes out.
#'   The legend is still labelled on the real detection-count scale. Default is FALSE (linear).
#'
#' @return A base R heatmap showing device x time detection patterns.
#'
#' @examples
#' \dontrun{
#' data <- process_bluetooth("data/combined_sort.txt")
#' plot_detection_heatmap(data, time_unit = "day")
#' plot_detection_heatmap(data, time_unit = "hour", log = TRUE)
#' }
#' @importFrom grDevices colorRampPalette
#' @export
plot_detection_heatmap <- function(data, time_unit = "month", log = FALSE) {
  
  # Validate input
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame or bluetooth_data object")
  }
  
  if (!all(c("device", "datetime", "count") %in% names(data))) {
    stop("data must have columns: device, datetime, count")
  }
  
  # Create time bins
  data$time_bin <- switch(time_unit,
    "hour" = format(data$datetime, "%Y-%m-%d %H:00"),
    "day" = format(data$datetime, "%Y-%m-%d"),
    "week" = format(as.Date(cut(data$datetime, "week")), "%Y-%m-%d"),
    "month" = format(data$datetime, "%Y-%m"),
    stop("time_unit must be one of: hour, day, week, month")
  )
  
  # Aggregate
  agg_data <- aggregate(count ~ device + time_bin, data = data, FUN = sum)
  
  # Create matrix for heatmap
  devices <- sort(unique(agg_data$device))
  time_bins <- sort(unique(agg_data$time_bin))
  
  mat <- matrix(0, nrow = length(devices), ncol = length(time_bins))
  rownames(mat) <- devices
  colnames(mat) <- time_bins
  
  for (i in 1:nrow(agg_data)) {
    row_idx <- match(agg_data$device[i], devices)
    col_idx <- match(agg_data$time_bin[i], time_bins)
    mat[row_idx, col_idx] <- agg_data$count[i]
  }
  
  # Create color palette
  colors <- colorRampPalette(c("white", "yellow", "orange", "red"))(100)

  # Optional log color scale: map log10(count + 1) so low counts stay visible
  if (isTRUE(log)) {
    z <- log10(mat + 1)
  } else {
    z <- mat
  }
  zmax <- max(z)

  # Plot heatmap with space for legend on right and restore par afterwards
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(8, 8, 4, 8), xpd = TRUE)

  # Determine which columns to show (show every nth if too many)
  n_cols <- ncol(mat)
  show_labels <- if (n_cols <= 24) {
    TRUE
  } else {
    seq(1, n_cols, by = ceiling(n_cols / 24))
  }

  image(1:ncol(mat), 1:nrow(mat), t(z),
        col = colors, zlim = c(0, zmax),
        xlab = "", ylab = "",
        main = paste0("Detection Heatmap by ", time_unit,
                      if (isTRUE(log)) " (log scale, n = " else " (n = ",
                      format(sum(data$count), big.mark = ","), ")"),
        axes = FALSE)
  
  # Add axes
  axis(2, at = 1:nrow(mat), labels = rownames(mat), las = 1, cex.axis = 0.8)
  
  if (is.logical(show_labels)) {
    axis(1, at = 1:ncol(mat), labels = colnames(mat), las = 2, cex.axis = 0.6)
  } else {
    axis(1, at = show_labels, labels = colnames(mat)[show_labels], las = 2, cex.axis = 0.6)
  }
  
  # Add color scale legend to the right of plot (labelled on the real count scale)
  if (isTRUE(log)) {
    maxc <- max(mat)
    legend_vals <- c(0, 1, 10, 100, 1000, 10000, 100000, 1000000)
    legend_vals <- legend_vals[legend_vals <= maxc]
    if (length(legend_vals) == 0 || legend_vals[length(legend_vals)] < maxc) {
      legend_vals <- c(legend_vals, round(maxc))
    }
    legend_fill <- colors[pmax(1, pmin(100, round(log10(legend_vals + 1) / zmax * 99) + 1))]
  } else {
    legend_vals <- pretty(range(mat), n = 5)
    legend_fill <- colorRampPalette(c("white", "yellow", "orange", "red"))(length(legend_vals))
  }
  legend(x = ncol(mat) + 1, y = nrow(mat) / 2,
         legend = format(legend_vals, big.mark = ","),
         fill = legend_fill,
         title = "Detections",
         bty = "n",
         xjust = 0,
         yjust = 0.5,
         cex = 0.7)

  par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)
}

#' Plot Normalized Detection Heatmap
#'
#' Creates a heatmap showing detection patterns where counts for each device
#' are normalized to that device's maximum, revealing relative activity patterns.
#'
#' @param data A bluetooth_data object or data.frame from \code{process_bluetooth()}.
#' @param time_unit Character. Time aggregation: "hour", "day", "week", or "month". Default is "month".
#' @param log Logical. If TRUE, log10-transform counts (log10(count + 1)) before normalizing each
#'   device to its maximum, so relative activity is compared on a log scale. Default is FALSE.
#'
#' @return A base R heatmap showing normalized device x time detection patterns.
#'
#' @examples
#' \dontrun{
#' data <- process_bluetooth("data/combined_sort.txt")
#' plot_detection_heatmap_normalized(data, time_unit = "day")
#' plot_detection_heatmap_normalized(data, time_unit = "hour", log = TRUE)
#' }
#' @importFrom grDevices colorRampPalette
#' @export
plot_detection_heatmap_normalized <- function(data, time_unit = "month", log = FALSE) {
  
  # Validate input
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame or bluetooth_data object")
  }
  
  if (!all(c("device", "datetime", "count") %in% names(data))) {
    stop("data must have columns: device, datetime, count")
  }
  
  # Create time bins
  data$time_bin <- switch(time_unit,
    "hour" = format(data$datetime, "%Y-%m-%d %H:00"),
    "day" = format(data$datetime, "%Y-%m-%d"),
    "week" = format(as.Date(cut(data$datetime, "week")), "%Y-%m-%d"),
    "month" = format(data$datetime, "%Y-%m"),
    stop("time_unit must be one of: hour, day, week, month")
  )
  
  # Aggregate
  agg_data <- aggregate(count ~ device + time_bin, data = data, FUN = sum)
  
  # Create matrix for heatmap
  devices <- sort(unique(agg_data$device))
  time_bins <- sort(unique(agg_data$time_bin))
  
  mat <- matrix(0, nrow = length(devices), ncol = length(time_bins))
  rownames(mat) <- devices
  colnames(mat) <- time_bins
  
  for (i in 1:nrow(agg_data)) {
    row_idx <- match(agg_data$device[i], devices)
    col_idx <- match(agg_data$time_bin[i], time_bins)
    mat[row_idx, col_idx] <- agg_data$count[i]
  }
  
  # Optionally log-transform counts before normalizing (log10(count + 1))
  base <- if (isTRUE(log)) log10(mat + 1) else mat

  # Normalize each row (device) to its maximum
  mat_normalized <- t(apply(base, 1, function(row) {
    max_val <- max(row)
    if (max_val > 0) {
      row / max_val
    } else {
      row
    }
  }))
  
  # Create color palette
  colors <- colorRampPalette(c("white", "yellow", "orange", "red"))(100)
  
  # Plot heatmap with space for legend on right and restore par afterwards
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(8, 8, 4, 8), xpd = TRUE)
  
  # Determine which columns to show (show every nth if too many)
  n_cols <- ncol(mat_normalized)
  show_labels <- if (n_cols <= 24) {
    TRUE
  } else {
    seq(1, n_cols, by = ceiling(n_cols / 24))
  }
  
  image(1:ncol(mat_normalized), 1:nrow(mat_normalized), t(mat_normalized),
        col = colors,
        xlab = "", ylab = "",
        main = paste0("Normalized Detection Heatmap by ", time_unit,
                      if (isTRUE(log)) " (log scale, n = " else " (n = ",
                      format(sum(data$count), big.mark = ","), ")"),
        axes = FALSE)
  
  # Add axes
  axis(2, at = 1:nrow(mat_normalized), labels = rownames(mat_normalized), las = 1, cex.axis = 0.8)
  
  if (is.logical(show_labels)) {
    axis(1, at = 1:ncol(mat_normalized), labels = colnames(mat_normalized), las = 2, cex.axis = 0.6)
  } else {
    axis(1, at = show_labels, labels = colnames(mat_normalized)[show_labels], las = 2, cex.axis = 0.6)
  }
  
  # Add color scale legend to the right of plot (0 to 1)
  legend_vals <- seq(0, 1, by = 0.25)
  legend(x = ncol(mat_normalized) + 1, y = nrow(mat_normalized) / 2,
         legend = paste0(legend_vals * 100, "%"),
         fill = colorRampPalette(c("white", "yellow", "orange", "red"))(length(legend_vals)),
         title = "Relative\nActivity",
         bty = "n",
         xjust = 0,
         yjust = 0.5,
         cex = 0.7)
}
