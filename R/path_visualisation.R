#' Rename Devices in Address Paths
#'
#' Replaces device identifiers in path strings with custom names.
#' Useful for converting numeric device IDs to meaningful location names.
#'
#' @param path_data A data.frame from \code{get_address_paths()} containing path information.
#' @param device_names A named list or vector where names are old device IDs and values are new names.
#'   Example: \code{c("14" = "Entrance", "18" = "Main Hall", "21" = "Exit")}
#'
#' @return A data.frame with the same structure as \code{path_data} but with renamed devices in the path column.
#'
#' @details
#' The function processes all columns that might contain device identifiers:
#' \itemize{
#'   \item \code{path}: The main path string (e.g., "14 -> 18 -> 21")
#'   \item \code{first_device}: Starting device
#'   \item \code{last_device}: Ending device
#'   \item Device-specific columns if present
#' }
#'
#' @examples
#' \dontrun{
#' # Get address paths
#' paths <- get_address_paths(files, top_n = 50)
#'
#' # Define device names
#' names <- c("14" = "Entrance", "18" = "Main Hall", "21" = "Gallery", "16" = "Exit")
#'
#' # Rename devices
#' paths_named <- rename_path_devices(paths, names)
#'
#' # Now paths show: "Entrance -> Main Hall -> Gallery"
#' plot_address_paths_sankey(paths_named)
#' }
#' @export
rename_path_devices <- function(path_data, device_names) {
  # Validate input
  if (!is.data.frame(path_data)) {
    stop("path_data must be a data.frame")
  }
  
  if (is.null(names(device_names)) || any(names(device_names) == "")) {
    stop("device_names must be a named list or vector")
  }
  
  # Make a copy to avoid modifying original
  result <- path_data
  
  # Replace in path column if it exists
  if ("path" %in% names(result)) {
    for (old_name in names(device_names)) {
      new_name <- device_names[[old_name]]
      # Replace whole words only (surrounded by spaces, arrows, or string boundaries)
      pattern <- paste0("(^| -> )", old_name, "( -> |$)")
      replacement <- paste0("\\1", new_name, "\\2")
      result$path <- gsub(pattern, replacement, result$path)
    }
  }
  
  # Replace in first_device column if it exists
  if ("first_device" %in% names(result)) {
    for (old_name in names(device_names)) {
      new_name <- device_names[[old_name]]
      result$first_device[result$first_device == old_name] <- new_name
    }
  }
  
  # Replace in last_device column if it exists
  if ("last_device" %in% names(result)) {
    for (old_name in names(device_names)) {
      new_name <- device_names[[old_name]]
      result$last_device[result$last_device == old_name] <- new_name
    }
  }
  
  return(result)
}


#' Visualize Address Paths as Sankey Diagram
#'
#' Creates an interactive Sankey diagram showing the flow of addresses through devices.
#' Each path segment (device A -> device B) becomes a flow in the diagram, with width
#' proportional to the number of addresses following that route.
#'
#' @param path_data A data.frame from \code{get_address_paths()} containing a 'path' column.
#' @param min_flow Minimum number of addresses required for a flow to be displayed. Default is 1.
#' @param title Plot title. Default is "Address Movement Through Devices".
#' @param node_color Color scheme for nodes. Can be a single color or vector of colors.
#' @param link_color Color scheme for links. Options: "source" (color by source node),
#'   "target" (color by target node), or a specific color value. Default is "source".
#' @param font_size Font size for node labels. Default is 12.
#' @param node_width Width of the nodes in pixels. Default is 15.
#' @param node_padding Vertical padding between nodes in pixels. Default is 10.
#'
#' @return A networkD3 Sankey diagram object (can be printed to display).
#'
#' @details
#' The function parses path strings (e.g., "16 -> 18 -> 21") and creates a Sankey
#' diagram where:
#' \itemize{
#'   \item Nodes represent devices
#'   \item Links represent address movements between devices
#'   \item Link width shows the number of addresses following that path segment
#' }
#'
#' Requires the \code{networkD3} package. Install with: \code{install.packages("networkD3")}
#'
#' @examples
#' \dontrun{
#' # Get address paths
#' paths <- get_address_paths(files, top_n = 50)
#'
#' # Create Sankey diagram
#' plot_address_paths_sankey(paths)
#'
#' # Only show flows with at least 3 addresses
#' plot_address_paths_sankey(paths, min_flow = 3)
#'
#' # Customize appearance
#' plot_address_paths_sankey(paths,
#'                          title = "Device Movement Patterns",
#'                          node_color = "steelblue",
#'                          link_color = "target")
#' }
#' @export
plot_address_paths_sankey <- function(path_data,
                                      min_flow = 1,
                                      title = "Address Movement Through Devices",
                                      node_color = NULL,
                                      link_color = "source",
                                      font_size = 12,
                                      node_width = 15,
                                      node_padding = 10) {
  
  # Check for required package
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    stop("Package 'networkD3' is required for Sankey diagrams. Install it with: install.packages('networkD3')")
  }
  
  # Validate input
  if (!is.data.frame(path_data)) {
    stop("path_data must be a data.frame")
  }
  
  if (!"path" %in% names(path_data)) {
    stop("path_data must contain a 'path' column")
  }
  
  if (nrow(path_data) == 0) {
    stop("path_data is empty")
  }
  
  # Parse paths into links (source -> target pairs)
  # Each path like "16 -> 18 -> 21" becomes links: (16, 18) and (18, 21)
  link_list <- list()
  
  for (i in seq_len(nrow(path_data))) {
    path_str <- path_data$path[i]
    
    # Split by " -> " to get device sequence
    devices <- strsplit(path_str, " -> ", fixed = TRUE)[[1]]
    devices <- trimws(devices)  # Remove any whitespace
    
    # Create links for consecutive device pairs
    if (length(devices) >= 2) {
      for (j in 1:(length(devices) - 1)) {
        source_dev <- devices[j]
        target_dev <- devices[j + 1]
        
        link_list[[length(link_list) + 1]] <- data.frame(
          source = source_dev,
          target = target_dev,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(link_list) == 0) {
    stop("No valid path segments found in the data")
  }
  
  # Combine all links into a data frame
  links_df <- do.call(rbind, link_list)
  
  # Aggregate to count flows (number of addresses following each path segment)
  links_agg <- aggregate(
    list(value = rep(1, nrow(links_df))),
    by = list(source = links_df$source, target = links_df$target),
    FUN = sum
  )
  
  # Filter by minimum flow
  links_agg <- links_agg[links_agg$value >= min_flow, ]
  
  if (nrow(links_agg) == 0) {
    stop(paste0("No flows meet the minimum threshold of ", min_flow, 
                ". Try reducing min_flow or using more addresses."))
  }
  
  # Create node list (unique devices)
  nodes <- unique(c(links_agg$source, links_agg$target))
  nodes_df <- data.frame(name = nodes, stringsAsFactors = FALSE)
  
  # Convert node names to indices (0-based for networkD3)
  links_agg$source_idx <- match(links_agg$source, nodes_df$name) - 1
  links_agg$target_idx <- match(links_agg$target, nodes_df$name) - 1
  
  # Prepare node colors if specified
  if (!is.null(node_color)) {
    if (length(node_color) == 1) {
      # Single color - replicate for all nodes
      color_js <- paste0('d3.scaleOrdinal().domain([',
                        paste0('"', nodes_df$name, '"', collapse = ','),
                        ']).range(["', node_color, '"])')
    } else if (length(node_color) == nrow(nodes_df)) {
      # One color per node
      color_js <- paste0('d3.scaleOrdinal().domain([',
                        paste0('"', nodes_df$name, '"', collapse = ','),
                        ']).range([',
                        paste0('"', node_color, '"', collapse = ','),
                        '])')
    } else {
      warning("node_color length doesn't match number of nodes. Using default colors.")
      color_js <- 'd3.scaleOrdinal(d3.schemeCategory20)'
    }
  } else {
    # Default color scheme
    color_js <- 'd3.scaleOrdinal(d3.schemeCategory20)'
  }
  
  # Create Sankey diagram
  # Note: networkD3 link colors are controlled by LinkGroup parameter
  sankey <- networkD3::sankeyNetwork(
    Links = links_agg,
    Nodes = nodes_df,
    Source = "source_idx",
    Target = "target_idx",
    Value = "value",
    NodeID = "name",
    units = "addresses",
    fontSize = font_size,
    nodeWidth = node_width,
    nodePadding = node_padding,
    colourScale = color_js
  )
  
  # Add title if provided
  if (!is.null(title) && title != "") {
    sankey <- htmlwidgets::prependContent(
      sankey,
      htmltools::tags$h3(title, style = "text-align: center;")
    )
  }
  
  # Print summary
  message(sprintf("Sankey diagram created:"))
  message(sprintf("  - %d unique devices (nodes)", nrow(nodes_df)))
  message(sprintf("  - %d path segments (links)", nrow(links_agg)))
  message(sprintf("  - %d total address movements", sum(links_agg$value)))
  
  return(sankey)
}


#' Visualize Address Paths as Alluvial Diagram
#'
#' Creates an alluvial (stream) diagram showing address flows through devices over time.
#' Alternative to Sankey diagram using ggplot2 and ggalluvial.
#'
#' @param path_data A data.frame from \code{get_address_paths()} containing a 'path' column.
#' @param min_flow Minimum number of addresses required for a flow to be displayed. Default is 1.
#' @param max_steps Maximum number of path steps to display. Paths longer than this will be truncated. Default is 10.
#' @param fill_by How to color the flows. Options: "device" (color by starting device), 
#'   "step" (color by position in path). Default is "device".
#' @param alpha Transparency of the flows (0-1). Default is 0.7.
#'
#' @return A ggplot2 object showing the alluvial diagram.
#'
#' @details
#' The alluvial diagram shows the flow of addresses through sequential device detections.
#' Each vertical axis represents a step in the path, and flows show address movements.
#'
#' Requires the \code{ggalluvial} package. Install with: \code{install.packages("ggalluvial")}
#'
#' @examples
#' \dontrun{
#' paths <- get_address_paths(files, top_n = 30)
#' plot_address_paths_alluvial(paths, min_flow = 2)
#' }
#' @importFrom stats as.formula
#' @export
plot_address_paths_alluvial <- function(path_data,
                                        min_flow = 1,
                                        max_steps = 10,
                                        fill_by = "device",
                                        alpha = 0.7) {
  
  # Check for required packages
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    stop("Package 'ggalluvial' is required. Install it with: install.packages('ggalluvial')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install it with: install.packages('ggplot2')")
  }
  
  # Validate input
  if (!is.data.frame(path_data)) {
    stop("path_data must be a data.frame")
  }
  if (!"path" %in% names(path_data)) {
    stop("path_data must contain a 'path' column")
  }
  if (nrow(path_data) == 0) {
    stop("path_data is empty")
  }
  
  # Parse paths into step-by-step format
  path_list <- list()
  
  for (i in seq_len(nrow(path_data))) {
    path_str <- path_data$path[i]
    devices <- strsplit(path_str, " -> ", fixed = TRUE)[[1]]
    devices <- trimws(devices)
    
    if (length(devices) >= 2) {
      # Truncate if too long
      if (length(devices) > max_steps) {
        devices <- devices[1:max_steps]
      }
      
      # Create row with device at each step
      row_data <- as.list(devices)
      names(row_data) <- paste0("step", seq_along(devices))
      row_data$freq <- 1
      row_data$start_device <- devices[1]
      
      path_list[[length(path_list) + 1]] <- as.data.frame(row_data, stringsAsFactors = FALSE)
    }
  }
  
  # Combine all paths
  # Need to handle different path lengths - fill with NA
  max_length <- max(sapply(path_list, function(x) sum(grepl("^step", names(x)))))
  
  for (i in seq_along(path_list)) {
    current_steps <- sum(grepl("^step", names(path_list[[i]])))
    if (current_steps < max_length) {
      for (j in (current_steps + 1):max_length) {
        path_list[[i]][[paste0("step", j)]] <- NA
      }
    }
  }
  
  paths_df <- do.call(rbind, path_list)
  
  # Aggregate identical paths
  step_cols <- grep("^step", names(paths_df), value = TRUE)
  agg_formula <- as.formula(paste("freq ~", paste(c(step_cols, "start_device"), collapse = " + ")))
  
  paths_agg <- aggregate(agg_formula, data = paths_df, FUN = sum)
  
  # Filter by minimum flow
  paths_agg <- paths_agg[paths_agg$freq >= min_flow, ]
  
  if (nrow(paths_agg) == 0) {
    stop(paste0("No flows meet the minimum threshold of ", min_flow))
  }
  
  # Create the alluvial plot
  # Dynamically create axis mappings for all steps
  step_cols <- grep("^step", names(paths_agg), value = TRUE)
  axis_mappings <- stats::setNames(
    lapply(step_cols, function(col) rlang::sym(col)),
    paste0("axis", seq_along(step_cols))
  )
  
  # Build base aesthetic with y and all axes
  base_aes <- ggplot2::aes(y = freq)
  for (i in seq_along(axis_mappings)) {
    base_aes[[paste0("axis", i)]] <- axis_mappings[[i]]
  }
  
  # Create plot with dynamic aesthetics
  if (fill_by == "device") {
    p <- ggplot2::ggplot(paths_agg, base_aes) +
      ggalluvial::geom_alluvium(ggplot2::aes(fill = start_device), alpha = alpha) +
      ggalluvial::geom_stratum() +
      ggplot2::geom_text(stat = ggalluvial::StatStratum, 
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Address Movement Through Devices",
                   x = "Path Step",
                   y = "Number of Addresses",
                   fill = "Starting Device") +
      ggplot2::theme(legend.position = "bottom")
  } else {
    p <- ggplot2::ggplot(paths_agg, base_aes) +
      ggalluvial::geom_alluvium(ggplot2::aes(fill = step1), alpha = alpha) +
      ggalluvial::geom_stratum() +
      ggplot2::geom_text(stat = ggalluvial::StatStratum, 
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Address Movement Through Devices",
                   x = "Path Step",
                   y = "Number of Addresses",
                   fill = "First Device") +
      ggplot2::theme(legend.position = "bottom")
  }
  
  message(sprintf("Displaying %d path steps with %d unique flows", 
                  length(step_cols), nrow(paths_agg)))
  
  return(p)
}
