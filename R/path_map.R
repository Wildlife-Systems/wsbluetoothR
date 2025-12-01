#' Visualize Address Paths on a Geographic Map
#'
#' Creates a map-based visualization of address paths where device locations are
#' georeferenced and flows between devices are shown as curved lines.
#' This is similar to an alluvial diagram but with geographic positioning.
#'
#' @param path_data A data.frame from \code{get_address_paths()} containing a 'path' column.
#' @param device_locations A named list where names are device IDs and values are 
#'   numeric vectors of c(latitude, longitude). Example: 
#'   \code{list("14" = c(51.507, -0.127), "18" = c(51.508, -0.128))}
#' @param min_flow Minimum number of addresses required for a flow to be displayed. Default is 1.
#' @param max_steps Maximum number of path steps to display. Paths longer than this will be truncated. Default is 10.
#' @param map_provider Map tile provider. Options: "OpenStreetMap", "CartoDB.Positron", 
#'   "CartoDB.DarkMatter", "Esri.WorldImagery". Default is "CartoDB.Positron".
#' @param flow_color Color for the flow lines. Default is "steelblue".
#' @param flow_opacity Opacity of flow lines (0-1). Default is 0.6.
#' @param node_color Color for device nodes. Default is "red".
#' @param node_size Size of device nodes. Default is 10.
#' @param show_labels Whether to show device labels. Default is TRUE.
#' @param curve_weight How much to curve the flow lines (0 = straight, 1 = more curved). Default is 0.5.
#'
#' @return A leaflet map object showing georeferenced address paths.
#'
#' @details
#' The function creates an interactive map where:
#' \itemize{
#'   \item Nodes represent device locations (circles)
#'   \item Curved lines represent address flows between devices
#'   \item Line thickness is proportional to the number of addresses following that path
#'   \item Clicking on nodes shows device information
#'   \item Hovering over lines shows flow information
#' }
#'
#' Requires the \code{leaflet} package. Install with: \code{install.packages("leaflet")}
#'
#' @examples
#' \dontrun{
#' # Get address paths
#' paths <- get_address_paths(files, top_n = 50)
#'
#' # Define device locations
#' locations <- list(
#'   "14" = c(51.507, -0.127),
#'   "16" = c(51.508, -0.128),
#'   "18" = c(51.509, -0.126),
#'   "21" = c(51.507, -0.125)
#' )
#'
#' # Create map
#' plot_address_paths_map(paths, locations)
#'
#' # Customize appearance
#' plot_address_paths_map(paths, locations,
#'                       min_flow = 3,
#'                       flow_color = "purple",
#'                       map_provider = "CartoDB.DarkMatter")
#' }
#' @export
plot_address_paths_map <- function(path_data,
                                   device_locations,
                                   min_flow = 1,
                                   max_steps = 10,
                                   map_provider = "CartoDB.Positron",
                                   flow_color = "steelblue",
                                   flow_opacity = 0.6,
                                   node_color = "red",
                                   node_size = 10,
                                   show_labels = TRUE,
                                   curve_weight = 0.5) {
  
  # Check for required package
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required. Install it with: install.packages('leaflet')")
  }
  
  # Validate input
  if (!is.data.frame(path_data)) {
    stop("path_data must be a data.frame")
  }
  if (!"path" %in% names(path_data)) {
    stop("path_data must contain a 'path' column")
  }
  if (!is.list(device_locations) || is.null(names(device_locations))) {
    stop("device_locations must be a named list")
  }
  if (any(names(device_locations) == "")) {
    stop("All elements in device_locations must be named")
  }
  if (!all(sapply(device_locations, function(x) is.numeric(x) && length(x) == 2))) {
    stop("Each element in device_locations must be a numeric vector of length 2: c(latitude, longitude)")
  }
  
  # Parse paths into links (source -> target pairs) with counts
  link_list <- list()
  
  for (i in seq_len(nrow(path_data))) {
    path_str <- path_data$path[i]
    devices <- strsplit(path_str, " -> ", fixed = TRUE)[[1]]
    devices <- trimws(devices)
    
    # Truncate if too long
    if (length(devices) > max_steps) {
      devices <- devices[1:max_steps]
    }
    
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
  
  # Combine and aggregate links
  links_df <- do.call(rbind, link_list)
  links_agg <- aggregate(
    list(count = rep(1, nrow(links_df))),
    by = list(source = links_df$source, target = links_df$target),
    FUN = sum
  )
  
  # Filter by minimum flow
  links_agg <- links_agg[links_agg$count >= min_flow, ]
  
  if (nrow(links_agg) == 0) {
    stop(paste0("No flows meet the minimum threshold of ", min_flow))
  }
  
  # Add location data from named list
  links_agg$source_lat <- sapply(links_agg$source, function(d) {
    if (d %in% names(device_locations)) device_locations[[d]][1] else NA_real_
  })
  links_agg$source_lon <- sapply(links_agg$source, function(d) {
    if (d %in% names(device_locations)) device_locations[[d]][2] else NA_real_
  })
  
  links_agg$target_lat <- sapply(links_agg$target, function(d) {
    if (d %in% names(device_locations)) device_locations[[d]][1] else NA_real_
  })
  links_agg$target_lon <- sapply(links_agg$target, function(d) {
    if (d %in% names(device_locations)) device_locations[[d]][2] else NA_real_
  })
  
  # Check for missing locations
  missing_coords <- is.na(links_agg$source_lon) | is.na(links_agg$source_lat) |
                   is.na(links_agg$target_lon) | is.na(links_agg$target_lat)
  
  if (any(missing_coords)) {
    missing_devices <- unique(c(
      links_agg$source[is.na(links_agg$source_lon) | is.na(links_agg$source_lat)],
      links_agg$target[is.na(links_agg$target_lon) | is.na(links_agg$target_lat)]
    ))
    warning(sprintf("Missing coordinates for %d device(s): %s", 
                   length(missing_devices),
                   paste(missing_devices, collapse = ", ")))
    links_agg <- links_agg[!missing_coords, ]
  }
  
  if (nrow(links_agg) == 0) {
    stop("No valid flows with complete location data")
  }
  
  # Get unique devices with locations
  devices_in_use <- unique(c(links_agg$source, links_agg$target))
  devices_in_use <- devices_in_use[devices_in_use %in% names(device_locations)]
  
  # Create nodes data frame from named list
  nodes_df <- data.frame(
    device = devices_in_use,
    latitude = sapply(devices_in_use, function(d) device_locations[[d]][1]),
    longitude = sapply(devices_in_use, function(d) device_locations[[d]][2]),
    stringsAsFactors = FALSE
  )
  
  # Calculate line weights based on flow counts
  links_agg$weight <- scales::rescale(links_agg$count, to = c(2, 10))
  
  # Create base map
  map_center_lon <- mean(nodes_df$longitude)
  map_center_lat <- mean(nodes_df$latitude)
  
  m <- leaflet::leaflet() %>%
    leaflet::setView(lng = map_center_lon, lat = map_center_lat, zoom = 15)
  
  # Add tile layer based on provider
  if (map_provider == "OpenStreetMap") {
    m <- m %>% leaflet::addTiles()
  } else {
    m <- m %>% leaflet::addProviderTiles(map_provider)
  }
  
  # Add curved lines for flows
  for (i in seq_len(nrow(links_agg))) {
    # Create curved path using Bezier-style control point
    source_lon <- links_agg$source_lon[i]
    source_lat <- links_agg$source_lat[i]
    target_lon <- links_agg$target_lon[i]
    target_lat <- links_agg$target_lat[i]
    
    # Calculate midpoint
    mid_lon <- (source_lon + target_lon) / 2
    mid_lat <- (source_lat + target_lat) / 2
    
    # Offset midpoint perpendicular to line for curve
    # Calculate perpendicular direction
    dx <- target_lon - source_lon
    dy <- target_lat - source_lat
    dist <- sqrt(dx^2 + dy^2)
    
    if (dist > 0) {
      # Perpendicular offset
      perp_lon <- -dy / dist * curve_weight * dist * 0.1
      perp_lat <- dx / dist * curve_weight * dist * 0.1
      
      control_lon <- mid_lon + perp_lon
      control_lat <- mid_lat + perp_lat
      
      # Create curved path by interpolating through control point
      n_points <- 20
      t <- seq(0, 1, length.out = n_points)
      
      # Quadratic Bezier curve
      curve_lons <- (1-t)^2 * source_lon + 2*(1-t)*t * control_lon + t^2 * target_lon
      curve_lats <- (1-t)^2 * source_lat + 2*(1-t)*t * control_lat + t^2 * target_lat
      
      curve_coords <- matrix(c(curve_lons, curve_lats), ncol = 2)
      
      # Add curved polyline
      popup_text <- sprintf(
        "<b>Flow:</b> %s → %s<br><b>Count:</b> %d addresses",
        links_agg$source[i], links_agg$target[i], links_agg$count[i]
      )
      
      m <- m %>% 
        leaflet::addPolylines(
          lng = curve_coords[, 1],
          lat = curve_coords[, 2],
          weight = links_agg$weight[i],
          color = flow_color,
          opacity = flow_opacity,
          popup = popup_text,
          group = "flows"
        )
    }
  }
  
  # Add device nodes
  node_popup <- sprintf(
    "<b>Device:</b> %s<br><b>Location:</b> %.5f, %.5f",
    nodes_df$device, nodes_df$latitude, nodes_df$longitude
  )
  
  m <- m %>% 
    leaflet::addCircleMarkers(
      lng = nodes_df$longitude,
      lat = nodes_df$latitude,
      radius = node_size,
      color = node_color,
      fillColor = node_color,
      fillOpacity = 0.8,
      weight = 2,
      popup = node_popup,
      group = "nodes"
    )
  
  # Add labels if requested
  if (show_labels) {
    m <- m %>%
      leaflet::addLabelOnlyMarkers(
        lng = nodes_df$longitude,
        lat = nodes_df$latitude,
        label = nodes_df$device,
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-weight" = "bold",
            "font-size" = "12px",
            "border" = "none",
            "background" = "rgba(255,255,255,0.7)",
            "padding" = "2px 4px"
          )
        ),
        group = "labels"
      )
  }
  
  # Add layer control
  m <- m %>%
    leaflet::addLayersControl(
      overlayGroups = c("flows", "nodes", "labels"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  
  # Add legend
  flow_range <- range(links_agg$count)
  m <- m %>%
    leaflet::addLegend(
      position = "bottomright",
      colors = c(flow_color, node_color),
      labels = c(
        sprintf("Flows (%d-%d addresses)", flow_range[1], flow_range[2]),
        "Devices"
      ),
      title = "Legend",
      opacity = 1
    )
  
  message(sprintf("Map created with %d devices and %d flows", 
                  nrow(nodes_df), nrow(links_agg)))
  
  return(m)
}


#' Create Device Location Template
#'
#' Helper function to create a template named list for device locations.
#' Useful for setting up georeferencing data.
#'
#' @param path_data A data.frame from \code{get_address_paths()} containing path information.
#'
#' @return A named list with device IDs as names and NA coordinate vectors as values.
#'
#' @examples
#' \dontrun{
#' paths <- get_address_paths(files, top_n = 50)
#' 
#' # Create template
#' location_template <- create_device_location_template(paths)
#' 
#' # Fill in coordinates (example)
#' location_template[["14"]] <- c(51.507, -0.127)
#' location_template[["16"]] <- c(51.508, -0.128)
#' location_template[["18"]] <- c(51.509, -0.126)
#' 
#' # Use with map
#' plot_address_paths_map(paths, location_template)
#' }
#' @export
create_device_location_template <- function(path_data) {
  # Validate input
  if (!is.data.frame(path_data)) {
    stop("path_data must be a data.frame")
  }
  if (!"path" %in% names(path_data)) {
    stop("path_data must contain a 'path' column")
  }
  
  # Extract all unique devices from paths
  all_devices <- character(0)
  
  for (i in seq_len(nrow(path_data))) {
    path_str <- path_data$path[i]
    devices <- strsplit(path_str, " -> ", fixed = TRUE)[[1]]
    devices <- trimws(devices)
    all_devices <- c(all_devices, devices)
  }
  
  unique_devices <- sort(unique(all_devices))
  
  # Create template named list
  template <- stats::setNames(
    lapply(unique_devices, function(x) c(NA_real_, NA_real_)),
    unique_devices
  )
  
  message(sprintf("Created template for %d devices", length(template)))
  message("Fill in coordinates for each device: list_name[[\"device\"]] <- c(latitude, longitude)")
  
  return(template)
}
