#' Find Common Prefixes in Device Names
#'
#' Analyzes a vector of device names and finds common prefixes that appear
#' multiple times. This is useful for identifying patterns in device names
#' that can be used for filtering with include_prefixes or exclude_prefixes.
#'
#' @param device_names Character vector of device names, typically from
#'   \code{get_device_names()}.
#' @param min_length Integer. Minimum length of prefix to consider. Default is 3.
#' @param min_count Integer. Minimum number of names that must share a prefix
#'   for it to be included in results. Default is 2.
#' @param stop_char Character string. If provided, only extract prefixes up to
#'   (but not including) the first occurrence of this character. For example,
#'   use " " to find prefixes before the first space. Default is "" (no stop character).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{prefix}{Character. The common prefix}
#'     \item{count}{Integer. Number of device names starting with this prefix}
#'   }
#'   Sorted by count (descending), then by prefix length (descending).
#'
#' @examples
#' \dontrun{
#' # Get device names from file
#' names <- get_device_names("data/combined_sort.txt")
#' 
#' # Find common prefixes
#' prefixes <- find_common_prefixes(names)
#' head(prefixes)
#' 
#' # Find longer prefixes that appear at least 5 times
#' prefixes <- find_common_prefixes(names, min_length = 5, min_count = 5)
#' 
#' # Find prefixes before the first space (e.g., brand names)
#' prefixes <- find_common_prefixes(names, stop_char = " ")
#' 
#' # Use the most common prefix for filtering
#' top_prefix <- prefixes$prefix[1]
#' data <- process_bluetooth("data/combined_sort.txt", 
#'                          include_prefixes = top_prefix)
#' }
#'
#' @export
find_common_prefixes <- function(device_names, min_length = 3, min_count = 2, stop_char = "") {
  
  # Validate inputs
  if (!is.character(device_names)) {
    stop("device_names must be a character vector")
  }
  
  if (length(device_names) == 0) {
    return(data.frame(prefix = character(0), 
                     count = integer(0),
                     stringsAsFactors = FALSE))
  }
  
  if (min_length < 1) {
    stop("min_length must be at least 1")
  }
  
  if (min_count < 1) {
    stop("min_count must be at least 1")
  }
  
  if (!is.character(stop_char) || length(stop_char) != 1) {
    stop("stop_char must be a single character string")
  }
  
  # Call C++ implementation
  result <- find_common_prefixes_cpp(device_names, 
                                     as.integer(min_length), 
                                     as.integer(min_count),
                                     stop_char)
  
  return(result)
}
