#' Reference Name Prefixes for Device Categories
#'
#' Returns advertised-name prefixes that identify recognisable categories of
#' Bluetooth device - static beacons, vehicles, bikes, personal audio and
#' wearables. These devices are not the general (mostly phone-carrying) survey
#' population, but they are perfectly valid to study in their own right:
#' labelling them lets you either \emph{exclude} them for a person-only analysis
#' or \emph{select} them to look specifically at, say, bike or vehicle movement.
#'
#' The prefixes are seeded from patterns observed in real ws-bluetooth data plus
#' well-known device families, and are stored in
#' \code{inst/extdata/device_categories.csv} so they can be extended without
#' changing code.
#'
#' The prefixes are intended for use with the \code{include_prefixes} /
#' \code{exclude_prefixes} / \code{include_names} / \code{exclude_names}
#' arguments of \code{\link{process_bluetooth}},
#' \code{\link{get_address_duration}} and \code{\link{get_address_paths}}, and as
#' the default patterns for \code{\link{classify_addresses}}.
#'
#' @param categories Character vector of categories to return. Available
#'   categories are \code{"bike"}, \code{"vehicle"}, \code{"beacon"},
#'   \code{"audio"} and \code{"wearable"}. Default is
#'   \code{c("bike", "vehicle", "beacon")} - the moving and static non-person
#'   categories. Use \code{NULL} for all categories.
#' @param as_table Logical. If \code{TRUE}, return the full reference table
#'   (columns \code{prefix}, \code{category}, \code{source}, \code{note}). If
#'   \code{FALSE} (default), return just the character vector of prefixes.
#'
#' @return A character vector of name prefixes, or a data.frame if
#'   \code{as_table = TRUE}.
#'
#' @details
#' Name matching everywhere in this package is a case-sensitive
#' \emph{starts-with} test on the advertised name, so the prefixes are chosen to
#' anchor at the start of the name (e.g. \code{"lime-"}, \code{"[TV]"}).
#'
#' Because only a small fraction of packets carry a name, matching by these
#' prefixes selects the named packets of a device but can leave its nameless
#' packets behind. To act on a device completely, pass these prefixes to
#' \code{\link{classify_addresses}} to obtain the set of addresses, then apply
#' that set via \code{exclude_addresses}.
#'
#' @examples
#' \dontrun{
#' # Default: bikes, vehicles and static beacons
#' prefixes <- device_category_prefixes()
#'
#' # Include personal audio and wearables too
#' prefixes <- device_category_prefixes(categories = NULL)
#'
#' # Inspect the reference table
#' device_category_prefixes(as_table = TRUE)
#'
#' # Use directly as a name filter (exclude non-person devices)
#' dur <- get_address_duration("data.txt", exclude_names = device_category_prefixes())
#' }
#'
#' @export
device_category_prefixes <- function(categories = c("bike", "vehicle", "beacon"),
                                     as_table = FALSE) {

  f <- system.file("extdata", "device_categories.csv", package = "wsbluetoothR")
  if (f == "" || !file.exists(f)) {
    stop("Reference table 'device_categories.csv' not found in package extdata.")
  }

  tab <- utils::read.csv(f, stringsAsFactors = FALSE, comment.char = "")

  if (!is.null(categories)) {
    available <- unique(tab$category)
    unknown <- setdiff(categories, available)
    if (length(unknown) > 0) {
      stop("Unknown categor", if (length(unknown) > 1) "ies: " else "y: ",
           paste(unknown, collapse = ", "),
           ". Available: ", paste(available, collapse = ", "))
    }
    tab <- tab[tab$category %in% categories, , drop = FALSE]
  }

  if (isTRUE(as_table)) {
    rownames(tab) <- NULL
    return(tab)
  }

  unique(tab$prefix)
}

#' Classify Addresses into Device Categories by Advertised Name
#'
#' Makes a single pass over the raw data and finds every Bluetooth address that
#' has \emph{ever} advertised a name matching one of the supplied prefixes, and
#' labels it with the corresponding category (bike, vehicle, beacon, ...). This
#' promotes a per-packet name match to a per-address label, so that every packet
#' of a beacon, vehicle or bike - including the many packets it broadcasts with
#' no name - can be acted on together downstream (excluded for a person-only
#' analysis, or selected to study that category).
#'
#' @param files Character vector of file paths to process.
#' @param patterns Either a data.frame with columns \code{prefix} and
#'   \code{category} (as returned by \code{device_category_prefixes(as_table =
#'   TRUE)}), or a plain character vector of prefixes (category will be
#'   \code{NA}). Default is the bike/vehicle/beacon prefixes from
#'   \code{\link{device_category_prefixes}}.
#' @param categories Character vector used only to build the default
#'   \code{patterns}. Ignored if \code{patterns} is supplied explicitly. Default
#'   is \code{c("bike", "vehicle", "beacon")}.
#' @param progress_interval Integer. How often to print progress (0 = silent).
#'   Default is 10000.
#' @param verbose Logical. Whether to print progress and a summary. Default TRUE.
#' @param devices Character vector. Restrict the scan to these device IDs.
#'   \code{NULL} = all devices. Default \code{NULL}.
#' @param min_date,max_date Character (YYYY-MM-DD) or Date. Restrict the scan to
#'   this date range. \code{NULL} = no bound. Default \code{NULL}.
#'
#' @return A data.frame with one row per matched address:
#'   \describe{
#'     \item{address}{Bluetooth MAC address}
#'     \item{category}{Assigned category (from \code{patterns}), or \code{NA}}
#'     \item{matched_prefix}{The prefix that matched}
#'     \item{name}{A representative matched name}
#'     \item{hits}{Number of matching (named) records for this address}
#'     \item{first_seen}{Earliest matching timestamp (YYYYMMDD-HHMMSS)}
#'     \item{last_seen}{Latest matching timestamp (YYYYMMDD-HHMMSS)}
#'   }
#'
#' @seealso \code{\link{device_category_prefixes}} for the default patterns,
#'   \code{\link{remove_addresses}} to filter a result data.frame, and the
#'   \code{exclude_addresses} argument of \code{\link{get_address_paths}},
#'   \code{\link{get_address_duration}} and \code{\link{process_bluetooth}} to
#'   re-run an analysis with these addresses removed.
#'
#' @examples
#' \dontrun{
#' # Which addresses are bikes / vehicles / beacons?
#' classified <- classify_addresses("data.txt")
#' table(classified$category)
#'
#' # Re-run a paths analysis with those devices removed entirely
#' clean_paths <- get_address_paths("data.txt",
#'                                  exclude_addresses = classified$address,
#'                                  top_n = 100)
#'
#' # Or drop them from an existing address-keyed result (no second file pass)
#' dur <- get_address_duration("data.txt")
#' dur_people <- remove_addresses(dur, classified)
#'
#' # Conversely, select only the bikes to study them
#' bikes <- classified$address[classified$category == "bike"]
#' }
#'
#' @export
classify_addresses <- function(files,
                               patterns = device_category_prefixes(categories, as_table = TRUE),
                               categories = c("bike", "vehicle", "beacon"),
                               progress_interval = 10000,
                               verbose = TRUE,
                               devices = NULL,
                               min_date = NULL,
                               max_date = NULL) {

  # Validate files
  if (!is.character(files) || length(files) == 0) {
    stop("files must be a non-empty character vector of file paths")
  }
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop("File(s) not found: ", paste(missing_files, collapse = ", "))
  }

  # Normalise patterns into a prefix vector + prefix->category lookup
  if (is.data.frame(patterns)) {
    if (!"prefix" %in% names(patterns)) {
      stop("patterns data.frame must have a 'prefix' column")
    }
    prefixes <- as.character(patterns$prefix)
    cat_lookup <- if ("category" %in% names(patterns)) {
      stats::setNames(as.character(patterns$category), prefixes)
    } else {
      stats::setNames(rep(NA_character_, length(prefixes)), prefixes)
    }
  } else {
    prefixes <- as.character(patterns)
    cat_lookup <- stats::setNames(rep(NA_character_, length(prefixes)), prefixes)
  }

  prefixes <- prefixes[!is.na(prefixes) & nzchar(prefixes)]
  prefixes <- unique(prefixes)
  if (length(prefixes) == 0) {
    stop("No usable prefixes in 'patterns'.")
  }

  # Filter parameters
  device_filter <- if (is.null(devices)) character(0) else as.character(devices)
  min_date_str <- .format_yyyymmdd(min_date)
  max_date_str <- .format_yyyymmdd(max_date)

  if (!verbose) progress_interval <- 0

  result <- find_matching_addresses(files, prefixes, as.integer(progress_interval),
                                    device_filter, min_date_str, max_date_str)

  # Map matched prefix -> category
  result$category <- unname(cat_lookup[result$matched_prefix])

  # Reorder columns
  result <- result[, c("address", "category", "matched_prefix", "name",
                       "hits", "first_seen", "last_seen"), drop = FALSE]
  result <- result[order(-result$hits), , drop = FALSE]
  rownames(result) <- NULL

  if (verbose && nrow(result) > 0) {
    cat("\nAddress classification summary:\n")
    cat("  Matched addresses:", nrow(result), "\n")
    counts <- table(factor(result$category, exclude = NULL))
    for (nm in names(counts)) {
      label <- if (is.na(nm) || nm == "NA") "(uncategorised)" else nm
      cat(sprintf("    %-14s %d\n", label, counts[[nm]]))
    }
  }

  result
}

#' Remove Addresses from an Address-Keyed Result
#'
#' Convenience filter that drops rows whose \code{address} is in a given set.
#' Because the outputs of \code{\link{get_address_paths}} and
#' \code{\link{get_address_duration}} are keyed by address, filtering their rows
#' is equivalent to having excluded those devices from the analysis - and avoids
#' a second pass over the raw files.
#'
#' @param x A data.frame with an \code{address} column (e.g. the output of
#'   \code{get_address_paths} or \code{get_address_duration}).
#' @param addresses Either the data.frame returned by
#'   \code{\link{classify_addresses}} or a character vector of addresses to
#'   remove.
#'
#' @return \code{x} with the matching rows removed.
#'
#' @examples
#' \dontrun{
#' classified <- classify_addresses("data.txt")
#' dur <- get_address_duration("data.txt")
#' dur_people <- remove_addresses(dur, classified)
#' }
#'
#' @export
remove_addresses <- function(x, addresses) {
  if (!is.data.frame(x) || !"address" %in% names(x)) {
    stop("x must be a data.frame with an 'address' column")
  }
  addr <- if (is.data.frame(addresses)) {
    if (!"address" %in% names(addresses)) {
      stop("addresses data.frame must have an 'address' column")
    }
    as.character(addresses$address)
  } else {
    as.character(addresses)
  }
  out <- x[!x$address %in% addr, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Internal: convert a Date or "YYYY-MM-DD" string to "YYYYMMDD", or "" for NULL.
.format_yyyymmdd <- function(d) {
  if (is.null(d)) return("")
  if (inherits(d, "Date")) return(format(d, "%Y%m%d"))
  gsub("-", "", as.character(d))
}
