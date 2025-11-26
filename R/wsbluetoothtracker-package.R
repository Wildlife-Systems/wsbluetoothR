#' @useDynLib wsbluetoothtracker, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @keywords internal
"_PACKAGE"

#' @keywords internal
NULL

#' wsbluetoothtracker: Wireless Bluetooth Device Tracking
#'
#' Process Bluetooth RSSI data from multiple base stations to track device
#' positions using power-weighted centroid calculations. High-performance
#' C++ implementations via Rcpp for processing large datasets.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{process_bluetooth_fast}}: Process Bluetooth data files
#'   \item \code{\link{summarize_device_counts}}: Generate summary statistics
#'   \item \code{\link{plot_device_counts}}: Visualize device detections
#'   \item \code{\link{compare_devices}}: Compare multiple devices
#' }
#'
#' @docType package
#' @name wsbluetoothtracker-package
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # Compile Rcpp code if needed
  cpp_file <- system.file("src", "process_rcpp.cpp", package = pkgname)
  if (file.exists(cpp_file)) {
    tryCatch({
      Rcpp::sourceCpp(cpp_file)
    }, error = function(e) {
      packageStartupMessage("Note: Could not compile C++ code on load. ",
                           "It will be compiled on first use.")
    })
  }
}
