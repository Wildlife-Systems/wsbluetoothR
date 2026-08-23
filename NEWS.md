# wsbluetoothR 0.1.0

## Initial Release

* High-performance Bluetooth data processing with Rcpp
* `process_bluetooth()` reads one or more ws-bluetooth text files and
  aggregates detections by device and datetime
* Address-level analysis: `get_address_duration()`,
  `get_average_address_duration()` and `get_address_paths()`
* Name-prefix discovery with `find_common_prefixes()` and `get_device_names()`
* Visualisation of detections, durations and paths (timelines, heatmaps,
  Sankey, alluvial and Leaflet map plots)

## Device categories

* `device_category_prefixes()` returns advertised-name prefixes for
  recognisable device categories (bike, vehicle, beacon, audio, wearable),
  stored in `inst/extdata/device_categories.csv` so the table can be extended
  without changing code
* `classify_addresses()` resolves those prefixes to the full set of addresses
  that ever advertised a matching name, so a device can be acted on completely
  rather than only on its named packets
* `remove_addresses()` drops classified addresses from an existing result
* `exclude_addresses` argument added to `process_bluetooth()`,
  `get_address_duration()`, `get_average_address_duration()` and
  `get_address_paths()`; `get_address_paths()` also gains `include_names` /
  `exclude_names`

## Performance

* The file-reading phase of every heavy reader is parallelised over byte-range
  chunks (OpenMP, `src/parallel_read.h`); the toolchain falls back to serial
  loops where OpenMP is unavailable
* Tunable via the `WSBT_READ_THREADS`, `WSBT_CHUNK_BYTES` and
  `WSBT_DEVICES_PER_PASS` environment variables
* `low_memory = TRUE` now processes devices in batches (default 4 per pass)
  rather than strictly one at a time
* Input files are opened in binary mode, so stray control bytes no longer
  truncate a read part-way through a file

## Other changes

* `plot_detection_heatmap()` gains `log = TRUE` for a log10 colour scale, with
  the legend still labelled on the real detection-count scale
