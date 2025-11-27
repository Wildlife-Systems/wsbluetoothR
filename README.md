# wsbluetoothR

R package for processing Wildlife Systems Bluetooth data.

## Overview

`wsbluetoothR` provides fast C++-based processing of Bluetooth RSSI (Received Signal Strength Indicator) data files. The package aggregates device detections by datetime and supports flexible filtering based on device name prefixes.

## Features

- **High Performance**: C++ implementation using Rcpp for fast processing of large datasets
- **Name Filtering**: Include or exclude records based on device name prefixes

## Installation

Install from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("Wildlife-Systems/wsbluetoothR")
```

## Usage

### Basic Processing

```r
library(wsbluetoothR)

# Process a Bluetooth data file
data <- process_bluetooth("data/bluetooth_data.txt")
```

### Filtering by Name Prefix

```r
# First, discover what device names are in your data
device_names <- get_device_names("data/bluetooth_data.txt")
head(device_names)

# Include only specific device names
data <- process_bluetooth("data/bluetooth_data.txt", 
                         include_prefixes = c("Apple", "Samsung"))

# Exclude specific device names
data <- process_bluetooth("data/bluetooth_data.txt", 
                         exclude_prefixes = c("Unknown", "Not Available"))
```

## Input Format

The input file should have space-separated columns:

```
device datetime address power name
```

Where:
- `device`: Detection device identifier (e.g., "pi1", "pi2")
- `datetime`: Timestamp in format YYYYMMDD-HHMMSS (e.g., "20250702-150505")
- `address`: Bluetooth MAC address
- `power`: Signal strength (RSSI value)
- `name`: Bluetooth device name (optional)

## Output

Returns a data.frame with columns:
- `device`: Character. Device identifier
- `datetime`: POSIXct. Timestamp in UTC timezone
- `count`: Integer. Number of unique Bluetooth device detections for each device-datetime combination

## License

GPL-3

## Author

Ed Baker (ed@ebaker.me.uk)
