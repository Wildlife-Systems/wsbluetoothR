#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame process_bluetooth_data(std::string input_file, int progress_interval = 1000) {
  
  // Open input file
  std::ifstream file(input_file);
  if (!file.is_open()) {
    stop("Cannot open input file: " + input_file);
  }
  
  // Hash map to store counts
  std::unordered_map<std::string, int> count_by_device;
  
  std::string line;
  int line_count = 0;
  
  // Process file line by line
  while (std::getline(file, line)) {
    line_count++;
    
    // Output progress
    if (progress_interval > 0 && line_count % progress_interval == 0) {
      Rcout << "Processed " << line_count << " lines...\n";
    }
    
    // Parse line: device datetime address power name
    std::istringstream iss(line);
    std::string device, datetime, address, power, name;
    
    if (iss >> device >> datetime >> address >> power) {
      // Get the rest of the line as name (optional)
      std::getline(iss, name);
      
      // Create key from device and datetime
      std::string key = device + "_" + datetime;
      
      // Increment count
      count_by_device[key]++;
    }
  }
  
  file.close();
  Rcout << "Total lines processed: " << line_count << "\n";
  
  // Convert map to vectors for DataFrame
  int total_count = count_by_device.size();
  std::vector<std::string> devices;
  std::vector<std::string> datetimes;
  std::vector<int> counts;
  
  devices.reserve(total_count);
  datetimes.reserve(total_count);
  counts.reserve(total_count);
  
  // Extract data from map
  for (const auto& pair : count_by_device) {
    // Split key into device and datetime
    std::string key = pair.first;
    size_t underscore_pos = key.find('_');
    
    if (underscore_pos != std::string::npos) {
      devices.push_back(key.substr(0, underscore_pos));
      datetimes.push_back(key.substr(underscore_pos + 1));
    } else {
      devices.push_back(key);
      datetimes.push_back("");
    }
    
    counts.push_back(pair.second);
  }
  
  // Sort by device then datetime
  std::vector<size_t> indices(total_count);
  for (size_t i = 0; i < indices.size(); i++) {
    indices[i] = i;
  }
  
  std::sort(indices.begin(), indices.end(), [&](size_t a, size_t b) {
    if (devices[a] != devices[b]) {
      return devices[a] < devices[b];
    }
    return datetimes[a] < datetimes[b];
  });
  
  // Reorder vectors
  std::vector<std::string> sorted_devices(total_count);
  std::vector<std::string> sorted_datetimes(total_count);
  std::vector<int> sorted_counts(total_count);
  
  for (size_t i = 0; i < indices.size(); i++) {
    sorted_devices[i] = devices[indices[i]];
    sorted_datetimes[i] = datetimes[indices[i]];
    sorted_counts[i] = counts[indices[i]];
  }
  
  // Create and return DataFrame
  return DataFrame::create(
    Named("device") = sorted_devices,
    Named("datetime") = sorted_datetimes,
    Named("count") = sorted_counts,
    _["stringsAsFactors"] = false
  );
}
