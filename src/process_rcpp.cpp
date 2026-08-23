#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>

#include "parallel_read.h"

using namespace Rcpp;

// Helper function to check if name starts with any prefix in the list
bool starts_with_any(const std::string& name, const std::vector<std::string>& prefixes) {
  if (prefixes.empty()) {
    return false;
  }
  
  // Trim leading whitespace from name
  size_t start = 0;
  while (start < name.length() && std::isspace(name[start])) {
    start++;
  }
  
  std::string trimmed_name = name.substr(start);
  
  for (const auto& prefix : prefixes) {
    if (trimmed_name.length() >= prefix.length() &&
        trimmed_name.substr(0, prefix.length()) == prefix) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
CharacterVector get_unique_device_names(std::string input_file, 
                                        int progress_interval = 10000) {
  
  // Open input file
  std::ifstream file(input_file, std::ios::binary);
  if (!file.is_open()) {
    stop("Cannot open input file: " + input_file);
  }
  
  // Use unordered_set to store unique names
  std::unordered_set<std::string> unique_names;
  
  std::string line;
  int line_count = 0;
  
  // Process file line by line
  while (std::getline(file, line)) {
    line_count++;
    
    // Check for user interrupts periodically
    if (line_count % 10000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    
    // Output progress
    if (progress_interval > 0 && line_count % progress_interval == 0) {
      Rcout << "Processed " << line_count << " lines...\n";
      R_FlushConsole();
    }
    
    // Parse line: device datetime address power name
    std::istringstream iss(line);
    std::string device, datetime, address, power, name;
    
    if (iss >> device >> datetime >> address >> power) {
      // Get the rest of the line as name (optional)
      std::getline(iss, name);
      
      // Trim leading whitespace from name
      size_t start = 0;
      while (start < name.length() && std::isspace(name[start])) {
        start++;
      }
      
      // Trim trailing whitespace
      size_t end = name.length();
      while (end > start && std::isspace(name[end - 1])) {
        end--;
      }
      
      std::string trimmed_name = name.substr(start, end - start);
      
      // Add to set (empty names will be stored as empty string)
      unique_names.insert(trimmed_name);
    }
  }
  
  file.close();
  Rcout << "Total lines processed: " << line_count << "\n";
  Rcout << "Unique device names found: " << unique_names.size() << "\n";
  
  // Convert set to sorted vector
  std::vector<std::string> names_vector(unique_names.begin(), unique_names.end());
  
  // Sort alphabetically
  std::sort(names_vector.begin(), names_vector.end());
  
  // Convert to R CharacterVector
  CharacterVector result(names_vector.begin(), names_vector.end());
  
  return result;
}

// [[Rcpp::export]]
DataFrame find_common_prefixes_cpp(CharacterVector device_names, 
                                   int min_length = 3, 
                                   int min_count = 2,
                                   std::string stop_char = "") {
  
  // Convert to C++ vector and remove empty strings
  std::vector<std::string> names;
  for (int i = 0; i < device_names.size(); i++) {
    std::string name = as<std::string>(device_names[i]);
    if (name.length() > 0) {
      names.push_back(name);
    }
  }
  
  if (names.empty()) {
    return DataFrame::create(
      Named("prefix") = CharacterVector(0),
      Named("count") = IntegerVector(0),
      _["stringsAsFactors"] = false
    );
  }
  
  // Map from prefix to set of names with that prefix
  std::unordered_map<std::string, std::unordered_set<std::string>> prefix_map;
  
  // Extract all possible prefixes
  for (const auto& name : names) {
    int name_length = name.length();
    
    // If stop_char is specified, find its position
    int max_len = name_length;
    if (!stop_char.empty() && stop_char.length() > 0) {
      size_t stop_pos = name.find(stop_char[0]);
      if (stop_pos != std::string::npos) {
        max_len = stop_pos;
      }
    }
    
    if (max_len >= min_length) {
      // If stop_char is used, only add the longest prefix (up to stop_char)
      // Otherwise, get all prefixes from min_length to max_len
      if (!stop_char.empty() && stop_char.length() > 0) {
        std::string prefix = name.substr(0, max_len);
        prefix_map[prefix].insert(name);
      } else {
        for (int len = min_length; len <= max_len; len++) {
          std::string prefix = name.substr(0, len);
          prefix_map[prefix].insert(name);
        }
      }
    }
  }
  
  // Structure to hold results
  struct PrefixInfo {
    std::string prefix;
    int count;
    int prefix_length;
  };
  
  std::vector<PrefixInfo> results;
  
  // Filter by min_count and prepare results
  for (const auto& pair : prefix_map) {
    int count = pair.second.size();
    
    if (count >= min_count) {
      results.push_back({
        pair.first,
        count,
        static_cast<int>(pair.first.length())
      });
    }
  }
  
  // Sort by count (descending), then by prefix length (descending)
  std::sort(results.begin(), results.end(), 
    [](const PrefixInfo& a, const PrefixInfo& b) {
      if (a.count != b.count) {
        return a.count > b.count;
      }
      return a.prefix_length > b.prefix_length;
    });
  
  // Convert to R vectors
  CharacterVector prefix_vec(results.size());
  IntegerVector count_vec(results.size());
  
  for (size_t i = 0; i < results.size(); i++) {
    prefix_vec[i] = results[i].prefix;
    count_vec[i] = results[i].count;
  }
  
  return DataFrame::create(
    Named("prefix") = prefix_vec,
    Named("count") = count_vec,
    _["stringsAsFactors"] = false
  );
}

// [[Rcpp::export]]
DataFrame process_bluetooth_files(CharacterVector input_files,
                                  int progress_interval = 1000,
                                  Nullable<CharacterVector> include_prefixes = R_NilValue,
                                  Nullable<CharacterVector> exclude_prefixes = R_NilValue,
                                  Nullable<CharacterVector> exclude_addresses = R_NilValue) {

  // Convert R prefix lists to C++ vectors
  std::vector<std::string> include_list;
  std::vector<std::string> exclude_list;

  if (include_prefixes.isNotNull()) {
    CharacterVector inc = as<CharacterVector>(include_prefixes);
    for (int i = 0; i < inc.size(); i++) {
      include_list.push_back(as<std::string>(inc[i]));
    }
  }

  if (exclude_prefixes.isNotNull()) {
    CharacterVector exc = as<CharacterVector>(exclude_prefixes);
    for (int i = 0; i < exc.size(); i++) {
      exclude_list.push_back(as<std::string>(exc[i]));
    }
  }

  // Addresses to exclude entirely (all packets, named or not)
  std::unordered_set<std::string> exclude_addr_set;
  if (exclude_addresses.isNotNull()) {
    CharacterVector exa = as<CharacterVector>(exclude_addresses);
    for (int i = 0; i < exa.size(); i++) {
      exclude_addr_set.insert(as<std::string>(exa[i]));
    }
  }
  bool filter_address = !exclude_addr_set.empty();
  
  // Convert the R file list to std::strings for the parallel reader.
  std::vector<std::string> files_vec;
  files_vec.reserve(input_files.size());
  for (int i = 0; i < input_files.size(); i++) {
    files_vec.push_back(as<std::string>(input_files[i]));
  }

  // Per-thread accumulator: device_datetime counts + unique names + counters.
  struct BtAcc {
    std::unordered_map<std::string, int> count_by_device;
    std::unordered_set<std::string> unique_names;
    size_t lines = 0;
    size_t filtered = 0;
  };

  // Per-line work (runs on worker threads; touches only its accumulator).
  auto line_fn = [&](BtAcc& acc, const std::string& line) {
    acc.lines++;

    // Parse the four whitespace-delimited fields as views into `line`.
    std::string_view tok[4], name;
    if (wsbt::split_fields(line, tok, 4, name) < 4) {
      return;  // malformed line (not counted as filtered, matching original)
    }
    std::string_view device = tok[0];
    std::string_view datetime = tok[1];
    std::string_view address = tok[2];

    // Drop all packets for excluded addresses (e.g. classified devices)
    if (filter_address &&
        exclude_addr_set.find(std::string(address)) != exclude_addr_set.end()) {
      acc.filtered++;
      return;
    }

    // Apply filtering based on name prefixes FIRST (name is the line remainder)
    bool should_include = true;
    if (!include_list.empty()) {
      should_include = wsbt::name_starts_with_any_sv(name, include_list);
    }
    if (should_include && !exclude_list.empty()) {
      should_include = !wsbt::name_starts_with_any_sv(name, exclude_list);
    }

    if (!should_include) {
      acc.filtered++;
      return;
    }

    // Store the trimmed name and increment the device_datetime count.
    std::string_view trimmed_name = wsbt::trim_view(name);
    acc.unique_names.emplace(trimmed_name);

    std::string key;
    key.reserve(device.size() + 1 + datetime.size());
    key.append(device.data(), device.size());
    key.push_back('_');
    key.append(datetime.data(), datetime.size());
    acc.count_by_device[std::move(key)]++;
  };

  // Fold one accumulator into another (runs on the main thread).
  auto merge_fn = [](BtAcc& dst, BtAcc& src) {
    dst.lines += src.lines;
    dst.filtered += src.filtered;
    if (dst.count_by_device.empty()) {
      dst.count_by_device = std::move(src.count_by_device);
    } else {
      for (auto& kv : src.count_by_device) dst.count_by_device[kv.first] += kv.second;
    }
    if (dst.unique_names.empty()) {
      dst.unique_names = std::move(src.unique_names);
    } else {
      for (auto& n : src.unique_names) dst.unique_names.insert(n);
    }
  };

  BtAcc acc = wsbt::parallel_reduce_files<BtAcc>(files_vec, line_fn, merge_fn);

  // Alias so the existing summary + DataFrame-construction code is unchanged.
  std::unordered_map<std::string, int>& count_by_device = acc.count_by_device;
  std::unordered_set<std::string>& unique_names = acc.unique_names;
  int total_lines = static_cast<int>(acc.lines);
  int total_filtered = static_cast<int>(acc.filtered);

  Rcout << "\nTotal lines processed: " << total_lines << "\n";
  Rcout << "Total lines filtered out: " << total_filtered << "\n";
  Rcout << "Unique device names found: " << unique_names.size() << "\n";
  
  // Convert map to vectors for DataFrame
  size_t total_count = count_by_device.size();
  std::vector<std::string> devices;
  std::vector<std::string> datetimes;
  std::vector<int> counts;
  
  // Reserve exact capacity for efficiency
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
      counts.push_back(pair.second);
    }
  }
  
  // Sort by device then datetime
  std::vector<size_t> indices(devices.size());
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
  std::vector<std::string> sorted_devices(devices.size());
  std::vector<std::string> sorted_datetimes(devices.size());
  std::vector<int> sorted_counts(devices.size());
  
  for (size_t i = 0; i < indices.size(); i++) {
    sorted_devices[i] = devices[indices[i]];
    sorted_datetimes[i] = datetimes[indices[i]];
    sorted_counts[i] = counts[indices[i]];
  }
  
  // Convert unique names to sorted vector
  std::vector<std::string> names_vector(unique_names.begin(), unique_names.end());
  std::sort(names_vector.begin(), names_vector.end());
  
  // Create and return DataFrame
  DataFrame result = DataFrame::create(
    Named("device") = sorted_devices,
    Named("datetime") = sorted_datetimes,
    Named("count") = sorted_counts,
    _["stringsAsFactors"] = false
  );
  
  // Add metadata as attributes
  result.attr("total_lines") = total_lines;
  result.attr("lines_filtered") = total_filtered;
  result.attr("unique_combinations") = sorted_devices.size();
  result.attr("device_names") = CharacterVector(names_vector.begin(), names_vector.end());
  
  return result;
}
