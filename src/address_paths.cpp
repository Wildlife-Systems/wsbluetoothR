#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <map>
#include <vector>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <ctime>
#include <cstring>
#include <cstdlib>
#include <cctype>
#include <charconv>

#include "parallel_read.h"

using namespace Rcpp;

// Convert a Nullable<CharacterVector> (default NULL) to std::vector<std::string>.
// Returns an empty vector when the argument is NULL (the default).
static std::vector<std::string> nullable_to_str_vec(Rcpp::Nullable<Rcpp::CharacterVector> x) {
    std::vector<std::string> out;
    if (x.isNotNull()) {
        Rcpp::CharacterVector cv = Rcpp::as<Rcpp::CharacterVector>(x);
        for (int i = 0; i < cv.size(); i++) out.push_back(Rcpp::as<std::string>(cv[i]));
    }
    return out;
}

// Structure to store detection event
struct Detection {
    std::string device;
    std::string datetime;
    std::time_t timestamp;
    int power;
    
    Detection() : timestamp(0), power(-999) {}
    Detection(const std::string& dev, const std::string& dt, std::time_t ts, int pwr) 
        : device(dev), datetime(dt), timestamp(ts), power(pwr) {}
};

// Parse datetime string in format YYYYMMDD-HHMMSS to time_t
std::time_t parse_datetime_path(std::string_view datetime_str) {
    if (datetime_str.length() < 15) {
        return -1;
    }

    struct tm tm = {0};

    // Fixed offsets; views are not null-terminated, so copy fixed byte counts.
    char year[5], month[3], day[3], hour[3], min[3], sec[3];
    const char* s = datetime_str.data();

    std::memcpy(year, s, 4);       year[4] = '\0';
    std::memcpy(month, s + 4, 2);  month[2] = '\0';
    std::memcpy(day, s + 6, 2);    day[2] = '\0';
    std::memcpy(hour, s + 9, 2);   hour[2] = '\0';
    std::memcpy(min, s + 11, 2);   min[2] = '\0';
    std::memcpy(sec, s + 13, 2);   sec[2] = '\0';

    tm.tm_year = std::atoi(year) - 1900;
    tm.tm_mon = std::atoi(month) - 1;
    tm.tm_mday = std::atoi(day);
    tm.tm_hour = std::atoi(hour);
    tm.tm_min = std::atoi(min);
    tm.tm_sec = std::atoi(sec);
    tm.tm_isdst = -1;

    return std::mktime(&tm);
}

//' Track Address Paths Through Devices
//'
//' For each address, creates a chronological path showing which devices detected
//' the address over time. Returns the top N addresses by detection count.
//'
//' @param input_files Character vector of file paths to process.
//' @param top_n Integer. Number of top addresses to return (by detection count). Default is 10.
//' @param progress_interval Integer. How often to print progress (0 = no progress). Default is 10000.
//' @param device_filter Character vector. Filter by specific device IDs. Empty = all devices.
//' @param min_date String. Minimum date in YYYYMMDD format. Empty = no minimum.
//' @param max_date String. Maximum date in YYYYMMDD format. Empty = no maximum.
//' @param include_list Character vector. Only include records where name starts with these prefixes. Empty = no filter.
//' @param exclude_list Character vector. Exclude records where name starts with these prefixes. Empty = no filter.
//' @param exclude_addresses Character vector. Exclude these addresses entirely (all packets). Empty = no filter.
//'
//' @return A data.frame with columns:
//'   \describe{
//'     \item{address}{Bluetooth MAC address}
//'     \item{detection_count}{Total number of detections}
//'     \item{device_count}{Number of unique devices that detected this address}
//'     \item{first_seen}{First detection timestamp (YYYYMMDD-HHMMSS)}
//'     \item{last_seen}{Last detection timestamp (YYYYMMDD-HHMMSS)}
//'     \item{path}{Chronological path through devices (e.g., "16 -> 18 -> 16 -> 21")}
//'   }
//'
//' @examples
//' \dontrun{
//' paths <- get_address_paths("data/combined_sort.txt", top_n = 20)
//' paths <- get_address_paths(c("file1.txt", "file2.txt"), top_n = 50)
//' }
//'
//' @export
// [[Rcpp::export]]
DataFrame calculate_address_paths(std::vector<std::string> input_files,
                                  int top_n = 10,
                                  int progress_interval = 10000,
                                  Rcpp::Nullable<Rcpp::CharacterVector> device_filter = R_NilValue,
                                  std::string min_date = "",
                                  std::string max_date = "",
                                  Rcpp::Nullable<Rcpp::CharacterVector> include_list = R_NilValue,
                                  Rcpp::Nullable<Rcpp::CharacterVector> exclude_list = R_NilValue,
                                  Rcpp::Nullable<Rcpp::CharacterVector> exclude_addresses = R_NilValue) {

    // Convert device filter
    std::vector<std::string> device_filter_vec = nullable_to_str_vec(device_filter);
    std::unordered_set<std::string> device_set(device_filter_vec.begin(), device_filter_vec.end());
    bool filter_device = !device_filter_vec.empty();
    bool filter_min_date = !min_date.empty();
    bool filter_max_date = !max_date.empty();

    // Name include/exclude prefixes
    std::vector<std::string> include_list_vec = nullable_to_str_vec(include_list);
    std::vector<std::string> exclude_list_vec = nullable_to_str_vec(exclude_list);

    // Addresses to exclude entirely (all packets, named or not)
    std::vector<std::string> exclude_addr_vec = nullable_to_str_vec(exclude_addresses);
    std::unordered_set<std::string> exclude_addr_set(exclude_addr_vec.begin(), exclude_addr_vec.end());
    bool filter_address = !exclude_addr_vec.empty();
    
    // Per-thread accumulator: address -> vector of detections, plus counters.
    struct PathAcc {
        std::unordered_map<std::string, std::vector<Detection>> map;
        size_t lines = 0;
        size_t filtered = 0;
    };

    // Warm up the C library's timezone state on the main thread before the
    // parallel read (parse_datetime_path -> mktime initialises it lazily).
    (void) parse_datetime_path("20200101-000000");

    // Per-line work (runs on worker threads; touches only its accumulator).
    auto line_fn = [&](PathAcc& acc, const std::string& line) {
        acc.lines++;

        // Parse the four whitespace-delimited fields as views into `line`.
        std::string_view tok[4], name;
        if (wsbt::split_fields(line, tok, 4, name) < 4) {
            return;
        }
        std::string_view device = tok[0];
        std::string_view datetime = tok[1];
        std::string_view address = tok[2];
        std::string_view power_sv = tok[3];

        int power = 0;
        std::from_chars(power_sv.data(), power_sv.data() + power_sv.size(), power);

        // Drop all packets for excluded addresses (e.g. classified devices)
        if (filter_address &&
            exclude_addr_set.find(std::string(address)) != exclude_addr_set.end()) {
            acc.filtered++;
            return;
        }

        // Apply name include/exclude filtering (name is the rest of the line)
        if (!include_list_vec.empty() || !exclude_list_vec.empty()) {
            bool should_include = true;
            if (!include_list_vec.empty()) {
                should_include = wsbt::name_starts_with_any_sv(name, include_list_vec);
            }
            if (should_include && !exclude_list_vec.empty()) {
                should_include = !wsbt::name_starts_with_any_sv(name, exclude_list_vec);
            }
            if (!should_include) {
                acc.filtered++;
                return;
            }
        }

        // Apply device filter
        if (filter_device && device_set.find(std::string(device)) == device_set.end()) {
            acc.filtered++;
            return;
        }

        // Apply date filters
        std::string_view date_str = datetime.substr(0, 8);  // YYYYMMDD
        if (filter_min_date && date_str < std::string_view(min_date)) {
            acc.filtered++;
            return;
        }
        if (filter_max_date && date_str > std::string_view(max_date)) {
            acc.filtered++;
            return;
        }

        std::time_t timestamp = parse_datetime_path(datetime);
        if (timestamp == -1) {
            return;
        }

        acc.map[std::string(address)].push_back(
            Detection(std::string(device), std::string(datetime), timestamp, power));
    };

    // Fold one accumulator into another (runs on the main thread). Per-address
    // detection order is irrelevant: the post-processing sorts by timestamp.
    auto merge_fn = [](PathAcc& dst, PathAcc& src) {
        dst.lines += src.lines;
        dst.filtered += src.filtered;
        if (dst.map.empty()) {
            dst.map = std::move(src.map);
            return;
        }
        for (auto& kv : src.map) {
            auto it = dst.map.find(kv.first);
            if (it == dst.map.end()) {
                dst.map.emplace(kv.first, std::move(kv.second));
            } else {
                std::vector<Detection>& d = it->second;
                std::vector<Detection>& s = kv.second;
                d.insert(d.end(),
                         std::make_move_iterator(s.begin()),
                         std::make_move_iterator(s.end()));
            }
        }
    };

    PathAcc acc = wsbt::parallel_reduce_files<PathAcc>(input_files, line_fn, merge_fn);

    // Alias so the existing summary + path-building code is unchanged.
    std::unordered_map<std::string, std::vector<Detection>>& address_detections = acc.map;
    size_t total_lines = acc.lines;
    size_t filtered_lines = acc.filtered;
    
    Rcout << "Total lines processed: " << total_lines << "\n";
    if (filtered_lines > 0) {
        Rcout << "Lines filtered out: " << filtered_lines << "\n";
    }
    Rcout << "Unique addresses found: " << address_detections.size() << "\n";
    
    // Sort detections for each address by timestamp and build paths
    struct AddressInfo {
        std::string address;
        int detection_count;
        int device_count;
        std::string first_seen;
        std::string last_seen;
        std::string path;
    };
    
    // Snapshot the map entries into an index-addressable vector so the
    // per-address work (independent for each address) can run in parallel.
    // The pointers stay valid because address_detections is not modified below.
    std::vector<std::pair<const std::string, std::vector<Detection>>*> entries;
    entries.reserve(address_detections.size());
    for (auto& pair : address_detections) {
        entries.push_back(&pair);
    }

    // One output slot per address; each iteration writes only its own slot, so
    // no locking is needed. No R API may be called inside the parallel region.
    std::vector<AddressInfo> address_info(entries.size());

    // Last chance to interrupt before the parallel region: checkUserInterrupt()
    // is not thread-safe, so it cannot run inside the loop.
    Rcpp::checkUserInterrupt();

    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic, 256)
    #endif
    for (std::ptrdiff_t idx = 0; idx < static_cast<std::ptrdiff_t>(entries.size()); idx++) {
        const std::string& address = entries[idx]->first;
        std::vector<Detection>& detections = entries[idx]->second;

        // Sort by timestamp
        std::sort(detections.begin(), detections.end(),
                  [](const Detection& a, const Detection& b) {
                      return a.timestamp < b.timestamp;
                  });
        
        // Remove duplicates: when same timestamp, keep only highest power
        std::vector<Detection> filtered_detections;
        for (size_t i = 0; i < detections.size(); i++) {
            // Find all detections with same timestamp
            size_t j = i;
            while (j < detections.size() && detections[j].timestamp == detections[i].timestamp) {
                j++;
            }
            
            // If multiple detections at same time, find highest power
            if (j - i > 1) {
                size_t best_idx = i;
                for (size_t k = i + 1; k < j; k++) {
                    if (detections[k].power > detections[best_idx].power) {
                        best_idx = k;
                    }
                }
                filtered_detections.push_back(detections[best_idx]);
            } else {
                filtered_detections.push_back(detections[i]);
            }
            
            i = j - 1;  // Skip processed detections
        }
        
        // Build path (showing device transitions)
        // Remove cycles: if we see A -> B -> A within a short time window, simplify to A
        std::vector<std::pair<std::string, std::time_t>> path_nodes;  // device, timestamp
        std::unordered_set<std::string> unique_devices;
        
        for (const auto& det : filtered_detections) {
            unique_devices.insert(det.device);
            
            // Only add to path if device changed
            if (path_nodes.empty() || det.device != path_nodes.back().first) {
                path_nodes.push_back(std::make_pair(det.device, det.timestamp));
            }
        }
        
        // Remove short cycles (A -> B -> A where B appears for < 5 minutes)
        std::vector<std::string> simplified_path;
        simplified_path.reserve(path_nodes.size());
        
        for (size_t i = 0; i < path_nodes.size(); i++) {
            // Check if this creates a cycle with previous node
            if (i >= 2 && 
                path_nodes[i].first == path_nodes[i-2].first &&
                path_nodes[i].first != path_nodes[i-1].first) {
                
                // Check time difference: if middle node was brief (< 180 seconds), skip it
                std::time_t time_at_middle = path_nodes[i-1].second;
                std::time_t time_return = path_nodes[i].second;
                
                if (time_return - time_at_middle < 180) {  // 3 minutes
                    // Remove the last added node (the middle of the cycle)
                    if (!simplified_path.empty()) {
                        simplified_path.pop_back();
                    }
                    // Don't add current node (we stay at the original)
                    continue;
                }
            }
            
            simplified_path.push_back(path_nodes[i].first);
        }
        
        // Build path string
        std::string path;
        for (size_t i = 0; i < simplified_path.size(); i++) {
            if (i > 0) {
                path += " -> ";
            }
            path += simplified_path[i];
        }
        
        AddressInfo info;
        info.address = address;
        info.detection_count = filtered_detections.size();
        info.device_count = unique_devices.size();
        info.first_seen = filtered_detections.front().datetime;
        info.last_seen = filtered_detections.back().datetime;
        info.path = path;

        address_info[idx] = info;
    }
    
    // Sort by detection count (descending) and take top N
    std::sort(address_info.begin(), address_info.end(),
              [](const AddressInfo& a, const AddressInfo& b) {
                  return a.detection_count > b.detection_count;
              });
    
    // Take top N
    if (address_info.size() > static_cast<size_t>(top_n)) {
        address_info.resize(top_n);
    }
    
    Rcout << "Returning top " << address_info.size() << " addresses\n";
    
    // Convert to DataFrame
    std::vector<std::string> out_addresses, out_first_seen, out_last_seen, out_paths;
    std::vector<int> out_detection_counts, out_device_counts;
    
    for (const auto& info : address_info) {
        out_addresses.push_back(info.address);
        out_detection_counts.push_back(info.detection_count);
        out_device_counts.push_back(info.device_count);
        out_first_seen.push_back(info.first_seen);
        out_last_seen.push_back(info.last_seen);
        out_paths.push_back(info.path);
    }
    
    return DataFrame::create(
        Named("address") = out_addresses,
        Named("detection_count") = out_detection_counts,
        Named("device_count") = out_device_counts,
        Named("first_seen") = out_first_seen,
        Named("last_seen") = out_last_seen,
        Named("path") = out_paths,
        _["stringsAsFactors"] = false
    );
}
