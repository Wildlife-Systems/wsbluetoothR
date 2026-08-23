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

#include "parallel_read.h"
#include "datetime_parse.h"

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

// Structure to store time range and detection count
struct TimeRange {
    std::time_t first_seen;
    std::time_t last_seen;
    int count;
    std::string device;
    std::string date;
    std::string address;
    std::string first_seen_str;
    std::string last_seen_str;
    
    TimeRange() : first_seen(0), last_seen(0), count(0) {}
};

// Parse datetime string in format YYYYMMDD-HHMMSS to time_t
std::time_t parse_datetime(std::string_view datetime_str) {
    return wsbt_parse_datetime(datetime_str);
}

// Extract date string in format YYYY-MM-DD from datetime string
std::string extract_date(std::string_view datetime_str) {
    if (datetime_str.length() < 8) {
        return "";
    }

    std::string out;
    out.reserve(10);
    out.append(datetime_str.data(), 4);       // YYYY
    out.push_back('-');
    out.append(datetime_str.data() + 4, 2);   // MM
    out.push_back('-');
    out.append(datetime_str.data() + 6, 2);   // DD
    return out;
}

// Scan files to find all unique devices (parallel, allocation-free parsing).
std::vector<std::string> scan_unique_devices(const std::vector<std::string>& input_files,
                                             const std::vector<std::string>& device_filter_vec,
                                             const std::string& min_date,
                                             const std::string& max_date) {
    std::unordered_set<std::string> device_filter_set(device_filter_vec.begin(), device_filter_vec.end());
    bool filter_device = !device_filter_vec.empty();
    bool filter_min_date = !min_date.empty();
    bool filter_max_date = !max_date.empty();

    struct ScanAcc { std::unordered_set<std::string> devices; };

    auto line_fn = [&](ScanAcc& acc, const std::string& line) {
        std::string_view tok[2], rest;
        if (wsbt::split_fields(line, tok, 2, rest) < 2) return;
        std::string_view device = tok[0];
        std::string_view datetime = tok[1];

        if (filter_device &&
            device_filter_set.find(std::string(device)) == device_filter_set.end()) {
            return;
        }
        std::string_view date_str = datetime.substr(0, 8);
        if (filter_min_date && date_str < std::string_view(min_date)) return;
        if (filter_max_date && date_str > std::string_view(max_date)) return;

        acc.devices.emplace(device);
    };

    auto merge_fn = [](ScanAcc& dst, ScanAcc& src) {
        if (dst.devices.empty()) {
            dst.devices = std::move(src.devices);
            return;
        }
        for (const auto& d : src.devices) dst.devices.insert(d);
    };

    ScanAcc acc = wsbt::parallel_reduce_files<ScanAcc>(input_files, line_fn, merge_fn);
    return std::vector<std::string>(acc.devices.begin(), acc.devices.end());
}

// Extract hour string from datetime in format YYYYMMDD-HHMMSS to YYYY-MM-DD-HH
std::string extract_datetime_hour(const std::string& datetime_str) {
    if (datetime_str.length() < 11) {
        return "";
    }
    
    std::string year = datetime_str.substr(0, 4);
    std::string month = datetime_str.substr(4, 2);
    std::string day = datetime_str.substr(6, 2);
    std::string hour = datetime_str.substr(9, 2);
    
    return year + "-" + month + "-" + day + "-" + hour;
}

//' Calculate Address Duration per Device and Day
//'
//' Calculates the time duration (in seconds) that each Bluetooth address
//' was detected by each device on each day. The duration is calculated as
//' the time difference between the first and last detection of that address
//' on that device on that day.
//'
//' @param input_files Character vector of file paths to process.
//' @param progress_interval Integer. How often to print progress (0 = no progress). Default is 10000.
//' @param device_filter Character vector. Filter by specific device IDs. Empty = all devices.
//' @param min_date String. Minimum date in YYYYMMDD format. Empty = no minimum.
//' @param max_date String. Maximum date in YYYYMMDD format. Empty = no maximum.
//' @param include_list Character vector. Only include records where name starts with these prefixes.
//' @param exclude_list Character vector. Exclude records where name starts with these prefixes.
//' @param low_memory Logical. If TRUE, processes devices in batches (default 4 per pass, set via the \code{WSBT_DEVICES_PER_PASS} environment variable) to reduce peak memory, reading the input once per batch. Default is FALSE.
//' @param exclude_addresses Character vector. Exclude these addresses entirely (all packets). Empty = no filter.
//'
//' @return A data.frame with columns:
//'   \describe{
//'     \item{device}{Device ID}
//'     \item{date}{Date in YYYY-MM-DD format}
//'     \item{address}{Bluetooth MAC address}
//'     \item{first_seen}{Timestamp of first detection (YYYYMMDD-HHMMSS)}
//'     \item{last_seen}{Timestamp of last detection (YYYYMMDD-HHMMSS)}
//'     \item{duration_seconds}{Time duration in seconds between first and last detection}
//'     \item{detection_count}{Number of times this address was detected}
//'   }
//'
//' @examples
//' \dontrun{
//' durations <- calculate_address_duration("data/combined_sort.txt")
//' # Multiple files
//' durations <- calculate_address_duration(c("file1.txt", "file2.txt"))
//' }
//'
//' @export
// [[Rcpp::export]]
DataFrame calculate_address_duration(std::vector<std::string> input_files,
                                     int progress_interval = 10000,
                                     Rcpp::Nullable<Rcpp::CharacterVector> device_filter = R_NilValue,
                                     std::string min_date = "",
                                     std::string max_date = "",
                                     Rcpp::Nullable<Rcpp::CharacterVector> include_list = R_NilValue,
                                     Rcpp::Nullable<Rcpp::CharacterVector> exclude_list = R_NilValue,
                                     bool low_memory = false,
                                     Rcpp::Nullable<Rcpp::CharacterVector> exclude_addresses = R_NilValue) {

    // Convert Rcpp vectors to std::vector (NULL default -> empty)
    std::vector<std::string> device_filter_vec = nullable_to_str_vec(device_filter);
    std::vector<std::string> include_list_vec = nullable_to_str_vec(include_list);
    std::vector<std::string> exclude_list_vec = nullable_to_str_vec(exclude_list);

    // Addresses to exclude entirely (all packets, named or not)
    std::vector<std::string> exclude_addr_vec = nullable_to_str_vec(exclude_addresses);
    std::unordered_set<std::string> exclude_addr_set(exclude_addr_vec.begin(), exclude_addr_vec.end());
    bool filter_address = !exclude_addr_vec.empty();
    
    // Low-memory mode: process the devices in batches to bound peak memory,
    // reading the files once per batch instead of once per device.
    if (low_memory) {
        Rcout << "Low-memory mode: scanning for unique devices...\n";
        R_FlushConsole();

        // First scan to find all unique devices
        std::vector<std::string> unique_devices = scan_unique_devices(input_files, device_filter_vec, min_date, max_date);

        size_t per_pass = static_cast<size_t>(wsbt::devices_per_pass());
        size_t n_passes = per_pass > 0 ? (unique_devices.size() + per_pass - 1) / per_pass : 0;

        Rcout << "Found " << unique_devices.size() << " unique device(s); "
              << per_pass << " per pass (" << n_passes << " pass(es))...\n";
        R_FlushConsole();

        // Prepare result vectors
        std::vector<std::string> all_devices, all_dates, all_addresses, all_first_seen, all_last_seen;
        std::vector<double> all_durations;
        std::vector<int> all_counts;

        // Process a batch of devices per pass
        for (size_t start = 0; start < unique_devices.size(); start += per_pass) {
            size_t end = std::min(start + per_pass, unique_devices.size());

            Rcpp::CharacterVector batch(end - start);
            for (size_t i = start; i < end; i++) {
                batch[i - start] = unique_devices[i];
            }

            Rcout << "Processing pass " << (start / per_pass + 1) << "/" << n_passes
                  << " (" << (end - start) << " device(s))...\n";
            R_FlushConsole();

            // Process this batch (recursive call with low_memory=false)
            DataFrame device_result = calculate_address_duration(
                input_files, 0,  // progress_interval = 0 for inner calls
                Rcpp::Nullable<Rcpp::CharacterVector>(batch), min_date, max_date,
                include_list, exclude_list, false,  // low_memory = false
                exclude_addresses
            );

            // Append results
            CharacterVector dev_vec = device_result["device"];
            CharacterVector date_vec = device_result["date"];
            CharacterVector addr_vec = device_result["address"];
            CharacterVector first_vec = device_result["first_seen"];
            CharacterVector last_vec = device_result["last_seen"];
            NumericVector dur_vec = device_result["duration_seconds"];
            IntegerVector cnt_vec = device_result["detection_count"];

            for (int i = 0; i < dev_vec.size(); i++) {
                all_devices.push_back(as<std::string>(dev_vec[i]));
                all_dates.push_back(as<std::string>(date_vec[i]));
                all_addresses.push_back(as<std::string>(addr_vec[i]));
                all_first_seen.push_back(as<std::string>(first_vec[i]));
                all_last_seen.push_back(as<std::string>(last_vec[i]));
                all_durations.push_back(dur_vec[i]);
                all_counts.push_back(cnt_vec[i]);
            }
        }

        Rcout << "Low-memory processing complete. Total records: " << all_devices.size() << "\n";
        
        return DataFrame::create(
            Named("device") = all_devices,
            Named("date") = all_dates,
            Named("address") = all_addresses,
            Named("first_seen") = all_first_seen,
            Named("last_seen") = all_last_seen,
            Named("duration_seconds") = all_durations,
            Named("detection_count") = all_counts,
            _["stringsAsFactors"] = false
        );
    }
    
    // Normal mode: process all files at once, reading them in parallel.

    // Setup filters
    std::unordered_set<std::string> device_set(device_filter_vec.begin(), device_filter_vec.end());
    bool filter_device = !device_filter_vec.empty();
    bool filter_min_date = !min_date.empty();
    bool filter_max_date = !max_date.empty();

    // Per-thread accumulator: device_date_address -> TimeRange, plus counters.
    struct DurAcc {
        std::unordered_map<std::string, TimeRange> map;
        size_t lines = 0;
        size_t filtered = 0;
    };

    // Warm up the C library's timezone state on the main thread: parse_datetime
    // -> mktime initialises it lazily, and that must not first happen from
    // several worker threads at once.
    (void) parse_datetime("20200101-000000");

    // Per-line work (runs on worker threads; touches only its accumulator).
    auto line_fn = [&](DurAcc& acc, const std::string& line) {
        acc.lines++;

        // Parse the four whitespace-delimited fields as views into `line`.
        std::string_view tok[4], name;
        if (wsbt::split_fields(line, tok, 4, name) < 4) {
            return;  // Skip malformed lines
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

        // Apply name filtering (name is the remainder of the line)
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

        // Parse datetime to time_t
        std::time_t timestamp = parse_datetime(datetime);
        if (timestamp == -1) {
            return;  // Skip invalid datetime
        }

        std::string date = extract_date(datetime);
        if (date.empty()) {
            return;
        }

        // Create key: device_date_address
        std::string key;
        key.reserve(device.size() + date.size() + address.size() + 2);
        key.append(device.data(), device.size());
        key.push_back('_');
        key.append(date);
        key.push_back('_');
        key.append(address.data(), address.size());

        auto& entry = acc.map[std::move(key)];
        if (entry.count == 0) {
            // First detection - store parsed components
            entry.first_seen = timestamp;
            entry.last_seen = timestamp;
            entry.count = 1;
            entry.device.assign(device.data(), device.size());
            entry.date = date;
            entry.address.assign(address.data(), address.size());

            char buf[20];
            std::tm tmv = wsbt::localtime_safe(timestamp);
            std::strftime(buf, sizeof(buf), "%Y%m%d-%H%M%S", &tmv);
            entry.first_seen_str = std::string(buf);
            entry.last_seen_str = entry.first_seen_str;
        } else {
            if (timestamp < entry.first_seen) {
                entry.first_seen = timestamp;
                char buf[20];
                std::tm tmv = wsbt::localtime_safe(timestamp);
                std::strftime(buf, sizeof(buf), "%Y%m%d-%H%M%S", &tmv);
                entry.first_seen_str = std::string(buf);
            }
            if (timestamp > entry.last_seen) {
                entry.last_seen = timestamp;
                char buf[20];
                std::tm tmv = wsbt::localtime_safe(timestamp);
                std::strftime(buf, sizeof(buf), "%Y%m%d-%H%M%S", &tmv);
                entry.last_seen_str = std::string(buf);
            }
            entry.count++;
        }
    };

    // Fold one accumulator into another (runs on the main thread).
    auto merge_fn = [](DurAcc& dst, DurAcc& src) {
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
                TimeRange& d = it->second;
                TimeRange& s = kv.second;
                if (s.first_seen < d.first_seen) {
                    d.first_seen = s.first_seen;
                    d.first_seen_str = std::move(s.first_seen_str);
                }
                if (s.last_seen > d.last_seen) {
                    d.last_seen = s.last_seen;
                    d.last_seen_str = std::move(s.last_seen_str);
                }
                d.count += s.count;
            }
        }
    };

    DurAcc acc = wsbt::parallel_reduce_files<DurAcc>(input_files, line_fn, merge_fn);

    // Alias so the existing summary + DataFrame-construction code is unchanged.
    std::unordered_map<std::string, TimeRange>& duration_map = acc.map;
    size_t total_lines = acc.lines;
    size_t filtered_lines = acc.filtered;
    
    Rcout << "Total lines processed: " << total_lines << "\n";
    if (filtered_lines > 0) {
        Rcout << "Lines filtered out: " << filtered_lines << "\n";
    }
    Rcout << "Unique records: " << duration_map.size() << "\n";
    
    // Convert map to DataFrame
    std::vector<std::string> devices, dates, addresses, first_seen_strs, last_seen_strs;
    std::vector<double> duration_seconds;
    std::vector<int> detection_counts;
    
    devices.reserve(duration_map.size());
    dates.reserve(duration_map.size());
    addresses.reserve(duration_map.size());
    first_seen_strs.reserve(duration_map.size());
    last_seen_strs.reserve(duration_map.size());
    duration_seconds.reserve(duration_map.size());
    detection_counts.reserve(duration_map.size());
    
    size_t counter = 0;
    for (const auto& pair : duration_map) {
        // Check for user interrupts periodically
        if (++counter % 10000 == 0) {
            Rcpp::checkUserInterrupt();
        }
        
        const TimeRange& tr = pair.second;
        
        // Calculate duration in seconds
        double duration = std::difftime(tr.last_seen, tr.first_seen);
        
        // Move pre-formatted data (no parsing or formatting needed!)
        devices.push_back(std::move(tr.device));
        dates.push_back(std::move(tr.date));
        addresses.push_back(std::move(tr.address));
        first_seen_strs.push_back(std::move(tr.first_seen_str));
        last_seen_strs.push_back(std::move(tr.last_seen_str));
        duration_seconds.push_back(duration);
        detection_counts.push_back(tr.count);
    }
    
    // Clear map to free memory before creating DataFrame
    duration_map.clear();
    
    return DataFrame::create(
        Named("device") = devices,
        Named("date") = dates,
        Named("address") = addresses,
        Named("first_seen") = first_seen_strs,
        Named("last_seen") = last_seen_strs,
        Named("duration_seconds") = duration_seconds,
        Named("detection_count") = detection_counts,
        _["stringsAsFactors"] = false
    );
}

//' Calculate Average Address Duration per Device and Time Period
//'
//' Calculates statistics (median, mean, etc.) for address durations across all Bluetooth addresses
//' for each device grouped by time period (hour or day). This gives you the typical
//' detection duration for a device in each time window.
//'
//' @param input_files Character vector of file paths to process.
//' @param progress_interval Integer. How often to print progress (0 = no progress). Default is 10000.
//' @param device_filter Character vector. Filter by specific device IDs. Empty = all devices.
//' @param min_date String. Minimum date in YYYYMMDD format. Empty = no minimum.
//' @param max_date String. Maximum date in YYYYMMDD format. Empty = no maximum.
//' @param include_list Character vector. Only include records where name starts with these prefixes.
//' @param exclude_list Character vector. Exclude records where name starts with these prefixes.
//' @param low_memory Logical. If TRUE, processes devices in batches (default 4 per pass, set via the \code{WSBT_DEVICES_PER_PASS} environment variable) to reduce peak memory, reading the input once per batch. Default is FALSE.
//' @param time_group String. Time grouping: "day" or "hour". Default is "day".
//' @param exclude_addresses Character vector. Exclude these addresses entirely (all packets). Empty = no filter.
//'
//' @return A data.frame with columns:
//'   \describe{
//'     \item{device}{Device ID}
//'     \item{datetime}{Time period (YYYY-MM-DD for day, YYYY-MM-DD-HH for hour)}
//'     \item{median_duration_seconds}{Median duration across all addresses in this period}
//'     \item{mean_duration_seconds}{Mean duration across all addresses in this period}
//'     \item{min_duration_seconds}{Minimum duration}
//'     \item{max_duration_seconds}{Maximum duration}
//'     \item{address_count}{Number of unique addresses detected}
//'     \item{total_detections}{Total detection count}
//'   }
//'
//' @export
// [[Rcpp::export]]
DataFrame calculate_average_address_duration(std::vector<std::string> input_files,
                                           int progress_interval = 10000,
                                           Rcpp::Nullable<Rcpp::CharacterVector> device_filter = R_NilValue,
                                           std::string min_date = "",
                                           std::string max_date = "",
                                           Rcpp::Nullable<Rcpp::CharacterVector> include_list = R_NilValue,
                                           Rcpp::Nullable<Rcpp::CharacterVector> exclude_list = R_NilValue,
                                           bool low_memory = false,
                                           std::string time_group = "day",
                                           Rcpp::Nullable<Rcpp::CharacterVector> exclude_addresses = R_NilValue) {

    // First get daily durations using existing function
    DataFrame daily = calculate_address_duration(input_files, progress_interval,
                                                 device_filter, min_date, max_date,
                                                 include_list, exclude_list, low_memory,
                                                 exclude_addresses);
    
    // Extract columns
    std::vector<std::string> devices = as<std::vector<std::string>>(daily["device"]);
    std::vector<std::string> dates = as<std::vector<std::string>>(daily["date"]);
    std::vector<std::string> addresses = as<std::vector<std::string>>(daily["address"]);
    std::vector<std::string> first_seen_strs = as<std::vector<std::string>>(daily["first_seen"]);
    std::vector<double> durations = as<std::vector<double>>(daily["duration_seconds"]);
    std::vector<int> counts = as<std::vector<int>>(daily["detection_count"]);
    
    // Build map: device_datetime -> vector of durations
    std::unordered_map<std::string, std::vector<double>> duration_map;
    std::unordered_map<std::string, std::unordered_set<std::string>> address_map;
    std::unordered_map<std::string, int> detection_map;
    
    for (size_t i = 0; i < devices.size(); i++) {
        // Check for user interrupts periodically
        if (i % 10000 == 0) {
            Rcpp::checkUserInterrupt();
        }
        
        // Build key based on time_group
        std::string time_key;
        if (time_group == "hour") {
            time_key = extract_datetime_hour(first_seen_strs[i]);
        } else {
            time_key = dates[i];
        }
        
        std::string key = devices[i] + "_" + time_key;
        duration_map[key].push_back(durations[i]);
        address_map[key].insert(addresses[i]);
        detection_map[key] += counts[i];
    }
    
    // Calculate statistics for each device-time period pair.
    // Snapshot duration_map into an index-addressable vector so each key's
    // statistics (independent of every other key) can be computed in parallel.
    std::vector<const std::pair<const std::string, std::vector<double>>*> stat_entries;
    stat_entries.reserve(duration_map.size());
    for (const auto& pair : duration_map) {
        stat_entries.push_back(&pair);
    }

    size_t n_entries = stat_entries.size();
    std::vector<std::string> out_devices(n_entries), out_datetimes(n_entries);
    std::vector<double> out_median(n_entries), out_mean(n_entries), out_min(n_entries), out_max(n_entries);
    std::vector<int> out_address_count(n_entries), out_detections(n_entries);

    // Last chance to interrupt before the parallel region.
    Rcpp::checkUserInterrupt();

    // address_map / detection_map are only read here, via at() (never
    // operator[], which would insert). The standard treats at() as const for
    // data-race purposes, so concurrent reads are safe. Each iteration writes
    // only its own output slots, and no R API is touched inside the loop.
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic, 256)
    #endif
    for (std::ptrdiff_t i = 0; i < static_cast<std::ptrdiff_t>(n_entries); i++) {
        const std::string& key = stat_entries[i]->first;
        const std::vector<double>& vals = stat_entries[i]->second;

        // Parse key: device_datetime
        size_t underscore = key.find('_');
        std::string device = key.substr(0, underscore);
        std::string datetime = key.substr(underscore + 1);

        // Calculate median
        std::vector<double> sorted_vals = vals;
        std::sort(sorted_vals.begin(), sorted_vals.end());
        double median;
        size_t n = sorted_vals.size();
        if (n % 2 == 0) {
            median = (sorted_vals[n/2 - 1] + sorted_vals[n/2]) / 2.0;
        } else {
            median = sorted_vals[n/2];
        }

        // Calculate mean
        double sum = 0;
        for (double v : vals) sum += v;
        double mean = sum / vals.size();

        // Min and max
        double min_val = *std::min_element(vals.begin(), vals.end());
        double max_val = *std::max_element(vals.begin(), vals.end());

        // Store results
        out_devices[i] = device;
        out_datetimes[i] = datetime;
        out_median[i] = median;
        out_mean[i] = mean;
        out_min[i] = min_val;
        out_max[i] = max_val;
        out_address_count[i] = static_cast<int>(address_map.at(key).size());
        out_detections[i] = detection_map.at(key);
    }
    
    // Create DataFrame
    DataFrame result = DataFrame::create(
        Named("device") = out_devices,
        Named("datetime") = out_datetimes,
        Named("median_duration_seconds") = out_median,
        Named("mean_duration_seconds") = out_mean,
        Named("min_duration_seconds") = out_min,
        Named("max_duration_seconds") = out_max,
        Named("address_count") = out_address_count,
        Named("total_detections") = out_detections,
        _["stringsAsFactors"] = false
    );
    
    return result;
}
