#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cctype>

#include "parallel_read.h"

using namespace Rcpp;

// Aggregated information for one matched address.
struct AddressMatch {
    std::string name;           // representative (first-seen) matching name
    std::string matched_prefix; // prefix that matched
    int hits;                   // number of matching (named) records
    std::string first_seen;     // earliest matching datetime (lexicographic)
    std::string last_seen;      // latest matching datetime (lexicographic)

    AddressMatch() : hits(0) {}
};

//' Find Bluetooth addresses whose advertised name matches a prefix
//'
//' Single pass over the raw data. For every record whose advertised name starts
//' with one of \code{prefixes}, the address is recorded together with the prefix
//' that matched. This promotes a per-packet name match to a per-address match, so
//' that downstream steps can act on \emph{all} packets (named or not) for a
//' device identified as a beacon, vehicle, bike, etc.
//'
//' @param input_files Character vector of file paths to process.
//' @param prefixes Character vector of name prefixes to match.
//' @param progress_interval Integer. How often to print progress (0 = no progress).
//' @param device_filter Character vector. Restrict to these device IDs. Empty = all.
//' @param min_date String. Minimum date in YYYYMMDD format. Empty = no minimum.
//' @param max_date String. Maximum date in YYYYMMDD format. Empty = no maximum.
//'
//' @return A data.frame with one row per matched address: \code{address},
//'   \code{name}, \code{matched_prefix}, \code{hits}, \code{first_seen},
//'   \code{last_seen}.
//'
//' @keywords internal
// [[Rcpp::export]]
DataFrame find_matching_addresses(std::vector<std::string> input_files,
                                  std::vector<std::string> prefixes,
                                  int progress_interval = 10000,
                                  Rcpp::Nullable<Rcpp::CharacterVector> device_filter = R_NilValue,
                                  std::string min_date = "",
                                  std::string max_date = "") {

    if (prefixes.empty()) {
        stop("prefixes must contain at least one prefix");
    }

    // Device filter setup
    std::vector<std::string> device_filter_vec;
    if (device_filter.isNotNull()) {
        CharacterVector cv = as<CharacterVector>(device_filter);
        for (int i = 0; i < cv.size(); i++) device_filter_vec.push_back(as<std::string>(cv[i]));
    }
    std::unordered_set<std::string> device_set(device_filter_vec.begin(), device_filter_vec.end());
    bool filter_device = !device_filter_vec.empty();
    bool filter_min_date = !min_date.empty();
    bool filter_max_date = !max_date.empty();

    // Per-thread accumulator: address -> AddressMatch, plus counters.
    struct MatchAcc {
        std::unordered_map<std::string, AddressMatch> map;
        size_t lines = 0;
        size_t matched = 0;
    };

    // Per-line work (runs on worker threads; touches only its accumulator).
    auto line_fn = [&](MatchAcc& acc, const std::string& line) {
        acc.lines++;

        // Parse: device datetime address power [name]; fields are views into `line`.
        std::string_view tok[4], rest;
        if (wsbt::split_fields(line, tok, 4, rest) < 4) {
            return;
        }
        std::string_view device = tok[0];
        std::string_view datetime = tok[1];
        std::string_view address = tok[2];

        // The name is the trimmed remainder of the line (may be empty).
        std::string_view name = wsbt::trim_view(rest);
        if (name.empty()) {
            return;  // no name -> nothing to match on this pass
        }

        // Device filter
        if (filter_device && device_set.find(std::string(device)) == device_set.end()) {
            return;
        }
        // Date filters (datetime is YYYYMMDD-HHMMSS)
        if (datetime.length() >= 8) {
            std::string_view date_str = datetime.substr(0, 8);
            if (filter_min_date && date_str < std::string_view(min_date)) return;
            if (filter_max_date && date_str > std::string_view(max_date)) return;
        }

        int idx = wsbt::first_prefix_index_sv(name, prefixes);
        if (idx < 0) {
            return;
        }

        acc.matched++;
        std::string dt(datetime);
        auto& info = acc.map[std::string(address)];
        if (info.hits == 0) {
            info.name.assign(name.data(), name.size());
            info.matched_prefix = prefixes[idx];
            info.first_seen = dt;
            info.last_seen = dt;
        } else {
            if (dt < info.first_seen) info.first_seen = dt;
            if (dt > info.last_seen) info.last_seen = dt;
        }
        info.hits++;
    };

    // Fold one accumulator into another (runs on the main thread). The
    // representative name/prefix is taken from the match with the earliest
    // first_seen; ties keep the existing one.
    auto merge_fn = [](MatchAcc& dst, MatchAcc& src) {
        dst.lines += src.lines;
        dst.matched += src.matched;
        if (dst.map.empty()) {
            dst.map = std::move(src.map);
            return;
        }
        for (auto& kv : src.map) {
            auto it = dst.map.find(kv.first);
            if (it == dst.map.end()) {
                dst.map.emplace(kv.first, std::move(kv.second));
            } else {
                AddressMatch& d = it->second;
                AddressMatch& s = kv.second;
                if (s.first_seen < d.first_seen) {
                    d.name = std::move(s.name);
                    d.matched_prefix = std::move(s.matched_prefix);
                    d.first_seen = s.first_seen;
                }
                if (s.last_seen > d.last_seen) {
                    d.last_seen = s.last_seen;
                }
                d.hits += s.hits;
            }
        }
    };

    MatchAcc acc = wsbt::parallel_reduce_files<MatchAcc>(input_files, line_fn, merge_fn);

    // Alias so the existing summary + DataFrame-construction code is unchanged.
    std::unordered_map<std::string, AddressMatch>& matches = acc.map;
    size_t total_lines = acc.lines;
    size_t matched_lines = acc.matched;

    Rcout << "Total lines processed: " << total_lines << "\n";
    Rcout << "Matching (named) records: " << matched_lines << "\n";
    Rcout << "Matched addresses found: " << matches.size() << "\n";

    std::vector<std::string> out_address, out_name, out_prefix, out_first, out_last;
    std::vector<int> out_hits;
    out_address.reserve(matches.size());
    out_name.reserve(matches.size());
    out_prefix.reserve(matches.size());
    out_first.reserve(matches.size());
    out_last.reserve(matches.size());
    out_hits.reserve(matches.size());

    for (const auto& pair : matches) {
        out_address.push_back(pair.first);
        out_name.push_back(pair.second.name);
        out_prefix.push_back(pair.second.matched_prefix);
        out_hits.push_back(pair.second.hits);
        out_first.push_back(pair.second.first_seen);
        out_last.push_back(pair.second.last_seen);
    }

    return DataFrame::create(
        Named("address") = out_address,
        Named("name") = out_name,
        Named("matched_prefix") = out_prefix,
        Named("hits") = out_hits,
        Named("first_seen") = out_first,
        Named("last_seen") = out_last,
        _["stringsAsFactors"] = false
    );
}
