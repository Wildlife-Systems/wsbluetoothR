#ifndef WSBT_PARALLEL_READ_H
#define WSBT_PARALLEL_READ_H

// Shared infrastructure for reading the raw Bluetooth text files in parallel.
//
// Every heavy reader in this package follows the same shape: stream one or more
// large text files line by line, parse/filter each line, and fold it into a
// hash-map/set accumulator. That read phase is the bottleneck on the real data
// (single files of 5-25 GB). This header parallelises it by splitting the input
// into byte-range chunks, letting each thread build its own accumulator, and
// merging the per-thread accumulators on the main thread afterwards.
//
// Boundary handling (classic MapReduce line split): each chunk covers a byte
// range [start, end]. A chunk with start > 0 discards its first (partial) line,
// because that line is owned by the previous chunk, which reads one line past
// its own `end`. A line is processed by the chunk iff its starting offset is
// <= end. Adjacent chunks share the boundary value (end_k == start_{k+1}), so a
// line beginning exactly on the boundary is processed by the earlier chunk and
// discarded by the later one -- never lost, never doubled.
//
// Thread-safety rules for callers:
//   * The per-line functor is shared across threads and called concurrently, so
//     it must be const (no `mutable`) and must only read its captures and write
//     to the accumulator it is handed. No R API (Rcout, checkUserInterrupt,
//     Rcpp object construction) may be touched from inside it.
//   * Interrupt checks and all Rcpp/R work happen on the main thread, between
//     parallel regions.

#include <Rcpp.h>
#include <string>
#include <string_view>
#include <vector>
#include <fstream>
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <ctime>

#ifdef _OPENMP
#include <omp.h>
#else
static inline int omp_get_thread_num() { return 0; }
static inline int omp_get_max_threads() { return 1; }
#endif

namespace wsbt {

// Thread-safe replacement for std::localtime. On POSIX, std::localtime returns a
// pointer into a shared static buffer and cannot be used from multiple threads;
// localtime_r fills a caller-owned struct instead. On Windows the CRT's
// localtime uses a per-thread buffer, so copying its result out is safe to do
// concurrently.
inline std::tm localtime_safe(std::time_t t) {
    std::tm out;
    std::memset(&out, 0, sizeof(out));
#if defined(_WIN32)
    std::tm* p = std::localtime(&t);
    if (p) out = *p;
#else
    localtime_r(&t, &out);
#endif
    return out;
}

// ---- fast, allocation-free line tokenising ---------------------------------
//
// The raw records are "device datetime address power [name]" with the name being
// the whitespace-preceded remainder of the line. These helpers replace the old
// std::istringstream + per-field std::string parsing (which allocated ~6 strings
// per line and dominated the read cost) with views into the line itself.

inline bool is_field_ws(char c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f' || c == '\v';
}

// Split `line` into up to `max_tok` whitespace-delimited tokens (as views into
// `line`) and set `rest` to everything following the last token's terminator,
// matching `iss >> t0 >> t1 >> ...; std::getline(iss, rest);`. Returns the number
// of tokens found; `rest` is only meaningful once that reaches max_tok.
inline int split_fields(const std::string& line, std::string_view* tok, int max_tok,
                        std::string_view& rest) {
    const size_t n = line.size();
    const char* d = line.data();
    size_t i = 0;
    int count = 0;
    while (count < max_tok) {
        while (i < n && is_field_ws(d[i])) i++;
        if (i >= n) break;
        size_t start = i;
        while (i < n && !is_field_ws(d[i])) i++;
        tok[count++] = std::string_view(d + start, i - start);
    }
    if (count == max_tok) rest = std::string_view(d + i, n - i);
    return count;
}

inline std::string_view ltrim_view(std::string_view s) {
    size_t a = 0;
    while (a < s.size() && is_field_ws(s[a])) a++;
    return s.substr(a);
}

inline std::string_view trim_view(std::string_view s) {
    size_t a = 0, b = s.size();
    while (a < b && is_field_ws(s[a])) a++;
    while (b > a && is_field_ws(s[b - 1])) b--;
    return s.substr(a, b - a);
}

inline bool sv_starts_with(std::string_view s, std::string_view p) {
    return s.size() >= p.size() && s.compare(0, p.size(), p) == 0;
}

// Does `name` (after trimming leading whitespace) start with any of `prefixes`?
inline bool name_starts_with_any_sv(std::string_view name,
                                    const std::vector<std::string>& prefixes) {
    if (prefixes.empty()) return false;
    std::string_view t = ltrim_view(name);
    for (const auto& p : prefixes) if (sv_starts_with(t, p)) return true;
    return false;
}

// Index of the first prefix in `prefixes` that `name` starts with, or -1.
// `name` is assumed already trimmed.
inline int first_prefix_index_sv(std::string_view name,
                                 const std::vector<std::string>& prefixes) {
    for (size_t i = 0; i < prefixes.size(); i++)
        if (sv_starts_with(name, prefixes[i])) return static_cast<int>(i);
    return -1;
}

// Byte length of a file, or -1 if it cannot be opened.
inline std::streamoff file_size_bytes(const std::string& path) {
    std::ifstream f(path, std::ios::binary | std::ios::ate);
    if (!f.is_open()) return -1;
    return static_cast<std::streamoff>(f.tellg());
}

// Target chunk size in bytes. Overridable via WSBT_CHUNK_BYTES (used by tests to
// force many tiny chunks and exercise the boundary logic); defaults to 16 MB.
// The value must be a plain positive integer; a malformed value (e.g. the
// scientific-notation "1.6e+07") is rejected and the default used, rather than
// being misparsed as a tiny number that would explode into millions of chunks.
inline std::streamoff default_chunk_bytes() {
    const char* e = std::getenv("WSBT_CHUNK_BYTES");
    if (e && *e) {
        char* end = nullptr;
        long long v = std::strtoll(e, &end, 10);
        while (end && (*end == ' ' || *end == '\t' || *end == '\n' || *end == '\r')) end++;
        if (v > 0 && end && *end == '\0') return static_cast<std::streamoff>(v);
    }
    return static_cast<std::streamoff>(16) * 1024 * 1024;
}

// Number of threads to use for the file-reading phase. The read is I/O-bound on
// the real (large, uncached) files, where a handful of concurrent read streams
// overlap disk latency and speed the read up markedly; but on small/warm data it
// is memory-bandwidth-bound and extra threads only add contention. A modest
// default balances both; override with WSBT_READ_THREADS. (The CPU-bound
// post-processing phase is parallelised separately and still uses all cores.)
inline int read_thread_count() {
    int cap = 4;
    const char* e = std::getenv("WSBT_READ_THREADS");
    if (e && *e) {
        char* end = nullptr;
        long v = std::strtol(e, &end, 10);
        while (end && (*end == ' ' || *end == '\t' || *end == '\n' || *end == '\r')) end++;
        if (v >= 1 && end && *end == '\0') cap = static_cast<int>(v);
    }
    int mx = omp_get_max_threads();
    if (mx < 1) mx = 1;
    return std::min(cap, mx);
}

// Devices processed per pass in calculate_address_duration's low_memory mode.
// Each pass reads every file once, so this trades peak memory (larger = more
// devices held at once) against I/O (larger = fewer full re-reads). Default 4;
// override with WSBT_DEVICES_PER_PASS (1 reproduces the old device-at-a-time
// behaviour).
inline int devices_per_pass() {
    int v = 4;
    const char* e = std::getenv("WSBT_DEVICES_PER_PASS");
    if (e && *e) {
        char* end = nullptr;
        long x = std::strtol(e, &end, 10);
        while (end && (*end == ' ' || *end == '\t' || *end == '\n' || *end == '\r')) end++;
        if (x >= 1 && end && *end == '\0') v = static_cast<int>(x);
    }
    return v;
}

// A byte range within one input file.
struct LineChunk {
    size_t file_index;
    std::streamoff start;
    std::streamoff end;  // inclusive upper bound on a processed line's start offset
};

// Split every file into contiguous chunks of ~target_bytes. Files that cannot be
// sized get a single full-range chunk so the worker can report the open failure.
inline std::vector<LineChunk> plan_line_chunks(const std::vector<std::string>& files,
                                               std::streamoff target_bytes) {
    std::vector<LineChunk> chunks;
    if (target_bytes < 1) target_bytes = 1;
    for (size_t fi = 0; fi < files.size(); ++fi) {
        std::streamoff sz = file_size_bytes(files[fi]);
        if (sz < 0) {
            chunks.push_back(LineChunk{fi, 0, 0});
            continue;
        }
        if (sz == 0) continue;
        std::streamoff start = 0;
        while (start < sz) {
            std::streamoff end = start + target_bytes;
            if (end >= sz) end = sz;
            chunks.push_back(LineChunk{fi, start, end});
            start = end;
        }
    }
    return chunks;
}

// Process a single chunk: stream the lines this chunk owns into `acc` via `line_fn`.
template <class Acc, class LineFn>
void process_one_chunk(const std::string& path, std::streamoff start, std::streamoff end,
                       Acc& acc, LineFn& line_fn) {
    std::ifstream in(path, std::ios::binary);
    if (!in.is_open()) return;
    if (start > 0) in.seekg(start);

    std::streamoff pos = start;
    std::string line;

    if (start > 0) {
        // Discard the partial first line; it belongs to the previous chunk.
        if (!std::getline(in, line)) return;
        pos += static_cast<std::streamoff>(line.size()) + 1;
    }

    while (pos <= end) {
        if (!std::getline(in, line)) break;
        line_fn(acc, line);
        pos += static_cast<std::streamoff>(line.size()) + 1;
    }
}

// Read `files` in parallel, folding every line into a per-thread `Acc` and then
// merging the per-thread accumulators into one result.
//
//   line_fn:  void(Acc&, const std::string& line)   -- const, thread-safe
//   merge_fn: void(Acc& dst, Acc& src)              -- fold src into dst (may move)
//
// checkUserInterrupt() runs on the main thread between batches of chunks, so a
// long read over a huge file stays interruptible without touching the R API from
// a worker thread.
template <class Acc, class LineFn, class MergeFn>
Acc parallel_reduce_files(const std::vector<std::string>& files,
                          LineFn line_fn,
                          MergeFn merge_fn,
                          std::streamoff target_bytes = -1) {
    if (target_bytes < 0) target_bytes = default_chunk_bytes();
    std::vector<LineChunk> chunks = plan_line_chunks(files, target_bytes);

    int n_slots = read_thread_count();
    std::vector<Acc> locals(n_slots);

    const std::ptrdiff_t n_chunks = static_cast<std::ptrdiff_t>(chunks.size());
    // Run chunks in batches; between batches we are back on the main thread and
    // can safely check for a user interrupt.
    const std::ptrdiff_t batch = static_cast<std::ptrdiff_t>(n_slots) * 4 + 1;

    for (std::ptrdiff_t base = 0; base < n_chunks; base += batch) {
        const std::ptrdiff_t stop = std::min(base + batch, n_chunks);

        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(n_slots)
        #endif
        for (std::ptrdiff_t ci = base; ci < stop; ++ci) {
            const LineChunk& ch = chunks[ci];
            Acc& acc = locals[omp_get_thread_num()];
            process_one_chunk(files[ch.file_index], ch.start, ch.end, acc, line_fn);
        }

        Rcpp::checkUserInterrupt();
    }

    Acc result;
    for (int i = 0; i < n_slots; ++i) {
        merge_fn(result, locals[i]);
    }
    return result;
}

}  // namespace wsbt

#endif  // WSBT_PARALLEL_READ_H
