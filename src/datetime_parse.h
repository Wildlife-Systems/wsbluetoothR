#ifndef WSBT_DATETIME_PARSE_H
#define WSBT_DATETIME_PARSE_H

#include <ctime>
#include <string_view>

// Parse `n` ASCII digits at `s` into `out`. Returns false on any non-digit, so
// malformed fields are rejected instead of silently becoming 0 (atoi's
// behaviour), which would leave mktime to decide - and mktime accepts
// out-of-range dates on glibc while rejecting them elsewhere.
inline bool wsbt_parse_digits(const char* s, int n, int& out) {
    int value = 0;
    for (int i = 0; i < n; ++i) {
        if (s[i] < '0' || s[i] > '9') {
            return false;
        }
        value = value * 10 + (s[i] - '0');
    }
    out = value;
    return true;
}

// Parse datetime string in format YYYYMMDD-HHMMSS to time_t. Returns -1 for
// anything that is not a well-formed, in-range datetime.
inline std::time_t wsbt_parse_datetime(std::string_view datetime_str) {
    if (datetime_str.length() < 15) {
        return -1;
    }

    const char* s = datetime_str.data();
    if (s[8] != '-') {
        return -1;
    }

    int year, month, day, hour, minute, second;
    if (!wsbt_parse_digits(s, 4, year) ||
        !wsbt_parse_digits(s + 4, 2, month) ||
        !wsbt_parse_digits(s + 6, 2, day) ||
        !wsbt_parse_digits(s + 9, 2, hour) ||
        !wsbt_parse_digits(s + 11, 2, minute) ||
        !wsbt_parse_digits(s + 13, 2, second)) {
        return -1;
    }

    if (year < 1970 || month < 1 || month > 12 || day < 1 || day > 31 ||
        hour > 23 || minute > 59 || second > 60) {
        return -1;
    }

    struct tm tm = {0};
    tm.tm_year = year - 1900;
    tm.tm_mon = month - 1;
    tm.tm_mday = day;
    tm.tm_hour = hour;
    tm.tm_min = minute;
    tm.tm_sec = second;
    tm.tm_isdst = -1;  // Let mktime determine DST

    return std::mktime(&tm);
}

#endif  // WSBT_DATETIME_PARSE_H
