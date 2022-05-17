## Extension to `odbcn` with support for `std/times` objects such as
## `DateTime` and `Timestamp`.

import std/times
import "."/[private/core, wrapper]

proc toDateTime*(x: OdbcValue, zone: TimeZone = local()): DateTime =
    case x.kind
    of otCharArray, otWideArray:
        raise newException(ValueError, "Cannot convert string to DateTime " &
            "without format string. Use `std/times.parse` directly")
    of otDate:
        let d = x.date
        dateTime(d.year, d.month.Month, d.day, zone = zone)
    of otTimestamp:
        let d = x.datetime
        dateTime(d.year, d.month.Month, d.day, d.hour, d.minute, d.second,
                 d.fraction, zone = zone)
    else:
        raise newException(ValueError, "Cannot convert " & $x.kind & " to DateTime")
