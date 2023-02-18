# tests custom_overloads

import std/[
    logging,
    options,
    os,
    times,
    unittest,
]

import
    odbcn, odbcn/[
        connstr,
        dataschema,
        wrapper,
    ]

include env_read

type
    ArraySeq[I: static int, T] = object
        ## Simple wrapper around `array`
        len: int
        buf: array[I, T]

proc add[I, T](x: var ArraySeq[I, T], y: openArray[T]) =
    let newLen = x.len + y.len
    doAssert newLen <= I
    copyMem(x.buf[x.len].addr, cast[pointer](y), y.len)
    x.len = newLen

# Template here because the `bindParam` overload we wrap puts values into
# scope, and using template is the only way of preserving those before the
# exec call.
template bindParam[I, T](stmt: SqlHandle, i: TSqlUSmallInt, param: ptr ArraySeq[I, T]) =
    bindParam(stmt, i, param.buf.toOpenArray(0, param.len-1))

# In order to provide `getData` override, we must conform to
# `ResizableContainer` concept:

proc `[]`(x: var ArraySeq, i: int): var ArraySeq.T =
    doAssert 0 <= i and i < x.len
    x.buf[i]

proc setLen[I, T](x: var ArraySeq[I, T], len: int) =
    doAssert len <= I
    x.len = len

proc getData[I, T](stmt: SqlHandle, i: TSqlUSmallInt, ret: var ArraySeq[I, T]) =
    getDataResizable(stmt, i, ret)

suite "ArraySeq":
    setup:
        var conn = newOdbcConn(connString)

    test "bindParam":
        proc aux =

            # NOTE: Make sure `I` is greater than 1024, since that is the minimum
            # required buffer size in the `getData` implementation we are wrapping.
            # This can be unfortunate when implementing a "growable collection"
            # using static arrays. Wrapping `getData` for `array`s should be
            # better, since there is always a terminating null-byte for `char` and
            # `Utf16Char`.
            var x: ArraySeq[4000, char]

            let s = "Hello"
            x.add s
            check cast[cstring](x.buf[0].addr) == s.cstring
            check conn.exec("select ?", x).first(string) == some(s)
            #var x: MyObj
            #let s = "Hello"
            #x.x.add s
            #check cast[cstring](x.x.buf[0].addr) == s.cstring
            #check conn.exec("select ?, ?", x).first((string, string)) == some((s, ""))
        aux()

    test "getData":
        proc aux =
            let str = "hey"
            type MyArr = ArraySeq[2000, char]
            var exp: MyArr
            exp.add str
            check conn.exec("select ?", str).first(MyArr) == some(exp)
        aux()

proc toTimestamp(x: DateTime): OdbcTimestamp =
    OdbcTimestamp(
        year: x.year.TSqlSmallInt,
        month: x.month.TSqlUSmallInt,
        day: x.monthday.TSqlUSmallInt,
        hour: x.hour.TSqlUSmallInt,
        minute: x.minute.TSqlUSmallInt,
        second: x.second.TSqlUSmallInt,
    )

proc toDateTime(x: OdbcTimestamp): DateTime =
    dateTime(
        year = x.year.int,
        month = x.month.Month,
        monthday = x.day.MonthdayRange,
        hour = x.hour.HourRange,
        minute = x.minute.MinuteRange,
        second = x.second.SecondRange,
    )

template bindParam(stmt: SqlHandle, i: TSqlUSmallInt, param: ptr DateTime) =
    var t = param[].toTimestamp
    bindParam(stmt, i, t.addr)

proc getData(stmt: SqlHandle, i: TSqlUSmallInt, ret: var DateTime) =
    var ot: OdbcTimestamp
    getData(stmt, i, ot)
    ret = ot.toDateTime

suite "DateTime":
    setup:
        var conn = newOdbcConn(connString)
        let
            t = dateTime(2023, mFeb, 20, 13, 12, 43)
            ot = t.toTimestamp

    test "bindParam":
        proc aux =
            check conn.exec("select ?", t)
                .first(OdbcTimestamp) == some(ot)
        aux()

    test "getData":
        proc aux =
            check conn.exec("select ?", t)
                .first(DateTime) == some(t)
            check conn.exec("select ?", ot)
                .first(DateTime) == some(t)
        aux()
