# Tests that rely on a connection.
# FAQ: Why are most `test` bodies wrapped in an inner `proc`? This is necessary
# because `std/unittest.test` fiddles with the variables making the compiler
# believe there are RAII moves where there are not. The handle types cannot be
# copied, so this produces a compile-time error. Wrapping the body in a `proc`
# solves this.

import std/[
    unittest,
    os,
    options,
    macros,
    sequtils,
    encodings,
    strformat,
    logging,
    math,
    strutils,
    enumerate,
]
import
    odbcn,
    odbcn/[connstr, functions, widestrx],
    odbcn/private/core {.all.}
#from odbcn/wrapper import SqlHDBC
import odbcn/wrapper
#import odbc_wrapper_nim

const errorMsg {.used.} = """
Missing or malformed "env.nim" file in this directory. This file is not source
controlled, so user must create this themselves. This file defines the
following constants required for integration testing:

```
const dbServer = "" # name of test server/DSN
const dbUser = "" # name of user to connect
const dbPass = "" # password of user to connect
```"""

static:
    when not fileExists(currentSourcePath() /../ "env.nim"):
        macros.error errorMsg
    discard
import ./env {.all.} # Don't `include`, so that user can import std/os there
when not declared(dbServer) or not declared(dbUser) or not declared(dbPass):
    macros.error errorMsg
const connString = $initConnString {"DSN": dbServer, "UID": dbUser, "PWD": dbPass}

const testConnString = $initConnString {"DSN": dbServerx64, "UID": dbUser, "PWD": dbPass, "Database": "test"}
template prepNew(conn: OdbcConn, qry: static string): untyped =
    prep(conn, qry, testConnString)

type FConsoleLogger = ref object of Logger

method log(logger: FConsoleLogger, level: Level, args: varargs[string, `$`]) =
    let ln = substituteLog(logger.fmtStr, level, args)
    stderr.writeLine(ln)
    stderr.flushFile

addHandler FConsoleLogger(fmtStr: "$time [$levelname] ")

proc toArray[I: static int](s: string): array[I, char] =
    assert s.len <= I
    copyMem(result[0].addr, s[0].unsafeAddr, s.len)
    if s.len != I:
        result[s.len] = '\0'

proc toCString[I](s: array[I, char]): cstring =
    cast[cstring](s[0].unsafeAddr)

suite "Pre-connection utilities":
    test "listDrivers":
        check listDrivers().toSeq.len > 0
    test "listDataSources":
        check listDataSources().toSeq.len > 0

suite "Establishing connection":
    test "Base":
        check not newOdbcNoConn().connect(dbServer, dbUser, dbPass).SqlHDBC.isNil
    test "Failed connection":
        expect OdbcException:
            discard newOdbcNoConn().connect(dbServer & "_Nope", dbUser, dbPass)
    test "Connection string":
        let connString = $initConnString {"DSN": dbServer, "UID": dbUser, "PWD": dbPass}
        check not newOdbcNoConn().connect(connString).SqlHDBC.isNil

static:
    let initFile = currentSourcePath() /../ "init_databases.nim"
    let cmd = &"nim r --cpu:{hostCPU} " & initFile & " -c \"" & connString & "\""
    let (outp, rc) = gorgeEx cmd
    if rc != 0:
        macros.error "Command " & cmd & " failed with: " & outp

# This only checks SQL Server data types, but this should not be generic
# enough.
suite "Statements that check if SQL Server data types work":
    setup:
        var conn = newOdbcConn(dbServer, dbUser, dbPass)

    test "integer":
        proc doTest =
            let val = conn.execScalar"select 3"
            check val.kind == otInt32
            check val.i64 == 3
        doTest()
    test "smallint":
        proc doTest =
            let val = conn.execScalar"select cast(2 as smallint)"
            check val.kind == otInt16
            check val.i64 == 2
        doTest()
    test "tinyint":
        proc doTest =
            let val = conn.execScalar"select cast(5 as tinyint)"
            check val.kind == otInt8
            check val.i64 == 5
        doTest()
    test "bit":
        proc doTest =
            let val = conn.execScalar"select cast(1 as bit)"
            check val.kind == otBool
            check val.i64 == 1
        doTest()
    test "bigint":
        proc doTest =
            let val = conn.execScalar"select cast(5000000000 as bigint)"
            check val.kind == otInt64
            check val.i64 == 5_000_000_000
        doTest()

    test "float32":
        proc doTest =
            let val = conn.execScalar"select cast(345.32 as real)"
            check val.kind == otFloat32
            check almostEqual(val.f32, 345.32)
        doTest()

    test "float64":
        proc doTest =
            let val = conn.execScalar"select cast(3124.32 as double precision)"
            check val.kind == otFloat64
            check almostEqual(val.f64, 3124.32)
        doTest()
    test "string":
        proc doTest =
            let val = conn.execScalar"select 'Hey'"
            check val.kind == otWideArray
            check val.wchars.len == 3
            check $val == "Hey"
        doTest()
    test "widestring":
        proc doTest =
            let val = conn.execScalar"select N'Hey'"
            check val.kind == otWideArray
            check $val == "Hey"
        doTest()
    test "binary":
        proc doTest =
            let val = conn.execScalar "select cast(123456 as binary(4))"
            check val.kind == otByteArray
            check val.bytes == @[0x00'u8, 0x01, 0xe2, 0x40]
            check $val == "0001E240"
        doTest()
    test "date":
        proc doTest =
            let val = conn.execScalar"select datefromparts(2020, 3, 4)"
            check val.kind == otDate
            check $val == "2020-03-04"
        doTest()
    test "time":
        proc doTest =
            let val = conn.execScalar"select timefromparts(13, 32, 4, 321, 4)"
            check val.kind == otTime
            check $val == "13:32:04"
        doTest()

    test "null string produces empty string":
        proc doTest =
            let val = conn.execScalar "select cast(null as varchar(5))"
            check val.kind == otWideArray
            check $val == ""
        doTest()

    # NOTE: Non-zero milliseconds cause rounding which makes a `check`
    # unfeasible. This is likely fine in normal scenarios as the `datetime`
    # type is stored in SQL Server as floating point anyway. Use `datetime2`
    # or a float-based datetime instead.
    test "Simple timestamp select":
        proc doTest =
            let val = conn.execScalar"select datetimefromparts(2021, 10, 5, 13, 32, 4, 0)"
            check val.kind == otTimestamp
            check $val == "2021-10-05T13:32:04.000000000"
        doTest()

    test "Simple timestamp2 select":
        proc doTest =
            let val = conn.execScalar"select datetime2fromparts(2021, 10, 5, 13, 32, 4, 323456, 7)"
            check val.kind == otTimestamp
            check $val == "2021-10-05T13:32:04.032345600"
        doTest()

    test "Getting top results":
        proc doTest =
            let ds = conn.exec("select top 2 * from sys.types")
            var row: OdbcRowSet
            check ds.next(row) == true
            check ds.next(row) == true
            check ds.next(row) == false
        doTest()

    test "Get current catalog":
        check conn.dbmsName != "Microsoft SQL Server" or conn.catalog == "master"

    test "Dump basic info":
        echo "driver odbc ver ", conn.driverOdbcVersion
        echo "driver ver ", conn.driverVersion
        echo "driver name ", conn.driverName
        echo "dbms name ", conn.dbmsName
        echo "dbms ver ", conn.dbmsVersion
        echo "server name ", conn.serverName
        echo "user name ", conn.userName

    test "Parameterized simple query":
        proc doTest =
            let ds = conn.exec("select ?", 3)
            var row: OdbcRowSet
            check ds.next(row)
            check hostCPU != "amd64" or row[0].kind == otInt64
            check hostCPU != "i386" or row[0].kind == otInt32
            check row[0].i64 == 3
            check not ds.next(row)
        doTest()

    test "Repeated parameterized query":
        proc testInner =
            var
                qry = conn.prep("select ?")
                row: OdbcRowSet
            for i in 2..5:
                let ds = qry.exec(i)
                check ds.next(row)
                check row[0].i64 == i
                check not ds.next(row)
                qry = ds.unbind
        testInner()

    test "Repeated parameterized query with withExec":
        proc testInner =
            var
                qry = conn.prep("select ?")
                row: OdbcRowSet
            for i in 2..5:
                qry.withExec(i):
                    check rs.next(row) == true
                    check row[0].i64 == i
                    check rs.next(row) == false
        testInner()

    test "Repeated parameterized query with withExec with no params":
        proc testInner =
            var
                qry = conn.prep("select 1")
                row: OdbcRowSet
            qry.withExec:
                check rs.next(row) == true
                check row[0].i64 == 1
                check rs.next == false
            qry.withExec:
                check rs.next(row) == true
                check row[0].i64 == 1
                check rs.next == false
        testInner()

    test "Parameterized widestring is written correctly to varchar column":
        proc doTest =
            discard conn.exec"create table #abc (VeryLong varchar(max))"
            let param = 'b'.repeat(3500)
            discard conn.exec("insert #abc values (?)", param)
            # Check that actual number of bytes are correct
            check conn.execScalar"select datalength(VeryLong) from #abc".toInt == 3500
            # Check that string doesn't have spaces in it (it should be just 'a's)
            check conn.execScalar"select len(VeryLong) from #abc".toInt == 3500
            check $conn.execScalar"select VeryLong from #abc" == param
        doTest()
    test "Parameterized widestrings is written correctly to nvarchar column":
        proc doTest =
            discard conn.exec"create table #abc (VeryLong nvarchar(max))"
            let param = 'b'.repeat(3500)
            discard conn.exec("insert #abc values (?)", param)
            # Check that actual number of bytes are correct
            check conn.execScalar"select datalength(VeryLong) from #abc".toInt == 3500 * 2
            # Check that string doesn't have spaces in it (it should be just 'a's)
            check conn.execScalar"select len(VeryLong) from #abc".toInt == 3500
            check $conn.execScalar"select VeryLong from #abc" == param
        doTest()
    test "Unicode characters are inserted correctly":
        proc doTest =
            discard conn.exec"create table #abc (SomeCol nvarchar(max))"
            # æ is 1 byte ANSI / 2 byte UTF-8/16
            # シ is 2 byte UTF-16 / 3 byte UTF-8
            let unicode = "løæシ"
            check unicode.newWideCString.len == 4 # Sanity check
            discard conn.exec("insert #abc values (?)", unicode)
            check conn.execScalar"select len(SomeCol) from #abc".toInt == 4
            check $conn.execScalar"select SomeCol from #abc" == unicode
        doTest()

    test "Variable sized data":
        proc testInner =
            discard conn.exec"create table #abc (VeryLong varchar(max))"

            # `newWideCString` is unnecessary, because `string`s are converted
            # to UTF-16 regardless, but this demonstrates that `WideCString`
            # can be used for optimization purposes.
            discard conn.exec("insert #abc values (?)", 'b'.repeat(3000).newWideCString)
            check conn.execScalar"select datalength(VeryLong) from #abc".toInt == 3000
            check conn.execScalar"select len(VeryLong) from #abc".toInt == 3000
            discard conn.exec("update #abc set VeryLong = VeryLong + ?",
                                       'a'.repeat(1500))
            check conn.execScalar"select len(VeryLong) from #abc".toInt == 4500
            let val = conn.execScalar"select VeryLong from #abc"
            check val.wchars.len == 4500
        testInner()

    test "Prepared execution":
        proc doTest =
            let stmt = conn.prep"select * from sys.types where name = ?"
            let ds = stmt.exec("int")
            var row: OdbcRowSet
            check ds.next(row) == true
            check ds.next(row) == false
        doTest()

    test "Prepared UPDATE":
        proc doTest =
            discard conn.exec "create table #abc (SomeCol nvarchar(max))"
            block:
                let stmt = conn.prep "insert #abc values (?)"
                stmt.execOnly("int")
                stmt.execOnly("hm")
            block:
                let rs = conn.exec "select * from #abc"
                var row: OdbcRowSet
                check rs.next(row)
                check $row[0] == "int"
                check rs.next(row)
                check $row[0] == "hm"
                check not rs.next(row)
        doTest()

    test "Transactions":
        proc doTest =
            check not conn.inTran
            discard conn.exec"create table #abc (SomeCol nvarchar(max))"
            conn.withTran:
                discard conn.exec("insert #abc values (?)", "hey")
                check conn.inTran
                discard conn.exec("update #abc set SomeCol = ?", "no")
            check $conn.execScalar"select SomeCol from #abc" == "no"
            check not conn.inTran
        doTest()

    test "Executing, then unbinding and executing prepared statements works":
        proc doTest =
            var stmt = conn.prep"select * from sys.tables"
            var ds = stmt.exec
            check ds.next
            stmt = ds.unbind
            ds = stmt.exec
            check ds.next
        doTest()

    # Specific to SQL Server
    test "Compile-time query check":
        proc doTest =
            let stmt = conn.prepNew("select top 2 name, system_type_id from sys.types")
            var row = stmt.initRowSet
            let ds = stmt.exec()
            echo row.repr
            check ds.next(row) == true
            echo row.system_type_id
            check ds.next(row) == true
            echo row.repr
            check ds.next(row) == false
            echo row.repr
        doTest()

    test "Money types":
        proc doTest =
            discard conn.exec"create table #abc (Cash money)"
            discard conn.exec"insert #abc values (32.43)"
            let val = conn.execScalar"select * from #abc"
            check val.kind == otCharArray
            check $val == "32.4300"
        doTest()

    test "Datetime types":
        type TestObj = object
            someDate: string
        proc doTest =
            block:
                let stmt = conn.prepNew("select datefromparts(2020, 3, 4) as someDate")
                let rs = stmt.exec
                check rs.next
                var row: TestObj
                rs.getDatas row
                echo row.repr
                check row.someDate == "2020-03-04"
            block:
                let stmt = conn.prepNew("select datefromparts(2020, 3, 4) as someDate")
                check stmt.execFirst.get == OdbcDate(year: 2020, month: 3, day: 4)
        doTest()

suite "UTF-8 string with ANSI equivalent is stored as ANSI in varchar":
    setup:
        var conn = newOdbcConn(dbServer, dbUser, dbPass)
        let utf8Str = "ø"
        require utf8Str.len == 2
        let ansiStr = utf8Str.convert("CP1252", "UTF-8")
        require ansiStr.len == 1
        let collate = "latin1_general_100_ci_as_sc"
        proc doSetup =
            discard conn.exec "create table #abc (SomeCol varchar(max) collate " & collate & ")"
            discard conn.exec("insert #abc values (?)", utf8Str)
        doSetup()
    test "Number of bytes is 1 in ANSI":
        proc doTest =
            check conn.execScalar"select datalength(SomeCol) from #abc".toInt == 1
        doTest()
    test "Number of characters is 1 in ANSI":
        proc doTest =
            check conn.execScalar"select len(SomeCol) from #abc".toInt == 1
        doTest()
    test "Binary representation is in ANSI":
        proc doTest =
            block:
                type BinaryStr = object
                    someCol: seq[char]
                let rs = conn.exec("select SomeCol from #abc")
                check rs.items(BinaryStr).toSeq == @[BinaryStr(someCol: ansiStr.toSeq)]
        doTest()
    test "String representation is in UTF-8":
        proc doTest =
            check $conn.execScalar"select SomeCol from #abc" == utf8Str
        doTest()

# NOTE: A UTF-8 collation does NOT change how string data is sent. It must
# still be converted to UTF-16, sent over ODBC API, and then converted back
# to UTF-8 to be stored in the database. We cannot send UTF-8 data directly
# through "varchar" API. The below commented test demonstrates this; we are
# able to get UTF-8 encoded data into the database, but it is not interpreted
# as UTF-8.
# NOTE: Must use `seq[char]` here, because `string` is otherwise converted to
# `seq[uint16]` by this library.
#suite "UTF-8 string is stored as-is in database":
#    setup:
#        var conn = newOdbcConn(dbServer, dbUser, dbPass)
#        let utf8Str = cast[seq[byte]]("ø".toSeq)
#        require utf8Str.len == 2
#        proc doSetup =
#            discard conn.exec "drop database if exists utf8test"
#            discard conn.exec "create database utf8test collate latin1_general_100_ci_as_sc_UTF8"
#            conn.setCatalog "utf8Test"
#            discard conn.exec "create table #abc (SomeCol varchar(max))"
#            discard conn.exec("insert #abc values (?)", utf8Str)
#        doSetup()
#
#    # Succeeds
#    test "Number of bytes takes 2-byte UTF-8 into account":
#        proc doTest =
#            check conn.execScalar"select datalength(SomeCol) from #abc".toInt == 2
#        doTest()
#
#    # Fails: Result is 2 (indicates it doesn't understand it's UTF-8)
#    test "Number of characters is number of bytes":
#        proc doTest =
#            check conn.execScalar"select len(SomeCol) from #abc".toInt == 1
#        doTest()
#
#    # Succeeds
#    test "Binary representation is the same UTF-8 sequence sent in":
#        proc doTest =
#            block:
#                type BinaryStr = object
#                    someCol: seq[byte]
#                let rs = conn.exec("select SomeCol from #abc")
#                check rs.items(BinaryStr).toSeq == @[BinaryStr(someCol: utf8Str)]
#        doTest()
#
#    # Fails: Returns (0xC383_C2B8), expected (0xC3B8); likely thinks varchar
#    # data is in some other encoding
#    test "String representation is correct UTF-8":
#        proc doTest =
#            echo ($conn.execScalar"select SomeCol from #abc").repr
#            check $conn.execScalar"select SomeCol from #abc" == "ø"
#        doTest()

proc setupTestConn: OdbcConn =
    var noConn = newOdbcNoConn()
    noConn.setCatalog "test"
    var conn = noConn.connect(dbServer, dbUser, dbPass)
    require conn.catalog == "test"
    discard conn.exec"delete from abc"
    discard conn.exec"delete from TwoValue"
    discard conn.exec"delete from HasGuid"
    discard conn.exec"delete from FixedLenTypes"
    discard conn.exec"delete from SameTypes"
    conn

suite "Testing real database schemas":
    setup:
        var conn = setupTestConn()

    test "Binding columns with compile-time helper":
        proc testInner =
            block:
                let stmt = conn.prepNew("insert FixedLenTypes values (?, ?)")
                stmt.execOnly(4, 5.67890123456)
                stmt.execOnly(5, 3.45678901234)
            let stmt = conn.prepNew("select * from FixedLenTypes")
            var row = stmt.initRowSet
            stmt.bindCols row
            let ds = stmt.exec
            check ds.next
            check row.someFloat == 5.67890123456
            check row.someInt == 4
            check ds.next
            check row.someFloat == 3.45678901234
            check row.someInt == 5
            check not ds.next
        testInner()

    test "Compile-time query for variable sized data":
        proc testInner =
            discard conn.exec("insert abc values (?)", 'b'.repeat(3000).newWideCString)
            check conn.execScalar"select datalength(VeryLong) from abc".toInt == 3000
            check conn.execScalar"select len(VeryLong) from abc".toInt == 3000
            discard conn.exec("update abc set VeryLong = VeryLong + ?",
                                       'a'.repeat(1500))
            check conn.execScalar"select len(VeryLong) from abc".toInt == 4500
            let stmt = conn.prepNew"select VeryLong from abc"
            check stmt.execFirst.get.len == 4500
        testInner()

    test "Bind column locally":
        type TestObj = object
            veryLong: array[50, char]
        proc testInner =
            discard conn.exec("insert abc values (?)", "32")
            block:
                let ds = conn.exec"select VeryLong from abc"
                var row: TestObj
                ds.bindCols row
                check ds.next
                let str = $row.veryLong.toCString
                check str == "32"
                check not ds.next
        testInner()

    # Use nvarchar column for unicode since it's UTF-16
    test "Unicode in compile-time checked query":
        proc doTest =
            # æ is 1 byte ANSI / 2 byte UTF-8/16
            # シ is 2 byte UTF-16 / 3 byte UTF-8
            let unicode = "løæシ"
            check unicode.utf8To16.len == 4 # Sanity check
            check unicode.utf8To16.utf16To8 == unicode
            block:
                let stmt = conn.prepNew("insert TwoValue (SomeName) values (?)")
                stmt.execOnly(unicode)
            block:
                let stmt = conn.prepNew("select len(SomeName) as leng from TwoValue")
                check stmt.execFirst.get == 4
            block:
                let stmt = conn.prepNew("select SomeName from TwoValue")
                check stmt.execFirst.get == unicode
        doTest()

    # Both result sets go out of scope at the same time, i.e. at the end of the
    # proc. `discard stmt1.exec` doesn't make the return result set go out of
    # scope immediately.
    test "Multiple result sets in scope fails":
        proc doTest =
            block:
                let stmt = conn.prepNew("insert TwoValue (SomeName) values (?)")
                stmt.execOnly("Hello")
            let stmt1 = conn.prepNew("select len(SomeName) as leng from TwoValue")
            discard stmt1.exec
            let stmt2 = conn.prepNew("select SomeName from TwoValue")
            discard stmt2.exec
        expect Exception:
            doTest()

    test "Custom query type":
        type TestObj = object
            veryLong: int32
        proc testInner =
            discard conn.exec("insert abc values (?)", "3")
            let stmt = conn.prepNew("select convert(int, VeryLong) as veryLong from abc")
            var row: TestObj
            let ds = stmt.exec
            check ds.next
            ds.getDatas row
            check row.veryLong == 3
            check ds.next == false
        testInner()

    test "Multiple column custom query type in reverse order":
        proc testInner =
            discard conn.exec("insert TwoValue values (?, ?)", "heyo", 3)
            let stmt = conn.prepNew("select * from TwoValue where SomeValue = ?")
            let row = stmt.execFirst(3).get
            check row.someValue == 3
            check row.someName == "heyo"
        testInner()

    test "Insert with parameter type":
        type TestObj = object
            veryLong: seq[Utf16Char]
        proc testInner =
            let inp = TestObj(veryLong: utf8To16"hey")
            block:
                let stmt = conn.prepNew("insert abc values (?veryLong)")
                stmt.bindParams inp
                stmt.execOnly
            block:
                let stmt = conn.prepNew("select * from abc")
                let ds = stmt.exec
                var outp: TestObj
                check ds.next(outp) == true
                check outp == inp
                check ds.next(outp) == false
        testInner()

    # This cannot work, becasue the parameter binding routine knows nothing
    # about the order of parameters for non-constant queries.
    # For this to work, `qry` is forced non-constant by executing an arbitrary
    # if-statement.
    test "Insert with parameter type in reverse order doesn't work":
        type TestObj = object
            someValue: int
            someName: seq[Utf16Char]
        proc testInner =
            let inp = TestObj(someValue: 5, someName: utf8To16"Yup!")
            block:
                var qry = "update TwoValue set SomeName = ?, SomeValue = ?"
                if inp.someValue > 0:
                    qry &= " where SomeValue > 0"
                else:
                    qry &= " where SomeValue < 0"
                let stmt = conn.prep(qry)
                stmt.bindParams inp
                discard stmt.exec
        expect OdbcException:
            testInner()

    test "Insert with parameter type in reverse order with supplied order":
        type TestObj = object
            someValue: int
            someName: seq[Utf16Char]
        proc testInner =
            let inp = TestObj(someValue: 5, someName: utf8To16"Yup!")
            block:
                let stmt = conn.prepNew("insert TwoValue values (?someName, ?someValue)")
                stmt.bindParams(inp, ["someName", "someValue"])
                stmt.execOnly
            block:
                let stmt = conn.prepNew("select * from TwoValue")
                let ds = stmt.exec
                var outp: TestObj
                check ds.next(outp) == true
                check outp == inp
                check ds.next(outp) == false
        testInner()

    test "Named parameter varargs":
        proc doTest =
            block:
                let stmt = conn.prepNew("insert TwoValue values (?someName, ?someValue)")
                stmt.execOnly(someName = "Hello", someValue = 3)
            block:
                let stmt = conn.prepNew"select * from TwoValue"
                let row = stmt.execFirst.get
                check row.someValue == 3
                check row.someName == "Hello"
        doTest()

    test "Named parameter varargs in runtime query":
        proc doTest =
            discard conn.exec("insert TwoValue values (?someName, ?someValue)",
                              someName = "Hello", someValue = 3)
            let row = conn.execFirst("select * from TwoValue").get
            check $row["SomeName"] == "Hello"
            check row["SomeValue"].i64 == 3
        doTest()

    test "Named parameter varargs duplicate value":
        proc doTest =
            block:
                let stmt = conn.prepNew("insert SameTypes values (?someInt, ?someInt)")
                stmt.execOnly(someInt = 5)
            block:
                let stmt = conn.prepNew"select * from SameTypes"
                let row = stmt.execFirst.get
                check row.someInt1 == 5
                check row.someInt2 == 5
        doTest()

    test "Named parameter varargs duplicate value in runtime query":
        proc doTest =
            discard conn.exec("insert SameTypes values (?someInt, ?someInt)",
                         someInt = 5)
            let row = conn.execFirst("select * from SameTypes").get
            check row["SomeInt1"].i64 == 5
            check row["SomeInt2"].i64 == 5
        doTest()

    test "Parameter key-vals that don't cover all parameters creates error":
        proc testInner =
            let stmt = conn.prepNew("insert TwoValue values (?someName, ?someValue)")
            check not compiles(stmt.exec(someValue = 3))
        testInner()

    test "Parameters type that don't cover all parameters creates error":
        type TestObj = object
            someValue: int32
        proc testInner =
            let stmt = conn.prepNew("insert TwoValue values (?someName, ?someValue)")
            let params = TestObj(someValue: 3)
            check not compiles(stmt.bindParams params)
        testInner()

    test "Multiple executions of static insert query":
        type TestObj = object
            someValue: int
            someName: array[1, char]
        proc testInner =
            block:
                let stmt = conn.prepNew("insert TwoValue values (?someName, ?someValue)")
                for i in 0..3:
                    let inp = TestObj(someName: toArray[1]($i), someValue: i)
                    stmt.bindParams inp
                    stmt.execOnly
            block:
                let stmt = conn.prepNew("select * from TwoValue")
                var row = stmt.initRowSet
                let ds = stmt.exec
                for i in 0..3:
                    check ds.next(row)
                    check row.someValue == i
                    check $row.someName[0] == $i
                check not ds.next
        testInner()

    # SQL_NO_DATA
    test "No updated records work":
        proc testInner =
            discard conn.exec("update TwoValue set SomeName = ?", "this")
        testInner()

    test "Guid":
        proc doTest =
            let guid = GUID(D1: 0x1234, D2: 0x56, D3: 0x78,
                            D4: [0x90'i8, 0xAB'i8, 0xCD'i8, 0xEF'i8,
                                 0x12'i8, 0x34'i8, 0x56'i8, 0x78'i8])
            block:
                let stmt = conn.prepNew("insert HasGuid values (?)")
                stmt.execOnly(guid)
            block:
                let stmt = conn.prepNew("select * from HasGuid")
                let retGuid = stmt.execFirst.get
                check retGuid == guid
        doTest()

    test "SQLTables":
        # A demonstration of how to use `colIdx` to only get a subset of the
        # columns available in a special ODBC function such as `SQLTables` or
        # `SQLColumns`. `catalog` is index 1, and `table` is index 3. Can use
        # `SQLTables_<colName>` constants from ODBC wrapper instead of raw
        # integers here.
        type OdbcTables = object
            catalog: array[256, char]
            table {.odbcCol: 3.}: array[256, char]
            ty {.odbcCol: 4.}: array[16, char] # len(LOCAL TEMPORARY) + 1

        template cs(obj): untyped =
            $(obj[0].unsafeAddr.cstring)

        var
            ds = conn.newStmt.selectTables(table = "%Types", tableType = {ttTable})
            row: OdbcTables
        ds.bindCols row
        check ds.next
        check cs(row.table) == "FixedLenTypes"
        check ds.next
        check cs(row.table) == "SameTypes"
        check not ds.next

    test "Non-static exec query non-key-val compiles":
        proc doTest(inp: int) =
            var qry = "select * from TwoValue"
            if inp == 3:
                qry &= " where cast(SomeName as int) = ?"
            else:
                qry &= " where SomeValue = ?"
            let rs = conn.exec(qry, inp)
            check not rs.next
        doTest(3)

    test "Non-static exec query key-val not compiles":
        proc doTest(inp: int) =
            var qry = "select * from TwoValue"
            if inp == 3:
                qry &= " where cast(SomeName as int) = ?val"
            else:
                qry &= " where SomeValue = ?val"
            check not compiles(conn.exec(qry, val = inp))
        doTest(3)

    test "bindParam override":
        type
            ArraySeq[I: static int, T] = object
                len: int
                data: array[I, T]

        proc `=destroy`[I, T](x: var ArraySeq[I, T]) =
            for i in 0..<x.len:
                `=destroy`(x.data[i])

        proc `=copy`[I, T](dst: var ArraySeq[I, T], src: ArraySeq[I, T]) {.error.}

        proc toOpenArray[I, T](x: ArraySeq[I, T]): openArray[T] =
            x.data.toOpenArray(0, x.len - 1)

        func toPrimTy[I, T](_: typedesc[ArraySeq[I, T]]): OdbcPrimType = elemToPrimTy(T)

        proc bindParam[I, T](ds: OdbcStmt, idx: TSqlUSmallInt, param: ptr ArraySeq[I, T]) =
            const
                primTy = toPrimTy(param[].type)
                cTy = toCTy(primTy)
                odbcTy = toBestOdbcTy(primTy)
            bindParamPtr(ds, idx, cTy, odbcTy, param[].data[0].addr, param[].len)

        proc toArraySeq[I](s: string): ArraySeq[I, char] =
            assert s.len <= I
            copyMem(result.data[0].addr, s[0].unsafeAddr, s.len)
            result.len = s.len

        type TestObj = object
            someValue: int
            someName: ArraySeq[4, char]
        proc testInner =
            block:
                let stmt = conn.prepNew("insert TwoValue values (?someName, ?someValue)")
                for i in 0..3:
                    let inp = TestObj(someName: toArraySeq[4]($i), someValue: i)
                    stmt.bindParams inp
                    stmt.execOnly
            block:
                let stmt = conn.prepNew("select * from TwoValue")
                var row = stmt.initRowSet
                let ds = stmt.exec
                for i in 0..3:
                    check ds.next(row)
                    check row.someValue == i
                    check $row.someName[0] == $i
                check not ds.next
        testInner()

suite "Default values":
    setup:
        var conn = setupTestConn()

    test "dynamic execFirst default value":
        proc doTest =
            let row = conn.execFirst "select * from TwoValue"
            check row.isNone
        doTest()

    test "static execFirst default value":
        proc doTest =
            let ds = conn.prepNew "select * from TwoValue"
            let row = ds.execFirst
            check row.isNone
        doTest()

    test "execScalar default value":
        proc doTest =
            let row = conn.execScalar "select * from TwoValue"
            check row.kind == otByteArray
            check row.bytes.len == 0
        doTest()

    test "getOrDefault":
        proc doTest =
            var row: OdbcRowSet
            check row.getOrDefault(0).kind == otByteArray
            check row.getOrDefault("Another non-existing column").kind == otByteArray
            check row.getOrDefault(0).bytes.len == 0
            check row.getOrDefault("Column").bytes.len == 0
        doTest()

suite "Iterators":
    setup:
        var conn = setupTestConn()
        proc doSetup =
            let stmt = conn.prepNew("insert TwoValue values (?someName, ?someValue)")
            for i in 0..3:
                stmt.execOnly(i, $i)
        doSetup()

    test "Dynamic query":
        proc testInner =
            let rs = conn.exec("select * from TwoValue")
            for i, row in enumerate rs:
                check row["someValue"].toInt == i
                check $row["someName"] == $i
        testInner()

    test "Dynamic query with custom row type":
        type TwoValue = object
            someName: string
            someValue: int
        proc testInner =
            let rs = conn.exec("select * from TwoValue")
            for i, row in enumerate rs.items(TwoValue):
                check row.someValue == i
                check row.someName == $i
        testInner()

    test "Static query":
        proc testInner =
            # NOTE: Cannot do `let stmt = conn.prepNew(...)[0]`
            let stmt = conn.prepNew("select * from TwoValue")
            let ds = stmt.exec
            for i, row in enumerate ds:
                check row.someValue == i
                check row.someName == $i
        testInner()

    test "Static query with custom row type":
        type TwoValue = object
            someName: string
            someValue: int
        proc testInner =
            # NOTE: Cannot do `let stmt = conn.prepNew(...)[0]`
            let stmt = conn.prepNew("select * from TwoValue")
            let ds = stmt.exec
            for i, row in enumerate ds.items(TwoValue):
                check row.someValue == i
                check row.someName == $i
        testInner()
