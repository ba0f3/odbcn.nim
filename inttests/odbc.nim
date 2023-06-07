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

import env_read

proc toArray[I: static int](s: string): array[I, char] =
    assert s.len <= I
    copyMem(result[0].addr, s[0].unsafeAddr, s.len)
    if s.len != I:
        result[s.len] = '\0'

proc toCString[I](s: array[I, char]): cstring =
    cast[cstring](s[0].unsafeAddr)

proc tdConn(c: OdbcConn) =
    # `temptab` is a table used per test for specialized table creation.
    discard c.exec("drop table if exists temptab")

test "DSN and driver iterator finds tested DSN":
    let dsns = listDataSources().toSeq
    var dsnDriver: tuple[server, driver: string] = ("", "")
    for dsn in dsns:
        if cmpIgnoreCase(dsn.server, dbServer) == 0:
            dsnDriver = dsn
            break
    check dsnDriver.server != ""
    let drivers = listDrivers().toSeq
    check drivers.anyIt(cmpIgnoreCase(it.name, dsnDriver.driver) == 0)

suite "Establishing connection":
    test "Base":
        check not newOdbcNoConn().connect(dbServer, dbUser, dbPass).SqlHDBC.isNil
    test "Failed connection":
        expect OdbcException:
            discard newOdbcNoConn().connect(dbServer & "_Nope", dbUser, dbPass)
    test "Connection string":
        let connString = $initConnString {"DSN": dbServer, "UID": dbUser, "PWD": dbPass}
        check not newOdbcNoConn().connect(connString).SqlHDBC.isNil

let dbms = newOdbcConn(dbServer, dbUser, dbPass).dbmsName

template testBothPrepNonPrep(name, templ) =
    suite "Simple `bindParams`" & name:
        setup:
            var conn = newOdbcConn(dbServer, dbUser, dbPass)

        test "One int params":
            proc doTest =
                check templ(conn, "select ?", 3).firstOrDefault(int) == 3
            doTest()

        test "Two int params":
            proc doTest =
                check templ(conn, "select ?, ?", 3, 4)
                    .firstOrDefault((int, int)) == (3, 4)
            doTest()

        test "One string param":
            proc doTest =
                let s = "Hello, world!"
                check templ(conn, "select ?", s).firstOrDefault(string) == s
            doTest()

        test "Two string params":
            proc doTest =
                let s = ("Hello", "world!")
                check templ(conn, "select ?, ?", s[0], s[1])
                    .firstOrDefault((string, string)) == s
            doTest()

        test "One int and one string param":
            proc doTest =
                check templ(conn, "select ?, ?", 4, "world!")
                    .firstOrDefault((int, string)) == (4, "world!")
            doTest()

        test "One long addressable string param":
            proc doTest =
                const exp = 'b'.repeat(3500)
                let got = templ(conn, "select ?", exp).firstOrDefault(OdbcValue)
                check got.kind == otWideArray
                check got.wchars.len == 3500
                check $got == exp
            doTest()

        test "One long non-addressable string param":
            proc doTest =
                let got = templ(conn, "select ?", 'b'.repeat(3500))
                    .firstOrDefault(OdbcValue)
                check got.kind == otWideArray
                check got.wchars.len == 3500
                check $got == 'b'.repeat(3500)
            doTest()

        test "Two long string params":
            proc doTest =
                let
                    exp1 = "ø".repeat(3000)
                    exp2 = 'a'.repeat(3500)
                check templ(conn, "select ?, ?", exp1, exp2)
                    .firstOrDefault((string, string)) == (exp1, exp2)
            doTest()

        test "Long Byte array parameter":
            proc doTest =
                let got = templ(conn, "select ?", cast[seq[byte]]('b'.repeat(3500)))
                    .firstOrDefault(OdbcValue)
                check got.kind == otByteArray
                check got.bytes.len == 3500
                check cast[string](got.bytes) == 'b'.repeat(3500)
            doTest()

        test "WideCString parameter":
            proc doTest =
                let param = newWideCString"Heø"
                let got = templ(conn, "select ?", param.toOpenArray(0, param.len-1))
                    .firstOrDefault(OdbcValue)
                check got.kind == otWideArray
                check got.wchars.len == 3
                check $got == "Heø"
            doTest()

        test "array parameter":
            proc doTest =
                let param: array[4, byte] = [1'u8, 1'u8, 2'u8, 5'u8]
                let got = templ(conn, "select ?", param).firstOrDefault(OdbcValue)
                check got.kind == otByteArray
                check got.bytes.len == 4
                check got.bytes == param
            doTest()

        # See implementation of `bindParam(ptr array[I, byte])` for the global
        # there. That shall be instantiated per `I`.
        test "array parameter no. 2 assigns new len-global fine":
            proc doTest =
                let param: array[8, byte] = [1'u8, 1'u8, 2'u8, 5'u8, 3, 2, 1, 9]
                let got = templ(conn, "select ?", param).firstOrDefault(OdbcValue)
                check got.kind == otByteArray
                check got.bytes.len == 8
                check got.bytes == param
            doTest()

        test "Optional some int parameter":
            proc doTest =
                check templ(conn, "select ?", some(3)).first(int) == some(3)
            doTest()

        test "Optional none int parameter":
            proc doTest =
                check templ(conn, "select case when ? is null then 1 else 0 end", none(int))
                    .firstOrDefault(bool)
            doTest()

        # bindParamsOneSimple
        test "One parameter, too few parameters creates error, static query":
            proc doTest = check not compiles(templ(conn, "select ?, ?", 4))
            doTest()

        # bindParamsOneSimple
        test "One parameter, too few parameters creates error, dynamic query":
            proc doTest =
                let qry = "select ?, ?"
                discard templ(conn, qry, 4)
            expect(OdbcException):
                doTest()

        # bindParamsSimple
        test "Not enough multiple parameters creates error, static query":
            proc doTest = check not compiles(templ(conn, "select ?, ?, ?", 4, 3))
            doTest()

        # bindParamsSimple
        test "Not enough multiple parameters creates error, dynamic query":
            proc doTest =
                let qry = "select ?, ?, ?"
                discard templ(conn, qry, 4, 3)
            expect(OdbcException):
                doTest()

    # bindParamsKeyVal
    suite "Named `bindParams`" & name:
        setup:
            var conn = newOdbcConn(dbServer, dbUser, dbPass)

        test "One int param":
            proc doTest =
                check templ(conn, "select ?x", x=3).firstOrDefault(int) == 3
            doTest()

        test "Two int params":
            proc doTest =
                check templ(conn, "select ?x, ?y", x=3, y=4)
                    .firstOrDefault((int, int)) == (3, 4)
            doTest()

        test "One string param":
            proc doTest =
                check templ(conn, "select ?x", x="Hello, world!")
                    .firstOrDefault(string) == "Hello, world!"
            doTest()

        test "Two string params":
            proc doTest =
                check templ(conn, "select ?x, ?y", x="Hello", y="world!")
                    .firstOrDefault((string, string)) == ("Hello", "world!")
            doTest()

        test "Two string params in reverse order":
            proc doTest =
                check templ(conn, "select ?y, ?x", x="Hello", y="world!")
                    .firstOrDefault((string, string)) == ("world!", "Hello")
            doTest()

        test "Two string params with duplicate value":
            proc doTest =
                check templ(conn, "select ?x, ?x", x="Hello")
                    .firstOrDefault((string, string)) == ("Hello", "Hello")
            doTest()

        test "One int and one string param":
            proc doTest =
                check templ(conn, "select ?x, ?y", x=4, y="world!")
                    .firstOrDefault((int, string)) == (4, "world!")
            doTest()

        test "Using simple parameters with named parameter query treats query as simple":
            proc doTest =
                check templ(conn, "select ?x", 3).firstOrDefault(int) == 3
            doTest()

        test "Using named parameters with simple parameter query does not compile":
            proc doTest =
                check not compiles(templ(conn, "select ?", x=3))
            doTest()

        test "Not enough named parameters creates error, static query":
            proc doTest =
                check not compiles(templ(conn, "select ?x, ?y", x=4))
                check not compiles(templ(conn, "select ?x, ?y", y=4))
            doTest()

        test "Non-static query text does not compile":
            proc doTest =
                let qry = "select ?val"
                check not compiles(templ(conn, qry, val = 3))
            doTest()

template exec1(conn: OdbcConn, qry: string, params: varargs[untyped]): auto =
    conn.exec(qry, params)
template exec2(conn: OdbcConn, qry: string, params: varargs[untyped]): auto =
    let stmt = conn.prep(qry)
    stmt.exec(params)

testBothPrepNonPrep(" - non-prep", exec1)
testBothPrepNonPrep(" - prep", exec2)

suite "Object/tuple typed bindParams":
    type TestObj = object
        anInt: int
        veryLong: string

    # NOTE: These must be `let` instead of `const`, so that the object fields
    # are addressable. Reason is because of a Nim bug where Nim decides that
    # fields in a `const` object are addressable, but inlines the field values
    # in the generated C code (which makes them non-addressable), which GCC
    # throws an error about.
    let
        exp1 = TestObj(anInt: 42, veryLong: "Hello")
        exp2 = TestObj(anInt: 64, veryLong: "ø")

    setup:
        var conn = newOdbcConn(dbServer, dbUser, dbPass)
        proc doSetup =
            discard conn.exec "create table temptab (AnInt int, AString nvarchar(64))"
        doSetup()
        proc checkInsert =
            var stmt = conn.prep("select * from temptab")
            stmt.withExec:
                let got1 = rs.items(TestObj).toSeq
                check got1 == @[exp1, exp2]
            stmt.withExec:
                let got2 = rs.items((int, string)).toSeq
                check got2 == @[(exp1.anInt, exp1.veryLong), (exp2.anInt, exp2.veryLong)]
        proc check1Insert =
            var stmt = conn.prep("select * from temptab")
            stmt.withExec:
                let got1 = rs.items(TestObj).toSeq
                check got1 == @[exp1]
            stmt.withExec:
                let got2 = rs.items((int, string)).toSeq
                check got2 == @[(exp1.anInt, exp1.veryLong)]

    teardown:
        tdConn conn

    # This tests that UB does not occur in the implementation. The `string`
    # input must be converted to `seq[Utf16Char]` before being bound. If this
    # test succeeds, then the `seq[Utf16Char]` does not go out of scope before
    # the `SQLExecute`.
    test "Object with simple parameters, prep":
        proc doTest =
            let stmt = conn.prep "insert into temptab values (?, ?)"
            stmt.execOnly exp1
            stmt.execOnly exp2
        doTest()
        checkInsert()

    test "Object with simple parameters, non-prep":
        proc doTest =
            let stmt = conn.exec("insert into temptab values (?, ?)", exp1)
        doTest()
        check1Insert()

    test "Object with same-ordered named parameters, prep":
        proc doTest =
            let stmt = conn.prep "insert into temptab values (?anInt, ?veryLong)"
            stmt.execOnly exp1
            stmt.execOnly exp2
        doTest()
        checkInsert()

    test "Object with same-ordered named parameters, non-prep":
        proc doTest =
            discard conn.exec("insert into temptab values (?anInt, ?veryLong)", exp1)
        doTest()
        check1Insert()

    # Tests that fields in object is matched correctly
    test "Object with reverse-ordered named parameters, prep":
        proc doTest =
            let stmt = conn.prep "insert into temptab (AString, AnInt) values (?veryLong, ?anInt)"
            stmt.execOnly exp1
            stmt.execOnly exp2
        doTest()
        checkInsert()

    # Tests that fields in object is matched correctly
    test "Object with reverse-ordered named parameters, non-prep":
        proc doTest =
            discard conn.exec("insert into temptab (AString, AnInt) values (?veryLong, ?anInt)", exp1)
        doTest()
        check1Insert()

    test "Object field can be mapped to several parameters":
        proc doTest =
            type TestObj2 = object
                anInt, twoInt: int
            let thisExp = TestObj2(anInt: 4, twoInt: 9)
            let got = conn.exec("select ?twoInt, ?twoInt", thisExp).
                items((int, int)).toSeq
            check got == @[(9, 9)]
        doTest()

    test "Tuple; prep":
        proc doTest =
            let stmt = conn.prep "insert into temptab values (?, ?)"
            let
                tup1 = (exp1.anInt, exp1.veryLong)
                tup2 = (exp2.anInt, exp2.veryLong)
            stmt.execOnly tup1
            stmt.execOnly tup2
        doTest()
        checkInsert()

    test "Tuple; non-prep":
        proc doTest =
            let tup = (exp1.anInt, exp1.veryLong)
            discard conn.exec("insert into temptab values (?, ?)", tup)
        doTest()
        check1Insert()

    test "Object with more fields than parameters is fine; prep":
        type TestObj2 = object
            anInt, twoInt: int
            aStr: string
        let
            tst1 = TestObj2(anInt: exp1.anInt, twoInt: 4, aStr: exp1.veryLong)
            tst2 = TestObj2(anInt: exp2.anInt, twoInt: 3, aStr: exp2.veryLong)
        proc doTest =
            let stmt = conn.prep "insert into temptab values (?anInt, ?aStr)"
            stmt.execOnly tst1
            stmt.execOnly tst2
        doTest()
        checkInsert()

    test "Object with more fields than parameters is fine; non-prep":
        type TestObj2 = object
            anInt, twoInt: int
            aStr: string
        let tst = TestObj2(anInt: exp1.anInt, twoInt: 4, aStr: exp1.veryLong)
        proc doTest =
            discard conn.exec("insert into temptab values (?anInt, ?aStr)", tst)
        doTest()
        check1Insert()

    test "Individual values; prep":
        proc doTest =
            let stmt = conn.prep "insert into temptab values (?anInt, ?veryLong)"
            stmt.execOnly(anInt = exp1.anInt, veryLong = exp1.veryLong)
            stmt.execOnly(anInt = exp2.anInt, veryLong = exp2.veryLong)
        doTest()
        checkInsert()

    test "Individual values; non-prep":
        proc doTest =
            discard conn.exec("insert into temptab values (?anInt, ?veryLong)",
                anInt = exp1.anInt, veryLong = exp1.veryLong)
        doTest()
        check1Insert()

    test "Parameters not in object is an error; prep":
        proc doTest =
            let stmt = conn.prep("select ?nonExistingParam")
            check not compiles(conn.execOnly(exp1))
        doTest()

    test "Parameters not in object is an error; non-prep":
        proc doTest =
            check not compiles(conn.exec("select ?nonExistingParam", exp1))
        doTest()

suite "Object/tuple typed getDatas":
    type TestObj = object
        anInt: int
        veryLong: string
    let
        exp1 = TestObj(anInt: 42, veryLong: "Hello")
        exp2 = TestObj(anInt: 64, veryLong: "ø")

    setup:
        var conn = newOdbcConn(dbServer, dbUser, dbPass)
        proc doSetup =
            discard conn.exec "create table temptab (AnInt int, AString nvarchar(64))"
            let stmt = conn.prep "insert into temptab values (?anInt, ?veryLong)"
            stmt.execOnly exp1
            stmt.execOnly exp2
        doSetup()

    teardown:
        tdConn conn

    test "Manual `next` works":
        proc testInner =
            let rs = conn.exec "select * from temptab"
            var row: TestObj
            check rs.next row
            check row == exp1
            check rs.next row
            check row == exp2
            check not rs.next
        testInner()

    test "Iterator works":
        proc doTest =
            let rs = conn.exec "select * from temptab"
            let got = rs.items(TestObj).toSeq
            check got == @[exp1, exp2]
        doTest()

    test "Tuple works":
        proc doTest =
            let rs = conn.exec "select * from temptab"
            let got = rs.items((int, string)).toSeq
            check got == @[(exp1.anInt, exp1.veryLong), (exp2.anInt, exp2.veryLong)]
        doTest()

suite "Select into typed scalar type":
    setup:
        var conn = newOdbcConn(dbServer, dbUser, dbPass)

    teardown:
        tdConn conn

    test "string":
        proc doTest =
            discard conn.exec "create table temptab (AStr nvarchar(20))"
            discard conn.exec "insert into temptab values (N'Hey')"
            let got = conn.exec("select * from temptab").items(string).toSeq
            check got == @["Hey"]
        if dbms == "SQLite":
            skip
        else:
            doTest()

    test "int":
        proc doTest =
            discard conn.exec "create table temptab (AStr int)"
            discard conn.exec "insert into temptab values (42)"
            let got = conn.exec("select * from temptab").items(int).toSeq
            check got == @[42]
        doTest()

    test "many strings":
        proc doTest =
            discard conn.exec "create table temptab (AStr nvarchar(32))"
            discard conn.exec "insert into temptab values (N'Hey')"
            discard conn.exec "insert into temptab values (N'ą')"
            let got = conn.exec("select * from temptab").items(string).toSeq
            check got == @["Hey", "ą"]
        if dbms == "SQLite":
            skip
        else:
            doTest()

    test "Tuple":
        proc doTest =
            discard conn.exec "create table temptab (AStr nvarchar(32), AInt int)"
            discard conn.exec "insert into temptab values (N'Hey', 42)"
            discard conn.exec "insert into temptab values (N'ø', 64)"
            let got = conn.exec("select * from temptab").items((string, int)).toSeq
            check got == @[("Hey", 42), ("ø", 64)]
            check got[0].type is (string, int)
        if dbms == "SQLite": # SQLite doesn't support N
            skip
        else:
            doTest()

    test "Optional some":
        proc doTest =
            check conn.exec("select 3").items(Option[int]).toSeq == @[some(3)]
        doTest()

    test "Optional none":
        proc doTest =
            check conn.exec("select NULL").items(Option[int]).toSeq == @[none(int)]
        doTest()

    test "openArray":
        proc doTest =
            proc aux(rs: auto, x: var openArray[byte]) =
                check rs.next(x)
            let param: array[4, byte] = [1'u8, 4, 5, 1]
            let rs = conn.exec("select ?", param)
            var ret: array[4, byte]
            aux(rs, ret)
            check param == ret
        doTest()

proc setupDb =
    let conn = connString.newOdbcConn
    if dbms == "SQLite":
        for tab in conn.exec("select name from sqlite_master where type = 'table'").items(string):
            discard conn.exec("drop table " & tab)
        #discardResults conn.exec"pragma writable_schema = 1"
        #discardResults conn.exec"delete from sqlite_master"
        #discardResults conn.exec"pragma writable_schema = 0"
        #discardResults conn.exec"vacuum"
        #discardResults conn.exec"pragma integrity_check"
        discard conn.exec"create table abc (VeryLong text)"
    else:
        discard conn.exec"drop database if exists test"
        discard conn.exec"create database test"
        conn.setCatalog "test"
        discard conn.exec"create table abc (VeryLong varchar(max))"
    discard conn.exec"create table TwoValue (SomeName nvarchar(100), SomeValue int)"
    discard conn.exec"create table HasGuid (SomeGuid uniqueidentifier)"
    discard conn.exec"create table FixedLenTypes (SomeInt int, SomeFloat float)"
    discard conn.exec"create table SameTypes (SomeInt1 int, SomeInt2 int)"
setupDb()

# This only checks SQL Server data types, but this should not be generic
# enough.
if dbms != "SQLite":
    suite "Statements that check if SQL Server data types work":
        setup:
            var conn = newOdbcConn(dbServer, dbUser, dbPass)

        teardown:
            tdConn conn

        test "integer":
            proc doTest =
                let val = conn.exec("select 3").firstOrDefault(OdbcValue)
                check val.kind == otInt32
                check val.i64 == 3
            doTest()
        test "smallint":
            proc doTest =
                let val = conn.exec("select cast(2 as smallint)").firstOrDefault(OdbcValue)
                check val.kind == otInt16
                check val.i64 == 2
            doTest()
        test "tinyint":
            proc doTest =
                let val = conn.exec("select cast(5 as tinyint)").firstOrDefault(OdbcValue)
                check val.kind == otInt8
                check val.i64 == 5
            doTest()
        test "bit":
            proc doTest =
                let val = conn.exec("select cast(1 as bit)").firstOrDefault(OdbcValue)
                check val.kind == otBool
                check val.i64 == 1
            doTest()
        test "bigint":
            proc doTest =
                let val = conn.exec("select cast(5000000000 as bigint)").firstOrDefault(OdbcValue)
                check val.kind == otInt64
                check val.i64 == 5_000_000_000
            doTest()

        test "float32":
            proc doTest =
                let val = conn.exec("select cast(345.32 as real)").firstOrDefault(OdbcValue)
                check val.kind == otFloat32
                check almostEqual(val.f32, 345.32)
            doTest()

        test "float64":
            proc doTest =
                let val = conn.exec("select cast(3124.32 as double precision)")
                    .firstOrDefault(OdbcValue)
                check val.kind == otFloat64
                check almostEqual(val.f64, 3124.32)
            doTest()
        test "string":
            proc doTest =
                let val = conn.exec("select 'Hey'").firstOrDefault(OdbcValue)
                check val.kind == otWideArray
                check val.wchars.len == 3
                check $val == "Hey"
            doTest()
        test "widestring":
            proc doTest =
                let val = conn.exec("select N'Hey'").firstOrDefault(OdbcValue)
                check val.kind == otWideArray
                check $val == "Hey"
            doTest()
        test "binary":
            proc doTest =
                let val = conn.exec("select cast(123456 as binary(4))")
                    .firstOrDefault(OdbcValue)
                check val.kind == otByteArray
                check val.bytes == @[0x00'u8, 0x01, 0xe2, 0x40]
                check $val == "0001E240"
            doTest()
        test "date":
            proc doTest =
                let val = conn.exec("select datefromparts(2020, 3, 4)")
                    .firstOrDefault(OdbcValue)
                check val.kind == otDate
                check $val == "2020-03-04"
            doTest()
        test "time":
            proc doTest =
                let val = conn.exec("select timefromparts(13, 32, 4, 321, 4)")
                    .firstOrDefault(OdbcValue)
                check val.kind == otTime
                check $val == "13:32:04"
            doTest()

        test "null string produces empty string":
            proc doTest =
                let val = conn.exec("select cast(null as varchar(5))").firstOrDefault(OdbcValue)
                check val.kind == otWideArray
                check $val == ""
            doTest()

        # NOTE: Non-zero milliseconds cause rounding which makes a `check`
        # unfeasible. This is likely fine in normal scenarios as the `datetime`
        # type is stored in SQL Server as floating point anyway. Use `datetime2`
        # or a float-based datetime instead.
        test "Simple timestamp select":
            proc doTest =
                let val = conn.exec("select datetimefromparts(2021, 10, 5, 13, 32, 4, 0)")
                    .firstOrDefault(OdbcValue)
                check val.kind == otTimestamp
                check $val == "2021-10-05T13:32:04.000000000"
            doTest()

        test "Simple timestamp2 select":
            proc doTest =
                let val = conn.exec("select datetime2fromparts(2021, 10, 5, 13, 32, 4, 323456, 7)")
                    .firstOrDefault(OdbcValue)
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
                discard conn.exec"create table temptab (VeryLong varchar(max))"
                let param = 'b'.repeat(3500)
                discard conn.exec("insert into temptab values (?)", param)
                # Check that actual number of bytes are correct
                check conn.exec("select {fn octet_length(VeryLong)} from temptab").firstOrDefault(int) == 3500
                # Check that string doesn't have spaces in it (it should be just 'a's)
                check conn.exec("select {fn length(VeryLong)} from temptab").firstOrDefault(int) == 3500
                check conn.exec("select VeryLong from temptab").firstOrDefault(string) == param
            doTest()
        test "Parameterized widestrings is written correctly to nvarchar column":
            proc doTest =
                discard conn.exec"create table temptab (VeryLong nvarchar(max))"
                let param = 'b'.repeat(3500)
                discard conn.exec("insert into temptab values (?)", param)
                # Check that actual number of bytes are correct
                check conn.exec("select {fn octet_length(VeryLong)} from temptab")
                    .firstOrDefault(int) == 3500 * 2
                # Check that string doesn't have spaces in it (it should be just 'a's)
                check conn.exec("select {fn length(VeryLong)} from temptab").firstOrDefault(int) == 3500
                check conn.exec("select VeryLong from temptab").firstOrDefault(string) == param
            doTest()

        test "Unicode characters are inserted correctly":
            proc doTest =
                discard conn.exec"create table temptab (SomeCol nvarchar(max))"
                # æ is 1 byte ANSI / 2 byte UTF-8/16
                # シ is 2 byte UTF-16 / 3 byte UTF-8
                const unicode = "løæシ"
                check unicode.newWideCString.len == 4 # Sanity check
                discard conn.exec("insert into temptab values (?)", unicode)
                check conn.exec("select {fn length(SomeCol)} from temptab").firstOrDefault(int) == 4
                check conn.exec("select SomeCol from temptab").firstOrDefault(string) == unicode
            doTest()

        test "Variable sized data":
            proc testInner =
                discard conn.exec"create table temptab (VeryLong varchar(max))"

                discard conn.exec("insert into temptab values (?)", 'b'.repeat(3000))
                check conn.exec("select {fn octet_length(VeryLong)} from temptab").firstOrDefault(int) == 3000
                check conn.exec("select {fn length(VeryLong)} from temptab").firstOrDefault(int) == 3000
                discard conn.exec("update temptab set VeryLong = VeryLong + ?",
                                           'a'.repeat(1500))
                check conn.exec("select {fn length(VeryLong)} from temptab").firstOrDefault(int) == 4500
                let val = conn.exec("select VeryLong from temptab").firstOrDefault(OdbcValue)
                check val.wchars.len == 4500
            testInner()

        test "Transactions":
            proc doTest =
                check not conn.inTran
                discard conn.exec"create table temptab (SomeCol nvarchar(max))"
                conn.beginTran
                discard conn.exec("insert into temptab values (?)", "hey")
                discard conn.exec("update temptab set SomeCol = ?", "no")
                conn.commitTran
                check conn.exec("select SomeCol from temptab").first(string) == some("no")
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
        test "Money types":
            proc doTest =
                discard conn.exec"create table temptab (Cash money)"
                discard conn.exec"insert into temptab values (32.43)"
                let val = conn.exec("select * from temptab").firstOrDefault(OdbcValue)
                check val.kind == otCharArray
                check $val == "32.4300"
            doTest()

        test "Datetime types":
            type TestObj = object
                someDate: OdbcDate
            proc doTest =
                block:
                    let stmt = conn.prep("select {d '2020-03-04'} as someDate")
                    let rs = stmt.exec
                    check rs.next
                    var row: TestObj
                    rs.getDatas row
                    echo row.repr
                    check row.someDate.year == 2020
                    check row.someDate.month == 3
                    check row.someDate.day == 4
                block:
                    let stmt = conn.prep("select {d '2020-03-04'} as someDate")
                    check stmt.exec.firstOrDefault(OdbcDate) == OdbcDate(year: 2020, month: 3, day: 4)
            doTest()

    # sqlite always stores utf-8
    suite "UTF-8 string with ANSI equivalent is stored as ANSI in varchar":
        setup:
            var conn = newOdbcConn(dbServer, dbUser, dbPass)
            let utf8Str = "ø"
            require utf8Str.len == 2
            let ansiStr = utf8Str.convert("CP1252", "UTF-8")
            require ansiStr.len == 1
            let collate = "latin1_general_100_ci_as_sc"
            proc doSetup =
                discard conn.exec "create table temptab (SomeCol varchar(max) collate " & collate & ")"
                discard conn.exec("insert into temptab values (?)", utf8Str)
            doSetup()
    
        teardown:
            tdConn conn
    
        test "Number of bytes is 1 in ANSI":
            proc doTest =
                check conn.exec("select {fn octet_length(SomeCol)} from temptab").first(int) == some(1)
            doTest()
    
        test "Number of characters is 1 in ANSI":
            proc doTest =
                check conn.exec("select {fn length(SomeCol)} from temptab").first(int) == some(1)
            doTest()
    
        # NOTE: This cannot be tested for "ODBC Driver 13 for SQL Server" and
        # newer for Linux, because encoding for getting data is always UTF-8.
        # https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/programming-guidelines?view=sql-server-ver16#character-set-support
        #test "Binary representation is in ANSI":
        #    proc doTest =
        #        check conn.exec("select SomeCol from temptab")
        #            .first(seq[char]) == some(ansiStr.toSeq)
        #    doTest()
        test "String representation is in UTF-8":
            proc doTest =
                check conn.exec("select SomeCol from temptab").first(string) == some(utf8Str)
            doTest()

suite "NULL and empty string":
    setup:
        var conn = newOdbcConn(dbServer, dbUser, dbPass)

    teardown:
        tdConn conn

    test "Parameterized empty string":
        proc doTest =
            let ret = conn.exec("select ?", "").first(string).get
            check ret == ""
        doTest()

    test "Parameterized empty string in NOT NULL column":
        proc doTest =
            discard conn.exec"create table temptab (SomeCol nvarchar(20) not null)"
            discard conn.exec("insert into temptab (SomeCol) values (?)", "")
            let ret = conn.exec("select SomeCol from temptab").first(string).get
            check ret == ""
        doTest()

    test "Parameterized empty string in NULL column":
        proc doTest =
            discard conn.exec"create table temptab (SomeCol nvarchar(20) null)"
            discard conn.exec("insert into temptab (SomeCol) values (?)", "")
            let ret = conn.exec("select SomeCol from temptab").first(string).get
            check ret == ""
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
#            discard conn.exec "create table temptab (SomeCol varchar(max))"
#            discard conn.exec("insert temptab values (?)", utf8Str)
#        doSetup()
#
#    # Succeeds
#    test "Number of bytes takes 2-byte UTF-8 into account":
#        proc doTest =
#            check conn.execScalar"select {fn octet_length(SomeCol)} from temptab".toInt == 2
#        doTest()
#
#    # Fails: Result is 2 (indicates it doesn't understand it's UTF-8)
#    test "Number of characters is number of bytes":
#        proc doTest =
#            check conn.execScalar"select {fn length(SomeCol)} from temptab".toInt == 1
#        doTest()
#
#    # Succeeds
#    test "Binary representation is the same UTF-8 sequence sent in":
#        proc doTest =
#            block:
#                type BinaryStr = object
#                    someCol: seq[byte]
#                let rs = conn.exec("select SomeCol from temptab")
#                check rs.items(BinaryStr).toSeq == @[BinaryStr(someCol: utf8Str)]
#        doTest()
#
#    # Fails: Returns (0xC383_C2B8), expected (0xC3B8); likely thinks varchar
#    # data is in some other encoding
#    test "String representation is correct UTF-8":
#        proc doTest =
#            echo ($conn.execScalar"select SomeCol from temptab").repr
#            check $conn.execScalar"select SomeCol from temptab" == "ø"
#        doTest()

proc setupTestConn: OdbcConn =
    var noConn = newOdbcNoConn()
    var conn = noConn.connect(dbServer, dbUser, dbPass)
    if dbms != "SQLite":
        conn.setCatalog "test"
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

    teardown:
        tdConn conn

    test "Binding columns with compile-time helper":
        proc testInner =
            block:
                let stmt = conn.prep("insert into FixedLenTypes values (?, ?)")
                stmt.execOnly(4, 5.67890123456)
                stmt.execOnly(5, 3.45678901234)
            let stmt = conn.prep("select * from FixedLenTypes")
            type Row = object
                someInt: int
                someFloat: float
            var row: Row
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
            discard conn.exec("insert into abc values (?)", 'b'.repeat(3000))
            if dbms != "SQLite":
                check conn.exec("select {fn octet_length(VeryLong)} from abc").firstOrDefault(int) == 3000
            check conn.exec("select {fn length(VeryLong)} from abc").firstOrDefault(int) == 3000
            let concatOp =
                if dbms == "SQLite":
                    "||"
                else:
                    "+"
            discard conn.exec(&"update abc set VeryLong = VeryLong {concatOp} ?",
                                       'a'.repeat(1500))
            check conn.exec("select {fn length(VeryLong)} from abc").firstOrDefault(int) == 4500
            let stmt = conn.prep"select VeryLong from abc"
            check stmt.exec.firstOrDefault(string).len == 4500
        testInner()

    test "Bind column locally":
        type TestObj = object
            veryLong: array[50, char]
        proc testInner =
            discard conn.exec("insert into abc values (?)", "32")
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
                let stmt = conn.prep("insert into TwoValue (SomeName) values (?)")
                stmt.execOnly(unicode)
            block:
                let stmt = conn.prep("select {fn length(SomeName)} as leng from TwoValue")
                check stmt.exec.first(int) == some(4)
            block:
                let stmt = conn.prep("select SomeName from TwoValue")
                check stmt.exec.first(string) == some(unicode)
        doTest()

    # Both result sets go out of scope at the same time, i.e. at the end of the
    # proc. `discard stmt1.exec` doesn't make the return result set go out of
    # scope immediately.
    test "Multiple result sets in scope fails":
        proc doTest =
            block:
                let stmt = conn.prep("insert into TwoValue (SomeName) values (?)")
                stmt.execOnly("Hello")
            let stmt1 = conn.prep("select {fn length(SomeName)} as leng from TwoValue")
            discard stmt1.exec
            let stmt2 = conn.prep("select SomeName from TwoValue")
            discard stmt2.exec
        if dbms == "SQLite": # seems to allow multi-statements per conn
            skip
        else:
            expect Exception:
                doTest()

    test "Custom query type":
        type TestObj = object
            veryLong: int32
        proc testInner =
            discard conn.exec("insert into abc values (?)", "3")
            let stmt = conn.prep("select cast(VeryLong as int) as veryLong from abc")
            var row: TestObj
            let ds = stmt.exec
            check ds.next
            ds.getDatas row
            check row.veryLong == 3
            check ds.next == false
        testInner()

    # Removed test motivation: can no longer infer result set column names at
    # compile-time
    #test "Multiple column custom query type in reverse order":

    test "Insert with parameter type":
        type TestObj = object
            veryLong: seq[Utf16Char]
        proc testInner =
            let inp = TestObj(veryLong: utf8To16"hey")
            block:
                let stmt = conn.prep("insert into abc values (?veryLong)")
                stmt.execOnly inp
            block:
                let stmt = conn.prep("select * from abc")
                let ds = stmt.exec
                var outp: TestObj
                check ds.next(outp) == true
                check outp == inp
                check ds.next(outp) == false
        testInner()

    test "Insert with parameter type in reverse order with supplied order":
        type TestObj = object
            someValue: int
            someName: seq[Utf16Char]
        proc testInner =
            let inp = TestObj(someValue: 5, someName: utf8To16"Yup!")
            block:
                let stmt = conn.prep("insert into TwoValue values (?someName, ?someValue)")
                stmt.bindParams(["someName", "someValue"], inp)
                stmt.execOnly
            block:
                let stmt = conn.prep("select someValue, someName from TwoValue")
                let ds = stmt.exec
                var outp: TestObj
                check ds.next(outp) == true
                check outp == inp
                check ds.next(outp) == false
        testInner()

    test "explicit bindParams is memory safe for arrays":
        proc doTest =
            var myArr: array[10, char]
            let str = "Hello"
            copyMem(myArr[0].addr, str.cstring, str.len)
            let stmt = conn.prep("select ?")
            block:
                stmt.bindParams([""], myArr)
            let x = int.high # just put something in scope
            check stmt.exec.firstOrDefault(myArr.type) == myArr
        doTest()

    test "Reuse bindParams object works":
        proc doTest =
            type MyObj = object
                x: array[4, char]
                y: int
            let inps = [
                MyObj(x: ['a', 'b', '\0', '\0'], y: 3),
                MyObj(x: ['c', 'a', '\0', '\0'], y: 0),
            ]

            # Length of 2 checks that only null-terminated string is used, and
            # not the 2 extra null-bytes. Otherwise "Truncation error" would
            # occur in ODBC.
            discard conn.exec "create table temptab (c1 varchar(2), c2 int)"

            block:
                let stmt = conn.prep("insert into temptab values (?, ?)")
                var inp = inps[0]
                stmt.bindParams(inp)
                stmt.execOnlyKeepParams

                inp = inps[1]
                stmt.execOnlyKeepParams

            block:
                let rs = conn.exec("select * from temptab")
                var inp: MyObj
                for i in 0..1:
                    check rs.next(inp)
                    check inp == inps[i]
                check not rs.next
        doTest()

    test "Parameter key-vals that don't cover all parameters creates error":
        proc testInner =
            let stmt = conn.prep("insert into TwoValue values (?someName, ?someValue)")
            check not compiles(stmt.exec(someValue = 3))
        testInner()

    test "Multiple executions of static insert query":
        type TestObj = object
            someValue: int
            someName: array[2, char]
        proc testInner =
            block:
                let stmt = conn.prep("insert into TwoValue values (?someName, ?someValue)")
                for i in 0..3:
                    let inp = TestObj(someName: toArray[2]($i), someValue: i)
                    stmt.execOnly inp
            block:
                let stmt = conn.prep("select * from TwoValue")
                var row: TestObj
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
                let stmt = conn.prep("insert into HasGuid values (?)")
                stmt.execOnly(guid)
            block:
                let stmt = conn.prep("select * from HasGuid")
                let retGuid = stmt.exec.firstOrDefault(GUID)
                check retGuid == guid
        if dbms == "SQLite": # does not support GUIDs
            skip
        else:
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
            ty {.odbcCol: 4.}: array[16, char] # {fn length(LOCAL TEMPORARY)} + 1

        template cs(obj): untyped =
            $(obj[0].unsafeAddr.cstring)

        var
            # NOTE: "ODBC Driver for SQL Server" requires catalog and schema to
            # be specified. It will not default to the currently active
            # database.
            ds = conn.newStmt.selectTables(
                catalog = "test",
                schema = "dbo",
                table = "%Types",
                tableType = {ttTable})
            row: OdbcTables
        ds.bindCols row
        check ds.next
        check cs(row.table) == "FixedLenTypes"
        check ds.next
        check cs(row.table) == "SameTypes"
        check not ds.next

suite "Default values":
    setup:
        var conn = setupTestConn()

    test "first default value":
        proc doTest =
            let row = conn.exec("select * from TwoValue").first(OdbcRowSet)
            check row.isNone
        doTest()

    test "getOrDefault":
        proc doTest =
            var row: OdbcRowSet
            check row.getOrDefault(0).isDefault
            check row.getOrDefault("Another non-existing column").isDefault
            check row.getOrDefault(0).isDefault
            check row.getOrDefault("Column").isDefault
        doTest()

suite "Iterators":
    setup:
        var conn = setupTestConn()
        proc doSetup =
            let stmt = conn.prep("insert into TwoValue values (?someName, ?someValue)")
            for i in 0..3:
                stmt.execOnly(i, $i)
        doSetup()

    test "Dynamic query":
        proc testInner =
            let rs = conn.exec("select * from TwoValue")
            for i, row in enumerate rs:
                check row["someValue"].i64 == i
                check $row["someName"] == $i
        testInner()

    # Tests `next(OdbcAnyResult, var OdbcValue)`
    test "OdbcValue iterator":
        proc doTest =
            let rs = conn.exec("select someValue from TwoValue")
            for i, row in enumerate(rs.items(OdbcValue)):
                check $row == $i
        doTest()

    test "Query with custom row type":
        type TwoValue = object
            someName: string
            someValue: int
        proc testInner =
            let rs = conn.exec("select * from TwoValue")
            for i, row in enumerate rs.items(TwoValue):
                check row.someValue == i
                check row.someName == $i
        testInner()
