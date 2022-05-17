# Initializes databases at compile-time for testing `prepExecuteNew` in
# `inttests.nim`.

import odbcn

proc main(connString: string) =
    let conn = connString.newOdbcConn
    discard conn.exec"drop database if exists test"
    discard conn.exec"create database test"
    conn.setCatalog "test"
    discard conn.exec"create table abc (VeryLong varchar(max))"
    discard conn.exec"create table TwoValue (SomeName nvarchar(100), SomeValue int)"
    discard conn.exec"create table HasGuid (SomeGuid uniqueidentifier)"
    discard conn.exec"create table FixedLenTypes (SomeInt int, SomeFloat float)"
    discard conn.exec"create table SameTypes (SomeInt1 int, SomeInt2 int)"

when isMainModule:
    import cligen
    dispatch main
