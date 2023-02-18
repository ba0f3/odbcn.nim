## Abstraction of ODBC, which is an API for accessing many different DBMSes.
## This library provides both a familiar API for creating connections and
## executing queries. The data design uses Nim's new RAII instead of `ref`s for
## managing FFI handles. This library can therefore be used in non-GC
## environments.
##
## This module provides practical instructions on how to use this library. All
## API functions can be found in the `private/core` module.
##
## This library is never intended to abstract more than ODBC itself, i.e. this
## shall not abstract Postgres or MySQL as well. This constraint allows this
## library to take full advantage of the low-level API and increases
## transparency to the users.
##
## ## String encoding
##
## This library handles strings exclusively as UTF-8 encoded (not ANSI or
## UTF-16). All strings from the user are assumed to be valid UTF-8, so
## undefined behavior may happen if this cannot be guaranteed. The motivation
## for keeping it UTF-8-only is to prevent encoding bugs and pitfalls.
##
## This library guarantees that all output strings are UTF-8 because it relies
## exclusively on the UTF-16 ODBC API for data retrieval. All input strings,
## such as parameters, are converted to UTF-16 before sending to ODBC. All
## output strings, such as values from `nvarchar` columns (UTF-16-encoded), or
## `varchar` columns (encoding depends on collation), are retrieved as UTF-16,
## and then converted to UTF-8 before being available to the user.
##
## This re-encoding or processing by the driver/DBMS can be avoided by not
## using `string`s, and instead using `seq[byte]` or `seq[char]`.
##
## ## Establishing connection
##
## You may use an ODBC connection string.
##
## ```nim
## let conn = newOdbcConn("Server=myserver;Driver=FreeTDS;UID=sa;PWD=;Trusted_Connection=no")
## ```
##
## See also
##
## * `newOdbcConn <odbcn/private/core.html#newOdbcConn,string>`_
## * [Connection string attributes supported by the "SQL Server Native Client" driver](https://docs.microsoft.com/en-us/sql/relational-databases/native-client/applications/using-connection-string-keywords-with-sql-server-native-client)
## * [Connection string examples for different drivers](https://www.connectionstrings.com/)
##
## Alternatively use a DSN, username and password tuple:
##
## ```nim
## let conn = newOdbcConn("mydsn", "sa", "badpassword")
## ```
##
## * `newOdbcConn <odbcn/private/core.html#newOdbcConn,string,string,string>`_
##
## The module `odbcn/connstr <odbcn/connstr.html>`_ provides a
## simple API for manipulating connection strings using `std/strtabs`. Note
## that connection string attribute keys are case-insensitive. The "Driver"
## attribute is required if a DSN or FILEDSN attribute is absent. List all
## available drivers on the system with the `listDrivers
## <odbcn/private/core.html#listDrivers.i>`_ iterator. List all DSNs on
## the system with the `listDataSources
## <odbcn/private/core.html#listDataSources.i,OdbcEnv,DataSourcesFilter>`_
## iterator.
##
## ## SELECT query
##
## Make result set variable
##
## ```nim
## type MyObject = object
##   myIntCol: int
##   myStringCol: string
##
## let rs = conn.exec "select MyIntCol, MyStringCol from MyTable"
## ```
##
## Alternatives that do the same in different ways:
##
## #### Typed
##
## Recommended when the desired types are known at compile-time.
##
## ```nim
## var row: MyObject
## while rs.next(row):
##   let anInt = row.myIntCol + 1
##   echo "My integer column: ", anInt
##   echo "My string column: ", row.myStringCol
## ```
##
## ```nim
## for row in rs.items(MyObject):
##   let anInt = row.myIntCol + 1
##   echo "My integer column: ", anInt
##   echo "My string column: ", row.myStringCol
## ```
##
## ```nim
## for row in rs.items((int, string)):
##   # `row` here is a tuple with `int` and `string` as types
##   let anInt = row[0] + 1
##   echo "My integer column: ", anInt
##   echo "My string column: ", row[1]
## ```
##
## #### Untyped
##
## Recommended when the desired types of the columns are not known at
## compile-time. It is better and simpler to use the typed approach above. The
## user must convert between types manually by checking `row.kind` with the
## untyped approach.
##
## ```nim
## var row: OdbcRowSet
## while rs.next(row):
##   let anInt = row["MyIntCol"].i64 + 1
##   echo "My integer column: ", anInt
##   echo "My string column: ", row[1]
## ```
##
## ```nim
## for row in rs.items(OdbcRowSet):
##   let anInt = row["MyIntCol"].i64 + 1
##   echo "My integer column: ", anInt
##   echo "My string column: ", row[1]
## ```
##
## #### Only getting first row/value
##
## If the result set is expected to return at most 1 row, use `first
## <odbcn/private/core.html#first,sinkOdbcAnyResult,typedesc[T]>`_. It returns
## an `Option` of the type specified as argument to `first`.
##
## ```nim
## let aTuple = conn.exec("select 3, 'Hey'").first((int, string))
## doAssert aTuple == some((3, "Hey"))
## ```
##
## ```nim
## let aRow = conn.exec("select 3, 'Hey'").first
## # `aRow` is the type `Option[OdbcRowSet]`
## doAssert aRow.get[0].i32 == 3
## doAssert $aRow.get[1] == "Hey"
## ```
##
## Can also get into a single value if there is only 1 column.
##
## ```nim
## let anInt = conn.exec("select 3").first(int)
## doAssert anInt == some(3)
## ```
##
## Another alternative is to use `firstOrDefault
## <odbcn/private/core.html#firstOrDefault,sinkOdbcAnyResult,typedesc[T]>`_.
##
## ```nim
## let anInt = conn.exec("select 3").firstOrDefault(int)
## doAssert anInt == 3
## ```
##
## ## Quick UPDATE query
##
## ```nim
## discard conn.exec "update MyTable set MyIntCol = 1"
## ```
##
## ## Quick parameterized query
##
## Note that all parameters for any query must be either indexed and named.
##
## #### Indexed parameters
##
## The easiest method of having parameters, and also the closest to the
## low-level implementation.
##
## ```nim
## let rs = conn.exec("""
## select MyIntCol, MyStringCol from from MyTable
## where MyIntCol > ?
## """, 4)
## # ... use `rs`
## ```
##
## #### Named parameters
##
## ```nim
## let rs = conn.exec("""
## select MyIntCol, MyStringCol from MyTable
## where MyIntCol > ?low and MyIntCol < ?high
## """, high = 9, low = 3)
## # ... use `rs`
## ```
##
## The order of the parameters may be in any order, and the names are compared
## ignoring case and style. Two parameters can have the same value:
##
## ```nim
## let rs = conn.exec("""
## select MyIntCol, MyStringCol from MyTable
## where MyIntCol > ?low or MyIntCol = ?low
## """, low = 3)
## # ... use `rs`
## ```
##
## ## Prepared SELECT query
##
## ```nim
## var stmt = conn.prep """
## select MyIntCol, MyStringCol from MyTable
## where MyIntCol > ?
## """
## var row: OdbcRowSet
## stmt.withExec(3):
##   while rs.next(row):
##     let anInt = row["MyIntCol"].i64
##     let aString = $row["MyStringCol"] & $anInt
## # something else here
## stmt.withExec(4):
##   # ...
## ```
##
## Here `rs` is an injected symbol for the result set. If that conflicts with
## other code, this is the equivalent code with more control:
##
## ```nim
## var stmt = conn.prep """
## select MyIntCol, MyStringCol from MyTable
## where MyIntCol > ?
## """
## var row: OdbcRowSet
## block:
##   let myResultSet = stmt.exec(3)
##   while myResultSet.next(row):
##     let anInt = row["MyIntCol"].toInt
##     let aString = $row["MyStringCol"] & $anInt
##   stmt = myResultSet.unbind
## # something else here
## block:
##   let myResultSet = stmt.exec(4)
##   # ...
##   stmt = myResultSet.unbind
## ```
##
## The previous code block shows how the prepared query transitions state into
## a result set (with `exec`), and back again (with `unbind`). The `withExec`
## macro is intended to hide this back-and-forth to make the code look cleaner.
##
## ## Prepared UPDATE query
##
## ```nim
## let stmt = conn.prep """
## update MyTable
## set MyIntCol += 1
## where MyIntCol < ?
## """
## for i in 3..5:
##   stmt.execOnly(i)
## ```
##
## `execOnly` is an alternative to `exec` that does not transition the object.
## This is convenient for prepared statements that don't return a result set.
## The equivalent example when using `exec`:
##
## ```nim
## var stmt = conn.prep """
## update MyTable
## set MyIntCol += 1
## where MyIntCol < ?
## """
## for i in 3..5:
##   let rs = stmt.exec(i)
##   # Here `rs.len` is available, which returns affected rows.
##   stmt = rs.unbind
## ```
##
## #### Typed parameter
##
## ```nim
## import odbcn
## type InsertObj = object
##   someValue: int32
##   someString: string
## let stmt = conn.prep "insert TwoValue (SomeValue, SomeString) values (?, ?)"
## let params = InsertObj(someValue: 3, someString: "Hello")
## stmt.execOnly params
## ```
##
## ## SELECT query with bound columns
##
## This section describes how to retrieve database rows into Nim `object`s
## instead of `OdbcRowSet`.
##
## ```nim
## type MyTable = object
##   myIntCol: int
##   myStringCol: array[256, char]
## let rs = conn.exec "select MyIntCol, MyStringCol from MyTable"
## var row: MyTable
## rs.bindCols row
## while rs.next:
##   echo "My int column: ", row.myIntCol
##   echo "My string column: ", cast[cstring](row.myStringCol[0].addr)
## ```
##
## Note that `bindCols` is unsafe! The raw memory addresses to the object
## (`row` in this case) are stored in the ODBC driver, and it's kept there
## until the result set is discarded. Therefore it's undefined behavior if the
## bound object changes address.
##
## Also note that field names of the object is not matched against the column
## name in the SQL query. The ordering of fields must match the ordering of the
## columns. Alternatively, annotate an object field with e.g. `{.odbcCol: 3.}`
## to make that field match with the 3rd column (counting from 1). However, all
## fields following an `odbcCol`-annotated field must then also be annotated
## with `odbcCol`.
##
## Use of `bindCols` can be done with both a quick or prepared result set. The
## constraint is that only fixed-length data types may be used, which means
## `string` cannot be used. `array[X, char]` may be used instead.
##
## ## UPDATE query with object parameter
##
## Instead of indexed or named parameters, an object can be used as parameter.
## Note that, generally, each field in the object is bound to each parameter in
## order.
##
## ```nim
## import odbcn
## type InsertObj = object
##   someValue: int32
##   someString: string
## let stmt = conn.prep "insert TwoValue (SomeValue, SomeString) values (?, ?)"
## let params = InsertObj(someValue: 3, someString: "Hello")
## stmt.execOnly params
## ```
##
## `string` fields are first converted to UTF-16 before being set as a
## parameter. As such, use `seq[Utf16Char]` instead to optimize this.
##
## .. note:: It is not possible to bind a `const`ant object because of a bug in
##   Nim.
##
## #### Named parameters
##
## Name the parameters to match parameters with object field names.
##
## ```nim
## import odbcn
## type InsertObj = object
##   anInt, twoInt: int
##   aString: string
## let params = InsertObj(anInt: 2, twoInt: 9, aString: "Hi")
## let rs = conn.exec("""
## insert into ThreeValue (SomeValue, SomeString, TwoString)
## values (?anInt, ?aString, ?aString)""",
## params)
## ```
##
## This is useful if
##
## * Only subset of the object fields are used as parameters, or
## * The parameters go in a specific order, or
## * A single object field is mapped to several parameters
##
## ## NULL values
##
## This library supports setting NULL parameters and getting NULL values from
## result sets. This is done with the `std/options.Option` type. The
## `OdbcValue` type does not differentiate between NULL and the default value,
## so use the `Option` type if it is important to differentiate.
##
## This shows how selecting NULL into an `Option` sets the value to `none`.
##
## ```nim
## import odbcn, std/options, std/sequtils
## let vals = conn.exec("select NULL").items(Option[int]).toSeq
## doAssert vals == @[none(int)]
## ```
##
## For non-`Option` types, the default value is set.
##
## ```nim
## import odbcn, std/sequtils
## let vals = conn.exec("select NULL").items(int).toSeq
## doAssert vals == @[0]
## ```
##
## This shows how to set a NULL parameter.
##
## ```nim
## import odbcn, std/options, std/sequtils
## let vals = conn.exec("select ?", none(int)).items(Option[int]).toSeq
## doAssert vals == @[none(int)]
## ```
##
## ## Specializing
##
## `odbcn` supports overloading custom types for parameters and row set
## columns. Main motivation for doing this is to make it simpler to send an
## `object` parameter, or get rows into `object`s, without having to use raw
## ODBC types such as `OdbcDate`, or `string`s for fixed-precision numbers.
##
## See module `dataschema <odbcn/dataschema.html>`_ for documentation.
##
## ## Query with multiple result sets
##
## This section describes how to manage multiple result sets. This happens when
## executing a query with multiple SELECTs (or statements that produce result
## sets), or when executing certain statements such as "BACKUP" from T-SQL.
##
## Use case for multiple SELECTs is unknown, but will likely involve calling
## `nextResultSet <odbcn/private/core.html#nextResultSet,OdbcStmt>`_.
##
## For the "BACKUP" statement, all result sets must be entered for the
## statement to fully execute, so it's not enough to just execute the query.
##
## ```nim
## discardResults conn.exec "BACKUP DATABASE MyDb TO DISK = '/tmp/mydb.bak'"
## ```
##
## This is enough because `execDiscard` makes sure all result sets are entered.
## However, a normal `exec` which returns a result set does not do so.
##
## ```nim
## let rs = conn.exec "BACKUP DATABASE MyDb TO DISK = '/tmp/mydb.bak'"
## while rs.nextResultSet:
##   discard
## ```
##
## # Troubleshooting
##
## ## RAII compile-time errors
##
## This error from `nim c` can occur sometimes:
##
## > '=copy' is not available for type <OdbcStmt>; requires a copy because it's
## > not the last read of 'stmt\`gensym0'; routine: conntmp
##
## This happens when the RAII-type is initialized at module-scope. In this
## case, the value must be created at proc-scope.

{.warning[UnusedImport]: off.}:
    import odbcn/[private/core, functions, connstr]
export core except bindParams
