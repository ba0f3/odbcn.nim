Abstraction of ODBC for Nim. Example code:

```nim
import odbcn
let conn = newOdbcConn(";Server=127.0.0.1;UID=sa;PWD=mybadpassword;Database=testing")
discard conn.exec("insert into MyTable (MyIntCol, MyStrCol) values (?, ?)", 3, "Hello")
for row in conn.exec "select * from MyTable":
  assert row[0].toInt == 3
  assert $row["MyStrCol"] == "Hello"
```

## Build status

#### Linux (x86_64)

[![builds.sr.ht status](https://builds.sr.ht/~mjaa/odbcn-nim/commits/master/.build.yml.svg)](https://builds.sr.ht/~mjaa/odbcn-nim/commits/master/.build.yml?)

#### Windows

No CI for Windows. If Linux CI works, Windows should work as well, because the
Microsoft ODBC API is platform-independent. If Windows version of this library
doesn't work, the C API wrapper is likely at fault.

## Links

* [Project page](https://sr.ht/~mjaa/odbcn-nim/)
* [Repo](https://git.sr.ht/~mjaa/odbcn-nim)

See [the docs](https://mjaa.srht.site/odbcn-nim/odbcn.html) for in-depth manual
to the various features and API reference.

If you encounter issues, please submit a ticket by sending an email to
[~mjaa/odbcn-nim@todo.sr.ht](mailto:~/mjaa/odbcn-nim@todo.sr.ht). You don't
need to be registered at sr.ht to issue a ticket this way. Subscribe to this
tracker by sending an email to
[~/mjaa/odbcn-nim/subscribe@todo.sr.ht](mailto:~/mjaa/odbcn-nim/subscribe@todo.sr.ht).

All contributions are done by means of `git send-email`. Send these to the
mailing list at
[~mjaa/odbcn-nim@lists.sr.ht](mailto:~mjaa/odbcn-nim@lists.sr.ht). See
`CONTRIBUTING.md` for details on this. Subscribe to the mailing list by sending
an email to
[~mjaa/odbcn-nim+subscribe@lists.sr.ht](mailto:~mjaa/odbcn-nim+subscribe@lists.sr.ht).

Read `CONTRIBUTING.md` for code guidelines. Testing is done by running
`inttests/odbc.nim`. These integration tests are not run by CI; they must be
run locally before pushing to master.

## Conditional symbols

Add these Nim compiler flags to change `odbcn` behavior.

* `-d:odbcNoEnvInit` - disable initialization of global ODBC environment
  * In this case, this variable must be manually initialized, or the `env`
    argument must be given in every applicable `odbcn` proc
* `-d:odbcEnvConnectionPooling` - enable connection pooling for global ODBC
  environment

## Alternatives to this library:

* std/db_odbc from Nim standard library
* [coffeepots/odbc](https://github.com/coffeepots/odbc)

Why choose this library over the above?

std/db_odbc is very minimal, and is made to integrate well with the other
database abstractions by generalizing the API. odbcn aims to utilize the whole
ODBC API, and as such doesn't try to generalize the API together with other
DBMS APIs. This simplifies understanding of low-level details and optimizing
access. At the cost of its minimal design, std/db_odbc lacks features such as
connecting to a database server with a connection string. std/db_odbc is also
unsafe in the case of database errors, because of inproper cleanup. This is the
case for the `fast*` procs and iterators.

coffeepots/odbc is more complex than std/db_odbc, and solves the problems
described above. What sets odbcn apart from that library is the design
philosophy. odbcn utilizes Nim RAII with no `ref` types, while all handle types
in coffeepots/odbc are `ref` types. The type system in odbcn emulates a subset
of [ODBC state transitions][1] by representing a handle with several types, and
having a handle instance transition between those types. This helps eliminate
runtime bugs such as executing a query on a handle in a result set state.

"coffeepots/odbc" doesn't have a public API, because it is pre-1.0.0, and there
hasn't been changes for over a year. This project also doesn't have a public
API, but aims to do so soon.

[1]: https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/appendix-b-odbc-state-transition-tables?view=sql-server-ver15

## Credit

["nanodbc" C++ ODBC abstraction](https://github.com/nanodbc/nanodbc/) helped in
the following issues:

* Simple module design: all needed code goes in one module (`private/core`);
  extensions go into separate modules
* Knowing which SQL data types to support, because there are very many of these
  SQL data types
* Knowing which C data types to support
  * E.g. don't support "SQL_NUMERIC_STRUCT" for fixed-precision data
    types, because it is poorly designed in ODBC; just use use the
    "string" C data type instead

## License

MIT

## Versioning

This project follows [SemVer 2.0.0](https://semver.org/spec/v2.0.0.html). At
the moment (pre-1.0.0), features and bug fixes increment patch version, and
backwards-incompatible API changes increment minor version. If all goes well, I
would like to bump to 1.0 at the start of 2023.

## Todos

For 1.0 release:

* [ ] Reorganize and tidy integration tests in more specific `suite`s
* [ ] Find as many bugs in the implementation as possible, and fix and make
  integration tests for them
* [ ] Document remaining functions in core.nim
* [x] In `odbcn/connstr`, add support of `{...}` for attribute values, which is
  used to quote the value if it contains unsupported characters in ODBC
* [ ] Use `openArray[char]` instead of `string` for `utf8To16` parameter
* [ ] Document or fix why the array fields in `OdbcValue` are hidden
* [ ] Make `listDrivers` and `listDataSources` use UTF-16 version?
  * Current implementation may get ANSI-encoded data, which is undesirable;
    must research this
* [ ] Rework `OdbcException` so that it contains `OdbcError` objects, and not
  just a `string`
* [ ] Restructure modules? (is `private/core` module appropriate?)
* [ ] Add `toBytes(OdbcValue): seq[byte]`
* [ ] Change `newOdbcConn(s: string): OdbcConn` to `newOdbcConn(s: string):
  OdbcResult[OdbcConn]`?
  * Motivation: This proc may be used to test if a connection is valid, so
    raising an exception is unnecessary
* [ ] Add `requiresInit` to all handle types (except `OdbcEnv`), because `nil`
  values are not allowed
* [x] Add support for `getData` of result data into a non-object
  * Motivation: Easier to do queries that only contain 1 column
* [ ] Add warning if a field in typed `next` is unused? Maybe not
* [ ] Document that for static `prep`-variant, fields for typed `next` must
  correlate with the column name, else the column will not be read into a field
* [x] Add UTF-8 encoding for `varchar` columns
* [x] Add UTF-16 conversion for `seq[char]` values?
* [ ] Add tests for typed `next(OdbcAnyResult, T)` (and consequently typed
  `items` iterator)
* [ ] Split SQL Server-specific integration tests into its own file
  * Motivation: integration tests can be run with SQLite
  * This makes it easier to run integrations tests with CI, because the
    heavy SQL Server dependencies (docker, FreeTDS, SQL Server) are not
    required
* [ ] Add `insert into` abstraction, that makes it easier to construct the
  query
* [ ] Add tests for overloading `bindCol`, `getData` and `bindParam` procs in
  foreign modules
* [ ] Add `tryExec*` and `tryConnect` on `OdbcConn` so that exceptions are not
  raised when it is possible that the operation fails
* [ ] Somehow improve the stack traces - the large amount of templates and
  macros make it hard to inspect the stack trace
* [ ] Add support for `Option[T]` types
  * With `bindParams` the NULL value is bound
  * With `getData`, value is assigned `none` if `ind == SQL_NULL_DATA`,
    otherwise `some` with the value of `SQLGetData`
  * Do not support `bindCols`, because there's no place to store the `ind`
    pointer (indicating if the `SQLFetch` got a NULL value) between call to
    `SQLBindCol` and `SQLFetch`
  * Not supported as nonsensical: `Option[seq[T]]` (or option of any
    sequence-like type)

Other tasks:

* [ ] Add std/times helper
* [ ] Add `bindCols` specialization for `OdbcGenTyPreparedStmt` with known
  column order
* [ ] Maybe provide a way to call dynamic version of `prep` to
  avoid code generation
* [ ] Add async-support (`SQLCompleteAsync`, etc.)
  * I don't know the scale of implementing async; I know async in Nim is very
    hard to extend at the moment, so this may be a futile effort
  * Will probably require existing structures to change
* [ ] Add ORM table insert abstraction (getting table schema and generating
  structures)
  * Possibly do this in a separate repo, to avoid bloat of features
