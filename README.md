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

If you encounter issues, please submit a ticket
[here](https://todo.sr.ht/~mjaa/odbcn-nim).

All contributions are done by means of `git send-email`, which are submitted
[here](https://lists.sr.ht/~mjaa/odbcn-nim). Read `CONTRIBUTING.md` for code
guidelines. Testing is done by running `inttests/odbc.nim`. These integration
tests are not run by CI; they must be run locally before pushing to master.

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
runtime bugs such as executing a query on a handle in a result set state. There
are many other differences.

[1]: https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/appendix-b-odbc-state-transition-tables?view=sql-server-ver15

## License

MIT

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

If all goes well, I would like to bump to 1.0 at the start of 2023.

Other tasks:

* [ ] Add std/times helper
* [ ] Add `bindCols` specialization for `OdbcGenTyPreparedStmt` with known
  column order
* [] Maybe provide a way to call dynamic version of `prep` to
  avoid code generation
