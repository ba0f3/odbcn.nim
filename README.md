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

## Links

* [Project page](https://sr.ht/~mjaa/odbcn-nim/)
* [Repo](https://git.sr.ht/~mjaa/odbcn-nim)

See [the docs](https://mjaa.srht.site/odbcn-nim/odbcn.html) for in-depth manual
to the various features and API reference.

If you encounter issues, please submit a ticket
[here](https://todo.sr.ht/~mjaa/odbcn-nim).

All contributions are done by means of `git send-email`, which are submitted
[here](https://lists.sr.ht/~mjaa/odbcn-nim). Read `CONTRIBUTING.md` for code
guidelines. Testing is done by running the integration tests in
`inttests/odbc.nim`.

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
philosophy. odbcn utilizes Nim RAII with no `ref` types (except for
`ConnString` helper), while all handle types in coffeepots/odbc are `ref`
types. The type system in odbcn emulates a subset of [ODBC state
transitions][1] by representing a handle with several types, and having a
handle instance transition between those types. This helps eliminate runtime
bugs such as executing a query on a handle in a result set state. There are
many other differences.

[1]: https://docs.microsoft.com/en-us/sql/odbc/reference/appendixes/appendix-b-odbc-state-transition-tables?view=sql-server-ver15

## License

MIT

## Todos

For 1.0 release:

* [ ] Reorganize and tidy integration tests in more specific `suite`s
* [ ] Find as many bugs in the implementation as possible, and fix and make
  integration tests for them

If all goes well, I would like to bump to 1.0 at the start of 2023.

Other tasks:

* [ ] Add std/times helper
* [ ] Add `bindCols` specialization for `OdbcGenTyPreparedStmt` with known
  column order
