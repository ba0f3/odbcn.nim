Code style: 4 spaces for each tab. 80-100 textwidth. Use Nim version 1.6.2.
This version will be updated from time to time in a bump commit on the 2nd
number of the SemVer.

Please use [joelparkerhenderson/git-commit-message][1] as guideline for writing
commit messages. At the very least, all commit messages are required start with
a verb.

[1]: https://github.com/joelparkerhenderson/git-commit-message

# Testing

It's recommended to use `unixodbc` as Driver Manager, `freetds` as ODBC driver
and SQL Server in a Docker container to run the integration tests in
`inttests/odbc.nim`. The tests are designed for SQL Server so they may not work
with other DBMS's.

# Maintenance

* `CHOOSENIM_CHOOSE_VERSION` in `.build.yml` must be updated in a bump commit
