Code style: 4 spaces for each tab. 80-100 textwidth. Use Nim version 1.6.2.

Please use [joelparkerhenderson/git-commit-message][1] as guideline for writing
commit messages. At the very least, all commit messages are required start with
a verb.

[1]: https://github.com/joelparkerhenderson/git-commit-message

# Before coding

If you're making a fix of a bug, it's perfectly fine to push the fix to the
mailing list without making a ticket.

If you're making a feature, such as one from the TODOs in the README, always
issue a ticket first in which the design can be discussed. This is for the sake
of not wasting your time. The TODOs may be outdated, or I may disagree with the
design to the point of having to discard all the code.

# Submitting code

As stated in the README, this project uses `git send-email` instead of "PRs".
This means instead of opening PRs, you send an email with the `git send-email`
CLI command. The following setups `git send-email` for this repository:

```
cd odbcn-nim
git config sendemail.to ~mjaa/odbcn-nim@lists.sr.ht
```

Then use `git send-email` as described in [this
tutorial](https://git-send-email.io/).

# Testing

`inttests/odbc.nim` contains integration tests that are not executed by CI.
These must be executed locally. The convenience `run_full_test.sh` script runs
both integration tests and unit tests.

It's recommended to use `unixodbc` as Driver Manager, `freetds` as ODBC driver
and SQL Server in a Docker container to run the integration tests. The tests
are designed for SQL Server so they may not work with other DBMS's.

# Maintenance

* Version for `choosenim` command in `.build.yml` must be updated for new minor
  versions of the Nim compiler
