# Package

version       = "0.5.4"
author        = "Håvard Mjaavatten"
description   = "Abstraction of ODBC"
license       = "MIT"
srcDir        = "src"


# Dependencies

# Lowest version required for `--mm:arc` flag.
requires "nim >= 1.6.2"

import std/os

task test, "Runs the test suite":
    for module in ["connstr", "private/core", "widestrx"]:
        let moduleFile = "src/odbcn" / module & ".nim"
        echo "Testing " & moduleFile & "..."
        exec "nim r --verbosity:0 " & moduleFile
