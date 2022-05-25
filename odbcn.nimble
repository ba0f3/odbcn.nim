# Package

version       = "0.1.2"
author        = "Håvard Mjaavatten"
description   = "Abstraction of ODBC"
license       = "MIT"
srcDir        = "src"
bin = @["odbcn/odbcn_describeqry"]
installExt = @["nim"]


# Dependencies

# Lowest version required for `--mm:arc` flag.
requires "nim >= 1.6.2"
