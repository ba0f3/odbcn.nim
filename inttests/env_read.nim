# included in the integration tests

const errorMsg {.used.} = """
Missing or malformed "env.nim" file in this directory. This file is not source
controlled, so user must create this themselves. This file defines the
following constants required for integration testing:

```
const dbServer = "" # name of test server/DSN
const dbUser = "" # name of user to connect
const dbPass = "" # password of user to connect
```"""

static:
    when not fileExists(currentSourcePath() /../ "env.nim"):
        {.error: errorMsg.}
    discard
import ./env {.all.} # Don't `include`, so that user can import std/os there
when not declared(dbServer) or not declared(dbUser) or not declared(dbPass):
    {.error: errorMsg.}
const connString = $initConnString {"DSN": dbServer, "UID": dbUser, "PWD": dbPass}

type FConsoleLogger = ref object of Logger

method log(logger: FConsoleLogger, level: Level, args: varargs[string, `$`]) =
    let ln = substituteLog(logger.fmtStr, level, args)
    stderr.writeLine(ln)
    stderr.flushFile

addHandler FConsoleLogger(fmtStr: "$time [$levelname] ")
