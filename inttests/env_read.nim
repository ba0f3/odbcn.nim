# included in the integration tests

import
    std/[
        os,
        logging,
    ],

    odbcn/[
        connstr,
    ]

type
    ConnStrPart = enum
        dsn
        uid
        pwd

const
    envStr: array[ConnStrPart, string] = ["ODBCN_INTTEST_DSN", "ODBCN_INTTEST_UID", "ODBCN_INTTEST_PWD"]

proc myGetEnv(k: ConnStrPart): string =
    doAssert existsEnv(envStr[k])
    getEnv(envStr[k])

let
    dbServer* = myGetEnv dsn
    dbUser* = myGetEnv uid
    dbPass* = myGetEnv pwd
    connStringBase = initConnString {"DSN": dbServer, "UID": dbUser, "PWD": dbPass}
    connString* = $connStringBase

type FConsoleLogger = ref object of Logger

method log(logger: FConsoleLogger, level: Level, args: varargs[string, `$`]) =
    let ln = substituteLog(logger.fmtStr, level, args)
    stderr.writeLine(ln)
    stderr.flushFile

addHandler FConsoleLogger(fmtStr: "$time [$levelname] ")
