## Helpers for getting error information on ODBC handles.

import std/macros

import "."/wrapper

type
    OdbcState* = array[6, TSqlChar] # 6th char is _null-byte
    OdbcError* = object
        state*: OdbcState
        native*: TSqlInteger
        msg*: string
    OdbcErrors* = object
        retVal*: TSqlSmallInt
        procName*: string
        handleTy*: string
        errs*: seq[OdbcError]
    OdbcException* = object of CatchableError

proc toString*(x: openArray[char | byte]): string =
    result = newString(x.len)
    for i in 0..<x.len:
        result[i] = x[i].char

proc `$`*(x: OdbcState): string =
    x.toOpenArray(0, 4).toString

iterator toInf(start: int = 0): int =
    var i = start
    while true:
        yield i
        inc i

proc getDiagMsg*(handle: SqlHandle, handleKind: TSqlSmallInt): seq[OdbcError] {.raises: [].} =
    ## Get all messages (warnings and errors) associated with actions done with
    ## the given handle.
    var
        msg: array[SQL_MAX_MESSAGE_LENGTH, TSqlChar] # Is ANSI, beware
        state: OdbcState
        native: TSqlInteger
        textLen: TSqlSmallInt # Is the length of output `msg` without _null-byte
    for i in 1.toInf:
        let ret = SQLGetDiagRec(handleKind, handle, TSqlSmallInt(i), state[0].addr, native,
                                msg[0].addr, SQL_MAX_MESSAGE_LENGTH, textLen)
        if ret == SQL_NO_DATA:
            break
        if ret == SQL_INVALID_HANDLE:
            #warn "invalid handle ", cast[int](handle[])
            break
        if ret != SQL_SUCCESS:
            #error "unexpected rc for SQLGetDiagRec: ", ret
            break
        result.add OdbcError(state: state, native: native,
                             msg: msg.toOpenArray(0, textLen - 1).toString)

func procName(x: NimNode): string =
    if x.kind == nnkCall:
        if x[0].kind == nnkDotExpr:
            $x[0][^1]
        else:
            $x[0]
    elif x.kind == nnkDotExpr:
        $x[^1]
    else:
        ""

proc raiseOdbcException*(errs: OdbcErrors) =
    raise newException(OdbcException, repr errs)
proc raiseOdbcException*(errs: openArray[OdbcError]) =
    raise newException(OdbcException, "Unknown function failed " & repr errs)

macro odbcSimpleCheck*(handle: SqlHandle, kind: TSqlSmallInt,
                       stmt, actions: untyped): TSqlSmallInt =
    ## If `stmt` evaluates to false, use the return value of the ODBC call to
    ## get all errors.
    let procName = stmt.procName
    quote do:
        let rc = `stmt`
        if rc == SQL_ERROR:
            var errs {.inject.} = OdbcErrors(retVal: rc, procName: `procName`, handleTy: $`kind`)
            errs.errs = getDiagMsg(`handle`, `kind`)
            `actions`
        rc

template odbcCheck*(handle: SqlHandle, kind: TSqlSmallInt, stmt) =
    discard odbcSimpleCheck(handle, kind, stmt):
        errs.raiseOdbcException

template odbcCheckRet*(handle: SqlHandle, kind: TSqlSmallInt, stmt): TSqlSmallInt =
    odbcSimpleCheck(handle, kind, stmt):
        errs.raiseOdbcException
