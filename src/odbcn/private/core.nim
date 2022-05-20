## Contains the whole ODBC abstraction. See main module `odbcn` for
## how-to.

# Ignore `CdbcStmt(x)` expressions in functions generic over statements, etc.
{.push hint[ConvFromXtoItselfNotNeeded]: off.}

import std/[
    asyncdispatch,
    enumerate,
    logging,
    macrocache,
    options,
    sequtils,
    strformat,
    strutils,
    typetraits,
]
import std/macros except error, warn
import ".."/[widestrx, wrapper]

export GUID, OdbcDate, OdbcTime, OdbcTimestamp

{.experimental: "views".}
{.experimental: "strictFuncs".}

# These must be used to describe whether a type is distinct upon SqlHEnv,
# SqlHDBC or SqlHStmt. This is necessary because all those types are
# equivalent, so `std/typetraits.distinctBase` cannot be used.
template dbcKind {.pragma.}
template stmtKind {.pragma.}
template resultKind {.pragma.}
template preparedKind {.pragma.}

template isDbcKind(x): bool = hasCustomPragma(x, dbcKind)
template isStmtKind(x): bool = hasCustomPragma(x, stmtKind)
template isResultKind(x): bool = hasCustomPragma(x, resultKind)
template isPreparedKind(x): bool = hasCustomPragma(x, preparedKind)

# Handle types. `distinct` here is only utilized to make sure the handle is
# free'd correctly through `=destroy`. A type distinct upon `OdbcStmt`, such as
# `OdbcPreparedStmt`, will trigger `OdbcStmt`'s `=destroy` when it shall be
# free'd.

type
    OdbcEnv* = distinct SqlHEnv
        ## ODBC environment handle. This is the only handle type that can have
        ## `nil` values. Usually `nil` values are used when setting
        ## process-level attributes that affect all environments.

    OdbcNoConn* {.dbcKind.} = distinct SqlHDBC
    OdbcConn* {.dbcKind.} = distinct OdbcNoConn
    #OdbcAsyncConn* = distinct OdbcNoConn

    OdbcStmt* {.stmtKind.} = distinct SqlHStmt
    OdbcPreparedStmt* {.stmtKind, preparedKind.} = distinct OdbcStmt
    #OdbcAsyncStmt* = distinct SqlHStmt

    OdbcFastResultSet* {.stmtKind, resultKind.} = distinct OdbcStmt
    OdbcPreparedResultSet* {.stmtKind, resultKind.} = distinct OdbcStmt

    OdbcAnyConn* = concept type T
        isDbcKind(T)
        distinctBase(T) is SqlHandle
    OdbcAnyStmt* = concept type T
        isStmtKind(T)
        distinctBase(T) is SqlHandle
    OdbcAnyResult* = concept type T
        isResultKind(T)
        distinctBase(T) is SqlHandle
    OdbcAnyPrepared* = concept type T
        isPreparedKind(T)
        distinctBase(T) is SqlHandle

    OdbcHandle* = OdbcEnv | OdbcAnyConn | OdbcAnyStmt
        ## Represents a handle allocated through ODBC Driver Manager. Any
        ## instance of these types except `OdbcEnv` are guaranteed non-`nil`.

proc kind(h: OdbcEnv): TSqlSmallInt = SQL_HANDLE_ENV
proc kind(h: OdbcAnyConn): TSqlSmallInt = SQL_HANDLE_DBC
proc kind(h: OdbcAnyStmt): TSqlSmallInt = SQL_HANDLE_STMT

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

proc transit[T, U](x: sink T, to: typedesc[U]): U =
    ## Change the state of an object by changing its type to a more or less
    ## distinct type. This tries to use RAII in a way that Rust does; it
    ## consumes an owned source, and returnes an owned destination.
    ##
    ## This is in general only used within the library, but may be used by
    ## users that seek to extend this type system.
    ##
    ## This is used for ODBC types (connection and statement) to convey that
    ## those types are in different states, and therefore different rules
    ## apply. This basically makes the ODBC types safer, such as knowing that a
    ## connection is either connected or not.
    #static:
    #    assert distinctBase(T) is distinctBase(U)
    U(x.move)

# {{{1 Error handling

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

proc toString(x: openArray[char | byte]): string =
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

template odbcCheckRet*(handle: SqlHandle, kind: TSqlSmallInt, stmt): TSqlSmallInt =
    odbcSimpleCheck(handle, kind, stmt):
        errs.raiseOdbcException

template odbcCheckRet*(handle, stmt): TSqlSmallInt =
    odbcCheckRet(SqlHandle(handle), kind(handle), stmt)

template odbcCheckNoRaise*(handle; stmt) =
    discard odbcSimpleCheck(SqlHandle(handle), kind(handle), stmt):
        error repr errs

proc raiseOdbcException*(errs: OdbcErrors) =
    raise newException(OdbcException, repr errs)
proc raiseOdbcException*(errs: openArray[OdbcError]) =
    raise newException(OdbcException, "Unknown function failed " & repr errs)

template odbcCheck*(handle: SqlHandle, kind: TSqlSmallInt, stmt) =
    discard odbcSimpleCheck(handle, kind, stmt):
        errs.raiseOdbcException

template odbcCheck*(handle; stmt: untyped) =
    odbcCheck(SqlHandle(handle), kind(handle), stmt)

# {{{1 RAII
# These RAII procs apply to any types `distinct` on these types as well.

proc `=copy`(dest: var OdbcEnv, source: OdbcEnv) {.error.}
proc `=copy`(dest: var OdbcNoConn, source: OdbcNoConn) {.error.}
proc `=copy`(dest: var OdbcStmt, source: OdbcStmt) {.error.}

proc `=destroy`(x: var OdbcEnv) =
    odbcCheck x, SQLFreeHandle(SQL_HANDLE_ENV, x.SqlHEnv)

proc `=destroy`(x: var OdbcNoConn) =
    if not x.SqlHDBC.isNil:
        odbcCheck x, SQLFreeHandle(SQL_HANDLE_DBC, x.SqlHDBC)

# A rollback here prevents "invalid transaction state" when disconnecting.
proc `=destroy`(x: var OdbcConn) =
    if not x.SqlHDBC.isNil:
        odbcCheck x, SQLEndTran(SQL_HANDLE_DBC, x.SqlHDBC, SQL_ROLLBACK)
        odbcCheck x, SQLDisconnect(x.SqlHDBC)
        `=destroy`(x.OdbcNoConn)

proc `=destroy`(x: var OdbcStmt) =
    if not x.SqlHStmt.isNil:
        odbcCheck x, SQLFreeHandle(SQL_HANDLE_STMT, x.SqlHStmt)

# {{{1 Extensions to special ODBC types

proc `==`*(x, y: GUID): bool =
    cmpMem(x.unsafeAddr, y.unsafeAddr, GUID.sizeof) == 0

proc `$`*(x: OdbcDate): string =
    x.year.intToStr(4) & "-" & x.month.int.intToStr(2) & "-" &
        x.day.int.intToStr(2)

proc `==`*(x, y: OdbcDate): bool =
    cmpMem(x.unsafeAddr, y.unsafeAddr, OdbcDate.sizeof) == 0

proc `$`*(x: OdbcTime): string =
    x.hour.int.intToStr(2) & ":" & x.minute.int.intToStr(2) & ":" &
        x.second.int.intToStr(2)

proc `==`*(x, y: OdbcTime): bool =
    cmpMem(x.unsafeAddr, y.unsafeAddr, OdbcTime.sizeof) == 0

proc `$`*(x: OdbcTimestamp): string =
    x.year.intToStr(4) & "-" &
        x.month.int.intToStr(2) & "-" &
        x.day.int.intToStr(2) & "T" &
        x.hour.int.intToStr(2) & ":" &
        x.minute.int.intToStr(2) & ":" &
        x.second.int.intToStr(2) & "." &
        x.fraction.int.intToStr(9)

proc `==`*(x, y: OdbcTimestamp): bool =
    cmpMem(x.unsafeAddr, y.unsafeAddr, OdbcTimestamp.sizeof) == 0

# {{{1 Odbc `Any` type
# There are 3 different type kinds used in this section. Those are
#
# * "Odbc type" - a.k.a. "SQL data type" according to Microsoft Docs. This is
#   the data type used in the DBMS. Is called "Odbc" instead of "SQL" to
#   disambiguate.
# * "Primitive type" (OdbcPrimType) - a.k.a. "C data type" according to
#   Microsoft Docs. Describes the data type used in applications. Serves as the
#   "any" type (when type information is not available at compile-time).

type
    OdbcFixedLenType* = SomeNumber | bool | enum | GUID | OdbcDate |
        OdbcTime | OdbcTimestamp

    OdbcArrayType* = byte | char | Utf16Char
        ## Valid element types for `array` and `seq`.

    OdbcPrimType* = enum
        ## All data types that this library supports to be retrieved from or
        ## sent to the ODBC C library. `OdbcValue` doesn't use all ordinal
        ## types here, but these are required to support user-defined
        ## structures.
        otByteArray
        otCharArray
        otWideArray
        otBool
        otInt8
        otInt16
        otInt32
        otInt64
        otFloat32
        otFloat64
        otDate
        otTime
        otTimestamp
        otGuid

const
    odbcIntKinds* = {otBool..otInt64}

type
    OdbcValue* = object
        case kind*: OdbcPrimType
        of otByteArray: bytes: seq[byte]
        of otCharArray: chars: seq[char]
        of otWideArray: wchars: seq[Utf16Char]
        of odbcIntKinds: i64*: int64
        of otFloat32: f32*: float32
        of otFloat64: f64*: float64
        of otDate: date*: OdbcDate
        of otTime: time*: OdbcTime
        of otTimestamp: datetime*: OdbcTimestamp
        of otGuid: guid*: GUID

    OdbcRowSet* = object
        vals: seq[OdbcValue]
        names: seq[string]

const
    # Numeric and decimal types cannot be floats because they are exact numbers.
    primTyAsOdbcTy = [
        ## Describes the "C data type" that this library represents each "SQL
        ## data type".
        otByteArray: @[
            SQL_BINARY.TSqlSmallInt, SQL_VARBINARY, SQL_LONGVARBINARY, SQL_SS_UDT,
        ],
        otCharArray: @[
            SQL_CHAR.TSqlSmallInt, SQL_VARCHAR, SQL_LONGVARCHAR, SQL_NUMERIC,
            SQL_DECIMAL,
        ],
        otWideArray: @[
            SQL_WCHAR.TSqlSmallInt, SQL_WVARCHAR, SQL_WLONGVARCHAR, SQL_SS_XML,
            SQL_SS_TIMESTAMPOFFSET,
        ],
        otBool: @[SQL_BIT.TSqlSmallInt],
        otInt8: @[SQL_TINYINT.TSqlSmallInt],
        otInt16: @[SQL_SMALLINT.TSqlSmallInt],
        otInt32: @[SQL_INTEGER.TSqlSmallInt],
        otInt64: @[SQL_BIGINT.TSqlSmallInt],
        otFloat32: @[SQL_REAL.TSqlSmallInt],
        otFloat64: @[SQL_FLOAT.TSqlSmallInt, SQL_DOUBLE],
        otDate: @[SQL_TYPE_DATE.TSqlSmallInt, SQL_DATE],
        otTime: @[SQL_TYPE_TIME.TSqlSmallInt, SQL_TIME, SQL_SS_TIME2],
        otTimestamp: @[SQL_TYPE_TIMESTAMP.TSqlSmallInt, SQL_TIMESTAMP],
        otGuid: @[SQL_GUID.TSqlSmallInt],
    ]
    primTyAsCTy = [
        otByteArray: SQL_C_BINARY.TSqlSmallInt,
        otCharArray: SQL_C_CHAR,
        otWideArray: SQL_C_WCHAR,
        otBool: SQL_C_BIT,
        otInt8: SQL_C_STINYINT,
        otInt16: SQL_C_SSHORT,
        otInt32: SQL_C_SLONG,
        otInt64: SQL_C_SBIGINT,
        otFloat32: SQL_C_FLOAT,
        otFloat64: SQL_C_DOUBLE,
        otDate: SQL_C_TYPE_DATE,
        otTime: SQL_C_TYPE_TIME,
        otTimestamp: SQL_C_TYPE_TIMESTAMP,
        otGuid: SQL_C_GUID,
    ]

# {{{2 Primitive type mappings

func elemToPrimTy(_: typedesc[byte]): OdbcPrimType = otByteArray
func elemToPrimTy(_: typedesc[char]): OdbcPrimType = otCharArray
func elemToPrimTy(_: typedesc[Utf16Char]): OdbcPrimType = otWideArray
func elemToPrimTy[T](ty: typedesc[T]): OdbcPrimType =
    static:
        macros.error ty.name & " not implemented for `elemToPrimTy`"

func intSizeToOdbcTy(sz: int): OdbcPrimType =
    case sz
    of 1: otInt8
    of 2: otInt16
    of 4: otInt32
    of 8: otInt64
    else: raise newException(OdbcException, "not implemented integer size " & $sz)

# Mapping from a type to its "SQL data type". This is used when converting custom
# objects users use for parameter.
func toPrimTy(_: typedesc[cstring]): OdbcPrimType = otCharArray
func toPrimTy(_: typedesc[string | WideCString | WideCStringObj]): OdbcPrimType = otWideArray
func toPrimTy(_: typedesc[bool]): OdbcPrimType = otBool
func toPrimTy(ty: typedesc[SomeInteger | enum]): OdbcPrimType = intSizeToOdbcTy(sizeof(ty))
func toPrimTy(_: typedesc[float32]): OdbcPrimType = otFloat32
func toPrimTy(_: typedesc[float64 | float]): OdbcPrimType = otFloat64
func toPrimTy(_: typedesc[OdbcDate]): OdbcPrimType = otDate
func toPrimTy(_: typedesc[OdbcTime]): OdbcPrimType = otTime
func toPrimTy(_: typedesc[OdbcTimestamp]): OdbcPrimType = otTimestamp
func toPrimTy(_: typedesc[GUID]): OdbcPrimType = otGuid

func toPrimTy[T](_: typedesc[seq[T]]): OdbcPrimType = elemToPrimTy(T)
func toPrimTy[I, T](_: typedesc[array[I, T]]): OdbcPrimType = elemToPrimTy(T)

func toPrimTy[T](ty: typedesc[T]): OdbcPrimType =
    static:
        macros.error ty.name & " not implemented for `toPrimTy`"

func toCTy(x: OdbcPrimType): TSqlSmallInt = primTyAsCTy[x]
func toBestOdbcTy(x: OdbcPrimType): TSqlSmallInt = primTyAsOdbcTy[x][0]
func toCTy(x: typedesc): TSqlSmallInt = x.toPrimTy.toCTy

func odbcToPrimTy(ty: TSqlSmallInt): OdbcPrimType =
    ## Conversion from "SQL type" into the most appropriate "C type" for this
    ## library.
    for i, it in primTyAsOdbcTy:
        if ty in it:
            return i
    raise newException(OdbcException, "not implemented " & $ty)

proc toTy(ty: OdbcPrimType): NimNode =
    ## Best native Nim type to represent the C type.
    case ty
    of otByteArray: seq[byte].getTypeInst
    of otCharArray, otWideArray: string.getTypeInst
    of odbcIntKinds: int64.getTypeInst
    of otFloat32: float32.getTypeInst
    of otFloat64: float64.getTypeInst
    of otDate: OdbcDate.getTypeInst
    of otTime: OdbcTime.getTypeInst
    of otTimestamp: OdbcTimestamp.getTypeInst
    of otGuid: GUID.getTypeInst

# {{{2 Conversion from OdbcValue to various primitives

func `$`*(x: OdbcValue): string =
    case x.kind
    of otByteArray: # May or may not be UTF-8. Ignoring encoding for now
        x.bytes.toOpenArray(0, x.bytes.len - 1).map(toHex).join
    of otCharArray:
        x.chars.toOpenArray(0, x.chars.len - 1).toString
    of otWideArray:
        x.wchars.utf16To8
    of odbcIntKinds: $x.i64
    of otFloat32: $x.f32
    of otFloat64: $x.f64
    of otDate: $x.date
    of otTime: $x.time
    of otTimestamp: $x.datetime
    of otGuid: $x.guid

func toBool*(x: OdbcValue): bool =
    case x.kind
    of otCharArray, otWideArray: ($x).parseBool
    of odbcIntKinds: x.i64 != 0
    of otFloat32: x.f32 != 0.0
    of otFloat64: x.f64 != 0.0
    else: raise newException(ValueError, "Cannot convert " & $x.kind & " to bool")

func toInt*(x: OdbcValue): int =
    case x.kind
    of otCharArray, otWideArray: ($x).parseInt
    of odbcIntKinds: x.i64.int
    of otFloat32: x.f32.toInt
    of otFloat64: x.f64.toInt
    else: raise newException(ValueError, "Cannot convert " & $x.kind & " to integer")

func toFloat*(x: OdbcValue): BiggestFloat =
    case x.kind
    of otCharArray, otWideArray: ($x).parseFloat
    of odbcIntKinds: x.i64.int.toFloat
    of otFloat32: x.f32
    of otFloat64: x.f64
    else: raise newException(ValueError, "Cannot convert " & $x.kind & " to float")

proc bytes*(x: OdbcValue): lent seq[byte] = x.bytes
proc chars*(x: OdbcValue): lent seq[char] = x.chars
    ## Return reference to underlying char array. The actual `seq` field in
    ## `OdbcValue` cannot be public because it would break memory safety
    ## guarantees. TODO: Describe those guarantees.
proc wchars*(x: OdbcValue): lent seq[Utf16Char] = x.wchars
    ## Return reference to underlying wide char array. The actual `seq` field
    ## in `OdbcValue` cannot be public because it would break memory safety
    ## guarantees. TODO: Describe those guarantees.

proc `[]`*(x: OdbcRowSet, idx: int): lent OdbcValue = x.vals[idx]
    ## Index for a value in a row set.
proc index*(x: OdbcRowSet, key: string): int =
    ## Find the column index of a column name `key`.
    for i, it in x.names:
        if cmpIgnoreStyle(it, key) == 0:
            return i
    -1
proc `[]`*(x: OdbcRowSet, key: string): lent OdbcValue = x.vals[x.index key]
    ## Index for a value in a row set by searching for the column's name.
proc names*(x: OdbcRowSet): lent seq[string] = x.names
    ## Get a view into the list of column names.
proc len*(x: OdbcRowSet): int = x.vals.len
    ## Get number of columns in the row set.

# {{{1 Attribute manipulation

template genAttrFuncs(baseObj, objs: untyped, sqlSetFunc, sqlGetFunc: typed) =
    proc setAttrLow(h: baseObj, attr: TSqlInteger, n: SqlPointer, strLen: TSqlInteger) {.sideEffect.} =
        odbcCheck h, sqlSetFunc(SqlHandle(h), attr, n, strLen)

    proc getAttrLow(h: baseObj, attr: TSqlInteger, ret: SqlPointer,
                    len: TSqlInteger, retLen: ptr TSqlInteger) =
        odbcCheck h, sqlGetFunc(SqlHandle(h), attr, ret, len, retLen)

    proc setAttr*(h: objs, attr: int, n: uint) =
        ## Low-level attribute setter on a handle. See Microsoft docs for
        ## `SQLSet*Attr` functions for available `attr`s and their expected
        ## type of `n`.
        setAttrLow(baseObj(h), TSqlInteger(attr),
            cast[SqlPointer](n.TSqlUInteger), SQL_IS_UINTEGER)

    proc setAttr*(h: objs, attr, n: int) =
        ## Low-level attribute setter on a handle. See Microsoft docs for
        ## `SQLSet*Attr` functions for available `attr`s and their expected
        ## type of `n`.
        setAttrLow(baseObj(h), TSqlInteger(attr),
            cast[SqlPointer](n.TSqlInteger), SQL_IS_INTEGER)

    proc setAttr*(h: objs, attr: int, n: string) =
        ## Low-level attribute setter on a handle. See Microsoft docs for
        ## `SQLSet*Attr` functions for available `attr`s and their expected
        ## type of `n`.
        setAttrLow(baseObj(h), TSqlInteger(attr),
            cast[SqlPointer](n.cstring), TSqlInteger(n.len))

    proc getStringAttr*(h: objs, attr: int): string =
        ## Low-level attribute getter on a handle. See Microsoft docs for
        ## `SQLGet*Attr` functions for available `attr`s and their expected
        ## return type.
        result = newString(256)
        var retLen: TSqlInteger
        getAttrLow(
            baseObj(h), TSqlInteger(attr), cast[SqlPointer](result[0].addr),
            TSqlInteger(result.len), retLen.addr)
        result.setLen retLen

    proc getIntAttr*(h: objs, attr: int): uint =
        ## Low-level attribute getter on a handle. See Microsoft docs for
        ## `SQLGet*Attr` functions for available `attr`s and their expected
        ## return type.
        var ret: TSqlULen
        getAttrLow(baseObj(h), TSqlInteger(attr),
            cast[SqlPointer](ret.addr), 0, nil)
        ret

genAttrFuncs(OdbcEnv, OdbcEnv, SQLSetEnvAttr, SQLGetEnvAttr)
genAttrFuncs(OdbcNoConn, OdbcAnyConn, SQLSetConnectAttr, SQLGetConnectAttr)
genAttrFuncs(OdbcStmt, OdbcAnyStmt, SQLSetStmtAttr, SQLGetStmtAttr)

proc getStringInfo*(conn: OdbcConn, attr: uint): string =
    ## Low-level connection information getter. See Microsoft docs for
    ## `SQLGetInfo` function for available `attr`s and their expected return
    ## type.
    result = newString(256)
    var retLen: TSqlSmallInt
    odbcCheck conn, SqlHandle(conn).SQLGetInfo(
        TSqlUSmallInt(attr), cast[SqlPointer](result[0].unsafeAddr),
        TSqlSmallInt(result.len), retLen.addr)
    result.setLen retLen

proc getInt16Info*(conn: OdbcConn, attr: uint): uint16 =
    ## Low-level connection information getter. See Microsoft docs for
    ## `SQLGetInfo` function for available `attr`s and their expected return
    ## type.
    var ret: TSqlUSmallInt
    odbcCheck conn, SqlHandle(conn).SQLGetInfo(
        TSqlUSmallInt(attr), cast[SqlPointer](ret.addr), 0, nil)
    ret

proc getInt32Info*(conn: OdbcConn, attr: uint): uint32 =
    ## Low-level connection information getter. See Microsoft docs for
    ## `SQLGetInfo` function for available `attr`s and their expected return
    ## type.
    var ret: TSqlUInteger
    odbcCheck conn, SqlHandle(conn).SQLGetInfo(
        TSqlUSmallInt(attr), cast[SqlPointer](ret.addr), 0, nil)
    ret

# {{{1 Environment and initialization

type
    OdbcConnectionPooling* = enum
        ## Valid connection pooling methods.
        ocpOff
        ocpOnePerDriver ## All environments share a connection pool
        ocpOnePerEnv ## Each environment has their own connection pool
    OdbcVer* = enum ver2_00, ver3_00, ver3_80
        ## Valid ODBC versions. This is used by the Driver Manager to handle
        ## possible incompatibilities between application and driver. Always
        ## use latest version `ver3_80` for new applications.

const
    ocpConv = [
        ocpOff: SQL_CP_OFF,
        ocpOnePerDriver: SQL_CP_ONE_PER_DRIVER,
        ocpOnePerEnv: SQL_CP_ONE_PER_HENV,
    ]
    odbcVerConv = [
        ver2_00: SQL_OV_ODBC2,
        ver3_00: SQL_OV_ODBC3,
        ver3_80: SQL_OV_ODBC3_80,
    ]

proc setConnectionPooling*(env: OdbcEnv, value: OdbcConnectionPooling) =
    ## Affects the connection pooling attribute of `env`. `env` may be `nil`,
    ## in which case this setting affects all subsequently allocated
    ## environments for the computer process.
    env.setAttr(SQL_ATTR_CONNECTION_POOLING, ocpConv[value].int)

proc connectionPooling*(env: OdbcEnv): OdbcConnectionPooling =
    ## Get the current connection pooling attribute of `env`.
    let val = env.getIntAttr SQL_ATTR_CONNECTION_POOLING
    for i, other in ocpConv:
        if val == other:
            return i

proc newOdbcEnv*(odbcVer: OdbcVer = ver3_80): OdbcEnv =
    ## Initialize a new ODBC environment. This is required to create
    ## connections if `globalOdbcEnv` is not used.
    # NOTE: SQLAllocHandle for environment handles always returns either
    # SQL_SUCCESS or SQL_ERROR (when OOM). Raising exception here is reasonable
    # as OOM should be fatal anyway.
    var ret: SqlHEnv
    let rc = SQLAllocHandle(SQL_HANDLE_ENV, nil, ret)
    if rc == SQL_ERROR:
        raise newException(OdbcException,
            "Ran out of memory on SQLAllocHandle(SQL_HANDLE_ENV, ...) call")
    assert rc == SQL_SUCCESS
    assert ret != nil
    result = ret.OdbcEnv
    result.setAttr(SQL_ATTR_ODBC_VERSION, odbcVerConv[odbcVer])

var globalOdbcEnv*: OdbcEnv
    ## ODBC environment used as default environment to ODBC functions.
    ## It is initialized at application initialization with reasonable
    ## configuration, but this initialization can be prevented with
    ## `odbcNoEnvInit` compiler define.
    ##
    ## It is recommended to use a global environment, because all functions
    ## except allocation/freeing of the actual environment is thread-safe.
    ## Manual initialization is preferred if exceptions during initialization
    ## is problematic.

when not defined odbcNoEnvInit:
    globalOdbcEnv = newOdbcEnv()
    when defined odbcEnvConnectionPooling:
        globalOdbcEnv.setConnectionPooling(ocpOnePerEnv)

# {{{1 Connection
# {{{2 Transactions

proc inTran*(conn: OdbcConn): bool =
    ## `true` if connection is currently in a transaction.
    conn.getIntAttr(SQL_ATTR_AUTOCOMMIT) == SQL_AUTOCOMMIT_OFF

proc beginTran*(conn: OdbcConn) =
    ## Start a transaction. Prefer use of `withTran <#withTran.t,OdbcConn,>`_
    ## over this. Commit with `commitTran <#commitTran,OdbcConn>`_ or rollback
    ## with `rollbackTran <#rollbackTran,OdbcConn>`_.
    assert not conn.inTran
    conn.setAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF)

proc commitTran*(conn: OdbcConn) =
    ## Commit a transaction started with `beginTran <#beginTran,OdbcConn>`_.
    assert conn.inTran
    odbcCheck conn, SQLEndTran(SQL_HANDLE_DBC, SqlHandle(conn), SQL_COMMIT)
    conn.setAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON)

proc rollbackTran*(conn: OdbcConn) =
    ## Rollback a transaction started with `beginTran <#beginTran,OdbcConn>`_.
    assert conn.inTran
    odbcCheck conn, SQLEndTran(SQL_HANDLE_DBC, SqlHandle(conn), SQL_ROLLBACK)
    conn.setAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON)

template withTran*(conn: OdbcConn, code) =
    ## Starts a transaction, executes `code`, then commits the transaction if
    ## no exception occurred, otherwise rollbacks the transaction.
    ##
    ## Prefer use of this over `beginTran <#beginTran,OdbcConn>`_, etc.
    ##
    ## ```nim
    ## conn.withTran:
    ##   assert conn.inTran
    ##   discard conn.exec("update MyTable set MyCol = ?", 3)
    ##   discard conn.exec("update MyTable2 set MyCol = ?", 3)
    ## ```
    conn.beginTran
    try:
        code
        conn.commitTran
    except:
        conn.rollbackTran
        raise

# {{{2 Connection creation

proc newOdbcNoConn*(env = globalOdbcEnv): OdbcNoConn =
    ## Create a new disconnected connection handle. This is used to customize
    ## the handle before connecting.
    var conn: SqlHDBC
    odbcCheck env, SQLAllocHandle(SQL_HANDLE_DBC, SqlHandle(env), conn)
    assert not conn.isNil
    conn.OdbcNoConn

proc connect*(disConn: sink OdbcNoConn, dsn, user, pass: string): OdbcConn =
    ## Connect with a DSN (data source name), user and password.
    odbcCheck disConn, SqlHandle(disConn).SQLConnect(
        dsn.cstring, dsn.len.TSqlSmallInt, user.cstring,
        user.len.TSqlSmallInt, pass.cstring, pass.len.TSqlSmallInt)
    disConn.transit(OdbcConn)

proc connect*(disConn: sink OdbcNoConn, connString: string): OdbcConn =
    ## Connect with a connection string. Syntax is key-value pairs with '='
    ## separator, and each pair delimited by ';'. Available keys are the
    ## attributes documented by Microsoft on `SQLDriverConnect`, and attributes
    ## that the driver supports.
    if connString == "":
        raise newException(OdbcException, "`connString` was empty")
    odbcCheck disConn, SqlHandle(disConn).SQLDriverConnect(
        nil, connString.cstring, connString.len.TSqlSmallInt, nil, 0,
        nil, SQL_DRIVER_NOPROMPT)
    disConn.transit OdbcConn

proc newOdbcConn*(connString: string, env = globalOdbcEnv): OdbcConn =
    ## Connect with a connection string. Syntax is key-value pairs with '='
    ## separator, and each pair delimited by ';'. Available keys are the
    ## attributes documented by Microsoft on `SQLDriverConnect`, and attributes
    ## that the driver supports.
    newOdbcNoConn().connect(connString)

proc newOdbcConn*(dsn, user, pass: string, env = globalOdbcEnv): OdbcConn =
    ## Connect with a DSN (data source name), user and password.
    newOdbcNoConn().connect(dsn, user, pass)

proc disconnect*(conn: sink OdbcConn): OdbcNoConn =
    conn.rollbackTran
    odbcCheck conn, SQLDisconnect(SqlHandle(conn))
    conn.transit OdbcNoConn

# {{{2 Attributes

proc setLoginTimeout*(conn: OdbcNoConn, n: Natural) =
    ## Timeout for establishing initial connection (i.e. OdbcNoConn.connect)
    conn.setAttr(SQL_ATTR_LOGIN_TIMEOUT, n)

proc setConnTimeout*(conn: OdbcAnyConn, n: Natural) =
    ## Timeout for any request.
    conn.setAttr(SQL_ATTR_CONNECTION_TIMEOUT, n)

proc setCatalog*(conn: OdbcAnyConn, cat: string) =
    ## Set the catalog used for the connection. In SQL Server this is a `USE
    ## <cat>` statement.
    ##
    ## This is the recommended way of changing database context when using
    ## connection pooling. Using `USE <cat>` statements directly will make
    ## the next owner of the connection inherit a different database than
    ## requested.
    ##
    ## .. note:: Not all drivers support this function after a connection has
    ##   been established.
    conn.setAttr(SQL_ATTR_CURRENT_CATALOG, cat)

proc catalog*(conn: OdbcAnyConn): string =
    ## Get the catalog used for the connection. In SQL Server this is the
    ## database active.
    conn.getStringAttr SQL_ATTR_CURRENT_CATALOG

proc odbcVer*(conn: OdbcAnyConn): string =
    # NOTE: The only `SQLGetInfo` attribute where unconnected handle is valid.
    conn.OdbcConn.getStringInfo SQL_DRIVER_NAME

proc driverName*(conn: OdbcConn): string =
    conn.getStringInfo SQL_DRIVER_NAME

proc driverVersion*(conn: OdbcConn): string =
    conn.getStringInfo SQL_DRIVER_VER

proc driverOdbcVersion*(conn: OdbcConn): string =
    conn.getStringInfo SQL_DRIVER_ODBC_VER

proc dbmsName*(conn: OdbcConn): string =
    conn.getStringInfo SQL_DBMS_NAME

proc dbmsVersion*(conn: OdbcConn): string =
    conn.getStringInfo SQL_DBMS_VER

proc serverName*(conn: OdbcConn): string =
    conn.getStringInfo SQL_SERVER_NAME

proc userName*(conn: OdbcConn): string =
    conn.getStringInfo SQL_USER_NAME

# {{{2 Trace
# These functions enable tracing for all connections. Specifying a connection
# makes no difference.

{.push sideEffect.}

proc startOdbcTrace* = nil.OdbcNoConn.setAttr(SQL_ATTR_TRACE, SQL_OPT_TRACE_ON)
    ## Start trace of ODBC C functions being called from this application. The
    ## Driver Manager writes the trace to a file, which is the filename last
    ## specified in a `setOdbcTraceFile` call. If that has never happened, the
    ## default as specified in "system information" is used, otherwise
    ## "ODBC.LOG" in the root directory.
    ##
    ## .. warning:: Passwords in connection strings used to connect to a server
    ##   will be written to the trace file in plain text.
proc stopOdbcTrace* = nil.OdbcNoConn.setAttr(SQL_ATTR_TRACE, SQL_OPT_TRACE_OFF)
    ## Stop trace of ODBC C functions calls by the Driver Manager.
proc setOdbcTraceFile*(f: string) = nil.OdbcNoConn.setAttr(SQL_ATTR_TRACEFILE, f)
    ## Set the filename that the Driver Manager will write ODBC C function
    ## traces to after `startOdbcTrace` is called.

{.pop.}

proc startedOdbcTrace*: bool = nil.OdbcNoConn.getIntAttr(SQL_ATTR_TRACE) == SQL_OPT_TRACE_ON
    ## `true` if trace of ODBC C function calls from the Driver Manager is
    ## active.

# {{{1 Statement

proc newStmt*(conn: OdbcConn): OdbcStmt =
    var stmt: SqlHStmt
    odbcCheck conn, SQLAllocHandle(SQL_HANDLE_STMT, SqlHandle(conn), stmt)
    assert not stmt.isNil
    stmt.OdbcStmt

proc `maxRows=`*(stmt: OdbcAnyStmt, maxRows: Natural) =
    ## Set the maximum number of rows to return in a result set. This must be
    ## set before statement execution.
    ##
    ## .. note:: Not always supported by drivers. Known not to work on FreeTDS.
    ##   In this case use `top X` in query text instead.
    stmt.setAttr(SQL_ATTR_MAX_ROWS, maxRows)

proc maxLen*(stmt: OdbcAnyStmt): Natural =
    ## Maximum size of data returned from variable size data (like varchar) in
    ## any given SQLGetData (and maybe others too). This affects retrieving row
    ## sets.
    stmt.getIntAttr(SQL_ATTR_MAX_LENGTH)

proc `maxLen=`*(stmt: OdbcAnyStmt, n: Natural) =
    ## Maximum size of data returned from variable size data (like varchar) in
    ## any given SQLGetData (and maybe others too). This affects retrieving row
    ## sets.
    stmt.setAttr(SQL_ATTR_MAX_LENGTH, n)

proc `timeout=`*(stmt: OdbcAnyStmt, n: Natural) =
    ## Set the time before a query being executed is timed out. This can be set
    ## for any state, but only makes sense before execution.
    stmt.setAttr(SQL_ATTR_QUERY_TIMEOUT, n)

proc timeout*(stmt: OdbcAnyStmt): Natural =
    ## Time before a query being executes is timed out.
    stmt.getIntAttr(SQL_ATTR_QUERY_TIMEOUT)

# {{{1 Unbind logic
# Common unbinding method for converting result sets back into
# prepared/unprepared state. This is separated here because this is used often.

proc closeCursor(stmt: OdbcStmt) =
    odbcCheck(stmt, SQLFreeStmt(SqlHandle(stmt), SQL_CLOSE))
proc unbindCols(stmt: OdbcStmt) =
    odbcCheck(stmt, SQLFreeStmt(SqlHandle(stmt), SQL_UNBIND))
proc unbindParams(stmt: OdbcStmt) =
    odbcCheck(stmt, SQLFreeStmt(SqlHandle(stmt), SQL_RESET_PARAMS))

proc unbindAll(stmt: OdbcAnyStmt) =
    OdbcStmt(stmt).closeCursor
    OdbcStmt(stmt).unbindCols
    OdbcStmt(stmt).unbindParams

# {{{1 Binding parameters/columns and getting data
# NOTE: In general most routines take `OdbcStmt`, but only a wrapper that
# contrains that type into OdbcAnyResult or something is exported. This is
# intended to reduce duplicate code generation in compilation.

template odbcCol*(i: TSqlUSmallInt) {.pragma.}
    ## Annotate this on object fields. See `bindCols` for usage.

# {{{2 SQLGetData

{.push sideEffect, tags: [IOEffect].}

proc getData[I; T: OdbcArrayType](ds: OdbcStmt, colIdx: TSqlUSmallInt, ret: var array[I, T]) =
    const
        cTy = toCTy(ret.type)
        chunkSize = sizeOf(ret)
    var
        ind: TSqlLen
    odbcCheck(ds, SQLGetData(
        SqlHandle(ds), colIdx, cTy, ret[0].addr, chunkSize, ind.addr))
    assert ind != SQL_NO_TOTAL
    case ind
    of SQL_NULL_DATA: ret[0] = T(0)
    else: discard

proc getData(ds: OdbcStmt, colIdx: TSqlUSmallInt, ret: var seq[OdbcArrayType]) =
    const
        chunkSize = 1024
        cTy = toCTy(ret.type)
        isWide = cTy == SQL_C_WCHAR
    when isWide:
        ret.setLen chunkSize div 2
    else:
        ret.setLen chunkSize
    var
        ind: TSqlLen
        curOffset = 0
        bufLen = chunkSize
        bufPtr = cast[pointer](ret[0].addr)
    while true:
        assert chunkSize == bufLen - curOffset
        let rc = odbcCheckRet(ds, SQLGetData(
            SqlHandle(ds), colIdx, cTy, bufPtr, chunkSize, ind.addr))
        if rc != SQL_SUCCESS_WITH_INFO:
            ind += curOffset
            break
        when isWide:
            bufLen -= 2
        else:
            if ret[^1].char == '\0': bufLen -= 1
        curOffset = bufLen
        bufLen += chunkSize
        when isWide: ret.setLen bufLen div 2
        else: ret.setLen bufLen
        bufPtr = cast[pointer](cast[int](ret[0].addr) + curOffset)
    assert ind != SQL_NO_TOTAL
    case ind
    of SQL_NULL_DATA: ret.setLen 0
    else:
        when isWide:
            assert ind mod 2 == 0
            ret.setLen ind div 2
        else:
            ret.setLen ind

proc getData(ds: OdbcStmt, colIdx: TSqlUSmallInt, ret: var string) =
    var wideVal = newSeqOfCap[Utf16Char](ret.len * 2)
    getData(ds, colIdx, wideVal)
    ret = utf16To8(wideVal)

proc getData(ds: OdbcStmt, colIdx: TSqlUSmallInt, ret: var OdbcFixedLenType) =
    const cTy = toCTy(ret.type)
    var ind: TSqlLen
    odbcCheck ds, SQLGetData(
        SqlHandle(ds), colIdx, cTy, ret.addr, 0, ind.addr)
    if ind == SQL_NULL_DATA: ret.reset

proc getData[T](ds: OdbcStmt, colIdx: TSqlUSmallInt, ret: var T) =
    static:
        macros.error "Type `" & $ret.type  & "` is not a supported type " &
            "for getting data into."

{.pop.}

proc getData[T](ds: OdbcAnyResult, colIdx: TSqlUSmallInt, ret: var T) =
    mixin getData
    getData(OdbcStmt(ds), colIdx, ret)

proc getDatas*[T: OdbcFixedLenType or (not object or tuple)](ds: OdbcAnyResult, ret: var T, _: static openArray[string] = []) =
    getData(ds, 1, ret)

macro getDatas*[T: not OdbcFixedLenType and (object or tuple)](
    ds: OdbcAnyResult,
    ret: var T,
    order: static openArray[string],
) =
    result = newStmtList()
    for i, nextCol in order:
        result.add quote do:
            for key, val in fieldPairs(`ret`):
                when cmpIgnoreStyle(`nextCol`, key) == 0:
                    getData(`ds`, TSqlUSmallInt(`i` + 1), val)

proc getDatas*[T: not OdbcFixedLenType and (object or tuple)](ds: OdbcAnyResult, ret: var T) =
    ## Get all data from the current row in the result set `ds`, and put it
    ## into fields of `ret`. It is an ODBC error if this is called twice since
    ## the previous `next <#next>`_ call. This also fails if `next <#next,T>`_
    ## has been called instead of `next <#next>`_.
    for i, key, val in enumerate fieldPairs(ret):
        let idx =
            when hasCustomPragma(val, odbcCol):
                TSqlUSmallInt(getCustomPragmaVal(val, odbcCol))
            else:
                TSqlUSmallInt(i + 1)
        getData(ds, idx, val)

# {{{2 SQLBindParameter
# NOTE: Ignoring `StrLen_or_IndPtr` for binding columns. It is usually required
# for checking if a value is `NULL` after a `SQLFetch`. Reasons not to check
# this:
# * Given a normalized database environment, no columns may contain `NULL`
#   values to begin with
# * It is impractical for `bindCols`, which is intended not to store any state
#   other than the data pointers themselves
# This could be implemented for `Option[T]` types.

proc bindParamPtr(stmt: OdbcStmt, idx: TSqlUSmallInt, cTy: TSqlSmallInt,
                  odbcTy: TSqlSmallInt, param: pointer, paramLen: int) =
    odbcCheck(stmt, SQLBindParameter(
        SqlHandle(stmt), idx, SQL_PARAM_INPUT, cTy, odbcTy, TSqlULen(paramLen),
        0, param, TSqlLen(paramLen), nil))

proc bindParam(stmt: OdbcStmt, idx: TSqlUSmallInt,
               param: ptr (WideCString | WideCStringObj | seq[OdbcArrayType])) =
    const
        primTy = toPrimTy(param[].type)
        cTy = toCTy(primTy)
        odbcTy = toBestOdbcTy(primTy)
    bindParamPtr(stmt, idx, cTy, odbcTy, param[][0].addr, param[].len)

proc bindParam[I](stmt: OdbcStmt, idx: TSqlUSmallInt, param: ptr array[I, OdbcArrayType]) =
    const
        primTy = toPrimTy(param[].type)
        cTy = toCTy(primTy)
        odbcTy = toBestOdbcTy(primTy)
    bindParamPtr(stmt, idx, cTy, odbcTy, param[][0].addr, param[].len)

proc bindParam(stmt: OdbcStmt, idx: TSqlUSmallInt, param: ptr SomeFloat) =
    const
        primTy = toPrimTy(param[].type)
        cTy = toCTy(primTy)
        odbcTy = toBestOdbcTy(primTy)
    odbcCheck(stmt, SQLBindParameter(
        SqlHandle(stmt), idx, SQL_PARAM_INPUT, cTy, odbcTy, 27, 7, param, 0, nil))

proc bindParam(stmt: OdbcStmt, idx: TSqlUSmallInt,
               param: ptr (OdbcFixedLenType and not SomeFloat)) =
    const
        primTy = toPrimTy(param[].type)
        cTy = toCTy(primTy)
        odbcTy = toBestOdbcTy(primTy)
    odbcCheck(stmt, SQLBindParameter(
        SqlHandle(stmt), idx, SQL_PARAM_INPUT, cTy, odbcTy, 0, 0, param, 0, nil))

proc bindParam[T](stmt: OdbcStmt, idx: TSqlUSmallInt, param: ptr T) =
    static:
        macros.error "Type `" & $param.type  & "` is not a supported type " &
            "for binding parameters."

template initParamVal(s: string): seq[Utf16Char] =
    utf8To16(s)

template initParamVal[T](s: T): T =
    s

# NOTE: Memory unsafe. Make sure `params` doesn't leave the stack until the
# statement has been executed.
# NOTE: Size of a parameter is capped at 4000 UTF-16 characters or 8000 bytes.
# All strings are converted to UTF-16, so 4000 characters most likely apply.
# Exceeding this truncates the parameter to 4000/8000.
# TODO: Add warning when a parameter's size exceeds the cap.
macro bindParamsSingle(stmt: OdbcStmt, params: varargs[untyped]) =
    ## The simplest form of parameterized query. Bind parameters for queries
    ## that don't have a constant string query. Order of parameters is the same
    ## as the order of `?` in the query.
    result = newStmtList()
    for i, param in params:
        let idx = TSqlUSmallInt(i + 1)
        result.add quote do:
            when `param` is static:
                const param1 = initParamVal(`param`)
                var param = param1
            else:
                var param = initParamVal(`param`)
            bindParam(`stmt`, `idx`, addr(param))

# Reason for not implementing this: it would have `O(n*k)` if-statements (code
# complexity) at run-time, where `n` is the number of query parameters, and `k`
# is the number of elements in `params`. It's much simpler and efficient to use
# indexed `?` for this instead.
template bindParamsKeyVal(
    stmt: OdbcStmt,
    order: openArray[string],
    params: varargs[untyped],
) =
    static:
        macros.error "Cannot use named parameters for non-constant query. " &
            "Use single parameters (just `?`) instead."

macro bindParamsKeyVal(
    stmt: OdbcStmt,
    order: static openArray[string],
    params: varargs[untyped],
) =
    result = newStmtList()
    for i, nextParam in order:
        block paramLoop:
            for param in params:
                let key = param[0].strVal
                if cmpIgnoreStyle(nextParam, key) != 0:
                    continue
                let
                    val = param[1]
                    idx = TSqlUSmallInt(i + 1)
                result.add quote do:
                    var param = initParamVal(`val`)
                    bindParam(`stmt`, `idx`, addr(param))
                break paramLoop
            macros.error "Parameter named `" & nextParam &
                "` in SQL query is missing from the key-value list."

macro bindParams*(
    stmt: OdbcAnyStmt,
    order: openArray[string],
    params: varargs[untyped],
) =
    ## Bind parameters to a query. Optimally call this after the statement is
    ## prepared, or executed if not prepared. `order` may be empty, in which
    ## case `params` is a list of values. If `order` is non-empty, its length
    ## must be the same as the number of parameters in the query. In this case
    ## `params` may be key-value pairs. If the length of `order` is incorrect,
    ## then the ODBC C library will raise an error.
    if params.len == 0:
        return
    if params[0].kind == nnkExprEqExpr:
        quote do:
            {.hint[ConvFromXtoItselfNotNeeded]: off.}:
                bindParamsKeyVal(OdbcStmt(`stmt`), `order`, `params`)
    else:
        quote do:
            {.hint[ConvFromXtoItselfNotNeeded]: off.}:
                bindParamsSingle(OdbcStmt(`stmt`), `params`)

template bindParams*[T: object or tuple](stmt: OdbcAnyStmt, params: T) =
    for i, _, val in enumerate fieldPairs(params):
        let idx = TSqlUSmallInt(i + 1)
        bindParam(OdbcStmt(stmt), idx, unsafeAddr(val))

iterator objectFields(ty: NimNode): string =
    ty.expectKind nnkObjectTy
    let recList = ty.findChild it.kind == nnkRecList
    for identDefs in recList:
        yield identDefs[0].strVal

macro bindParams*[T: object or tuple](stmt: OdbcAnyStmt, params: T, order: static openArray[string]) =
    ## Bind parameters similarly to `bindParams`, except bind from an object
    ## instance instead of a varargs. `order`'s length must be the same as the
    ## number of parameters in the query. If the length of `order` is
    ## incorrect, then the ODBC C library will raise an error at runtime.
    ##
    ## This is ideal for repeated executions of a prepared "INSERT" query.
    result = newStmtList()
    let keys = params.getTypeImpl.objectFields.toSeq
    # TODO: Drop this?
    let bindParam = bindSym("bindParam", brOpen)
    for i, nextParam in order:
        let idx = TSqlUSmallInt(i + 1)
        if allIt(keys, cmpIgnoreStyle(nextParam, it) != 0):
            macros.error "Parameter named `" & nextParam &
                "` in SQL query is missing from the fields in the parameter object."
        result.add quote do:
            for key, val in fieldPairs `params`:
                when cmpIgnoreStyle(`nextParam`, key) == 0:
                    {.hint[ConvFromXtoItselfNotNeeded]: off.}:
                        `bindParam`(OdbcStmt(`stmt`), `idx`, unsafeAddr(val))

# {{{2 SQLBindCol

# Wrapper to reduce generic code generation.
proc bindColPointer(stmt: OdbcStmt, colIdx: TSqlUSmallInt, cTy: TSqlSmallInt,
                    ret: pointer, size: int) =
    odbcCheck stmt, SqlHandle(stmt).SQLBindCol(colIdx, cTy, ret, size, nil)

proc bindCol[I](stmt: OdbcStmt, colIdx: TSqlUSmallInt, ret: var array[I, byte | char]) =
    bindColPointer(stmt, colIdx, SQL_C_CHAR, ret[0].addr, sizeof(ret))

proc bindCol[I](stmt: OdbcStmt, colIdx: TSqlUSmallInt, ret: var array[I, Utf16Char]) =
    bindColPointer(stmt, colIdx, SQL_C_WCHAR, ret[0].addr, sizeof(ret))

proc bindCol(stmt: OdbcStmt, colIdx: TSqlUSmallInt, ret: var OdbcFixedLenType) =
    odbcCheck stmt, SqlHandle(stmt).SQLBindCol(colIdx, ret.type.toCTy, ret.addr, 0, nil)

proc bindCol[T](stmt: OdbcStmt, colIdx: TSqlUSmallInt, ret: var T) =
    static:
        macros.error "Type `" & $ret.type  & "` is not a supported type " &
            "for binding columns."

proc bindCols*[T](stmt: OdbcAnyStmt, ret: var T) =
    ## Bind all columns of the result set (or result set to be) `stmt` to
    ## fields in `ret`. Binding lasts until the result set is `unbind`ed. This
    ## is different from `ds.next(row)`, where the binding lasts only for the
    ## `next` function call itself. This can increase performance when
    ## iterating over a result set larger than a few rows.
    ##
    ## .. warning:: Avoid using this in more complex situations than on
    ##   stack-objects in the current scope or objects that don't change memory
    ##   addresses. This can easily break memory-safety as it makes ODBC store
    ##   raw pointers to program-owned memory.
    ##
    ## This can be used with `ret` being an object on the stack/heap, like e.g.
    ##
    ## ```nim
    ## type ATabObj = object
    ##   AnInt: int
    ##   AStr: array[256, char] # string SQL types are 0-terminated
    ## let ds = conn.exec"select AnInt, AStr from ATab"
    ## var row: ATabObj
    ## ds.bindCols row
    ## while ds.next:
    ##   # `row` is filled here, and updated for each iteration
    ## ```
    ##
    ## Note that it is an error to call `ds.next(rowSet)` where `rowSet` is
    ## `OdbcRowSet` after having called `bindCols` on `ds`. ODBC will raise an
    ## error because it is generally an error to mix permanent binding and
    ## per-row binding.
    for i, key, val in enumerate fieldPairs ret:
        let idx =
            when hasCustomPragma(val, odbcCol):
                TSqlUSmallInt(getCustomPragmaVal(val, odbcCol))
            else:
                TSqlUSmallInt(i + 1)
        bindCol(OdbcStmt(stmt), idx, val)

# {{{1 FFI wrappers annotated with IO effects
# FFI functions are determined to have IO if they are listed in the async-able
# functions table in:
# https://docs.microsoft.com/en-us/sql/odbc/reference/develop-app/asynchronous-execution-polling-method
# NOTE: Wrappers must do `transit`s to represent the state transition.

# Side-effect as this may be an UPDATE/INSERT, which modifies database state.
proc execDirectInternal(stmt: OdbcStmt, qry: openArray[Utf16Char]) {.tags: [IOEffect], sideEffect.} =
    odbcCheck stmt, SQLExecDirectW(SqlHandle(stmt), qry[0].unsafeAddr, TSqlInteger(qry.len))

# Same as above
proc execInternal(stmt: OdbcStmt) {.tags: [IOEffect], sideEffect.} =
    odbcCheck(stmt, SQLExecute(SqlHandle(stmt)))

proc prepareInternal(stmt: OdbcStmt, qry: openArray[Utf16Char]) {.tags: [ReadIOEffect].} =
    odbcCheck(stmt, SqlHandle(stmt).SQLPrepareW(qry[0].unsafeAddr, qry.len.TSqlInteger))

# {{{1 Simple query

# Doesn't make much sense to this since query text must be inserted, and
proc unbind*(ds: sink OdbcFastResultSet): OdbcStmt =
    ds.unbindAll
    ds.transit OdbcStmt

proc affectedRows*(ds: OdbcAnyResult): int =
    ## Return number of rows affected by an "INSERT", "UPDATE" or "DELETE"
    ## statement.
    ##
    ## This may return number of rows in result set returned from a "SELECT"
    ## operation, but this support depends on the driver.
    odbcCheck ds, SqlHandle(ds).SQLRowCount(result)

proc findEndOfParam(x: string): int =
    for i, c in x:
        if c notin {'a'..'z', 'A'..'Z', '0'..'9'}:
            return i
    -1

# Strip away parameter name at the beginning of the string.
func parsePart(part: string): (string, string) =
    if part == "" or part[0] notin {'a'..'z', 'A'..'Z'}:
        return ("", part)
    let paramEndIdx = part.findEndOfParam
    assert paramEndIdx != -1
    (part[0..<paramEndIdx], part[paramEndIdx..<part.len])

func parseQuery(qry: string): tuple[normString: string, params: seq[string]] =
    let parts = qry.split('?')
    result.normString = parts[0]
    for part in parts[1..<parts.len]:
        let (paramPart, stringPart) = part.parsePart
        result.normString &= "?" & stringPart
        result.params.add paramPart

template exec*(stmt: sink OdbcStmt, qry: string,
               params: varargs[untyped]): OdbcFastResultSet =
    ## Fastest way of executing a query.
    let
        (normQueryUtf8, paramNames) = parseQuery(qry)
        normQueryUtf16 = utf8To16(normQueryUtf8)
    bindParams(stmt, paramNames, params)
    execDirectInternal(stmt, normQueryUtf16)
    transit(stmt, OdbcFastResultSet)

template exec*(stmt: sink OdbcStmt, qry: static string,
               params: varargs[untyped]): OdbcFastResultSet =
    const
        (normQueryUtf8, paramNames) = parseQuery(qry)
        normQueryUtf16 = utf8To16(normQueryUtf8)
    bindParams(stmt, paramNames, params)
    execDirectInternal(stmt, normQueryUtf16)
    transit(stmt, OdbcFastResultSet)

template exec*(conn: OdbcConn, qry: string,
               params: varargs[untyped]): OdbcFastResultSet =
    ## Execute a simple query without parameters. This uses `SQLExecDirect`
    ## which is the fastest way of executing the simplest queries.
    var stmt = conn.newStmt
    stmt.exec(qry, params)

proc nextResultSet*(ds: OdbcAnyResult): bool {.tags: [IOEffect].} =
    ## Attempts to move cursor to the next result set. Returns `false` if no
    ## other result set exist. Returns `true` if a result set is found and
    ## cursor is successfully moved to that result set.
    ##
    ## It's important to reset the rowset object with `initRowSet`, or create a
    ## new rowset when fetching from this new result set.
    odbcCheckRet(ds, SqlHandle(ds).SQLMoreResults) != SQL_NO_DATA

proc discardResults*(ds: OdbcAnyResult) =
    ## Discards extra result sets associated with the batch. This is required
    ## for certain queries to fully execute, such as `create backup`.
    while ds.nextResultSet:
        discard

proc initRowSet(ds: OdbcStmt, ret: var OdbcRowSet) =
    var cols: TSqlSmallInt
    odbcCheck ds, SqlHandle(ds).SQLNumResultCols(cols)
    ret.vals.setLen cols
    ret.names.setLen cols
    var
        colName: array[256, TSqlChar]
        colNameLen, odbcTy, decimals, null: TSqlSmallInt
        colSize: TSqlULen
    for i in 1..cols:
        odbcCheck ds, SqlHandle(ds).SQLDescribeCol(
            TSqlUSmallInt(i), colName[0].addr, TSqlSmallInt(colName.len), colNameLen,
            odbcTy, colSize, decimals, null)
        ret.names[i-1] = colName.toOpenArray(0, colNameLen - 1).toString
        let kind = odbcTy.odbcToPrimTy
        ret.vals[i-1] = OdbcValue(kind: kind)

proc initRowSet*(ds: OdbcAnyResult or OdbcAnyPrepared, ret: var OdbcRowSet) =
    ## Initalize a row set for use when iterating over each row in the dataset.
    ## This is always run by `next` if it is not already initialized, so the
    ## user does not need to care about this proc. This is however useful for
    ## checking the column names and column types before fetching or supplying
    ## parameter values in prepared statements, which is an expensive action.
    initRowSet(OdbcStmt(ds), ret)

# This is only valid in a read-only result set, so there's no side effects.
# Only has a side-effect if columns are bound with SQLBindCol.
proc next*(ds: OdbcAnyResult): bool {.tags: [ReadIOEffect].} =
    ## Advance the result set cursor one row.
    odbcCheckRet(ds, SqlHandle(ds).SQLFetch) != SQL_NO_DATA

proc next*(ds: OdbcAnyResult, ret: var OdbcRowSet): bool =
    ## Advance the result set cursor one row and retrieve all data in the row
    ## into `row`.
    if not ds.next:
        return false
    if ret.vals.len == 0:
        ds.initRowSet(ret)
    for i in 0..<ret.vals.len:
        let idx = TSqlUSmallInt(i + 1)
        case ret.vals[i].kind
        of otByteArray  : ds.getData(idx, ret.vals[i].bytes)
        of otCharArray  : ds.getData(idx, ret.vals[i].chars)
        of otWideArray  : ds.getData(idx, ret.vals[i].wchars)
        of odbcIntKinds : ds.getData(idx, ret.vals[i].i64)
        of otFloat32    : ds.getData(idx, ret.vals[i].f32)
        of otFloat64    : ds.getData(idx, ret.vals[i].f64)
        of otDate       : ds.getData(idx, ret.vals[i].date)
        of otTime       : ds.getData(idx, ret.vals[i].time)
        of otTimestamp  : ds.getData(idx, ret.vals[i].datetime)
        of otGuid       : ds.getData(idx, ret.vals[i].guid)
    true

proc next*[T](ds: OdbcAnyResult, ret: var T): bool =
    if not ds.next:
        return false
    getDatas(ds, ret)

iterator items*(ds: OdbcAnyResult): OdbcRowSet =
    var row: OdbcRowSet
    while ds.next(row):
        yield row

iterator items*[T](ds: OdbcAnyResult, _: typedesc[T]): T =
    var row: T
    while ds.next(row):
        yield row

template execFirst*(conn: OdbcConn, qry: string, params: varargs[untyped]): OdbcRowSet =
    ## Specialized `exec` that only gets the first row of a query. This is
    ## useful for queries with "top 1" or "output" clauses.
    let ds = exec(conn, qry, params)
    var ret: OdbcRowSet
    doAssert ds.next(ret) == true
    ds.discardResults
    ret

template execScalar*(conn: OdbcConn, qry: string, params: varargs[untyped]): OdbcValue =
    ## Specialized `exec` that only fetches the first column of the first
    ## row of a query.
    let row = execFirst(conn, qry, params)
    row[0]

# {{{2 Prepared execution

proc prep*(stmt: sink OdbcStmt, qry: string): OdbcPreparedStmt =
    let qryUtf16 = qry.utf8To16
    prepareInternal(stmt, qryUtf16)
    stmt.transit OdbcPreparedStmt

proc prep*(conn: OdbcConn, qry: string): OdbcPreparedStmt =
    conn.newStmt.prep qry

proc unbind*(ds: sink OdbcPreparedResultSet): OdbcPreparedStmt =
    ## Unbind parameters so the prepared dataset is ready for next execution.
    ds.unbindAll
    ds.transit OdbcPreparedStmt

template basePrepExec(stmt: OdbcPreparedStmt, params: varargs[untyped]) =
    var numParams: TSqlSmallInt
    odbcCheck(stmt, SQLNumParams(SqlHandle(stmt), numParams))
    if numParams != varargsLen(params):
        raise newException(OdbcException, "Number of parameters in query (" &
            $numParams & ") and `params` (" & $varargsLen(params) & ") is not the same.")
    bindParamsSingle(OdbcStmt(stmt), params)
    execInternal(OdbcStmt(stmt))

template exec*(stmt: sink OdbcPreparedStmt, params: varargs[untyped]): OdbcPreparedResultSet =
    ## Execute the prepared statement and transition into a result set.
    basePrepExec(stmt, params)
    transit(stmt, OdbcPreparedResultSet)

template execOnly*(stmt: OdbcPreparedStmt, params: varargs[untyped]) =
    ## Execute the prepared statement and discarding the result set.
    basePrepExec(stmt, params)
    discardResults(OdbcFastResultSet(stmt))
    closeCursor(OdbcStmt(stmt))
    unbindParams(OdbcStmt(stmt))

macro withExec*(stmt: var OdbcAnyPrepared, paramsAndCode: varargs[untyped]) =
    ## Executes the prepared statement and then executes the code in the last
    ## argument to `withExec` where a result set named `rs` with type
    ## `OdbcPreparedResultSet` is injected into scope.
    ##
    ## ```nim
    ## var stmt = conn.prep "select * from MyTable where MyCol = ?"
    ## stmt.withExec(4):
    ##   for row in rs:
    ##     echo row[0], " and ", row[1]
    ## ```
    ##
    ## The use-case for this is, when using multiple `withExec`s on a single
    ## statement, to avoid having to transition the `stmt` back and forth. This
    ## increases code tidiness.
    let
        params = nnkArgList.newTree(paramsAndCode[0..<paramsAndCode.len-1])
        code = paramsAndCode[^1]
    quote do:
        block:
            let rs {.inject.} = exec(`stmt`, `params`)
            `code`
            `stmt` = unbind(rs)

# {{{1 ORM
# {{{2 Type generation

func describeToList(desc: openArray[string]): seq[string] =
    for i in 0..<desc.len div 2:
        result.add desc[i*2]

proc gorgeRaise(cmd: string): string {.compileTime.} =
    let (outp, rc) = gorgeEx(cmd, cache = "odbcn")
    if rc != 0:
        macros.error "Couldn't execute command '" & cmd & "' output: " & outp
    outp

func parseDesc(x: string): seq[string] =
    if x == "":
        return
    x.splitLines

func genRowType(desc: openArray[string]): NimNode =
    case desc.len div 2
    of 0: ident"void"
    of 1: desc[1].parseInt.OdbcPrimType.toTy
    else:
        let recList = nnkRecList.newNimNode
        for i in 0..<desc.len div 2:
            let
                fieldName = desc[i*2].normalize.ident
                ty = desc[i*2+1].parseInt.OdbcPrimType
            recList.add newIdentDefs(
                fieldName,
                #nnkPragmaExpr.newTree(
                #    fieldName,
                #    nnkPragma.newTree(
                #        nnkExprColonExpr.newTree(newIdentNode("odbcTy"), newLit(ty)))),
                ty.toTy)
        nnkObjectTy.newTree(newEmptyNode(), newEmptyNode(), recList)

# {{{2 Main

proc newLit(i: Utf16Char): NimNode {.borrow.}
proc `==`*(x, y: Utf16Char): bool {.borrow.}

macro prep*(stmt: sink OdbcStmt, qry: static string,
            connString: static string = ""): untyped =
    ## Prepare a compile-time-known query.
    let
        (normQueryUtf8, paramNames) = qry.parseQuery
        normQueryUtf16 = normQueryUtf8.utf8To16.newLit
        desc =
            if connString != "":
                gorgeRaise("odbcn_describeqry \"" & normQueryUtf8 & "\" \"" & connString & "\"").parseDesc
            else:
                @[]
        colOrder = desc.describeToList
    assert desc.len mod 2 == 0, $desc
    let dataTy = desc.genRowType
    when defined odbcnDebug:
        macros.hint("Generated type: " & dataTy.repr, callsite())
    quote do:
        type
            OdbcGenTyPreparedStmt {.gensym, stmtKind, preparedKind.} = distinct OdbcStmt
            OdbcGenTyPreparedResultSet {.gensym, stmtKind, resultKind.} = distinct OdbcGenTyPreparedStmt
            OdbcGenRow {.gensym.} = `dataTy`

        template baseExecute(stmt: typed, params: varargs[untyped]) {.gensym.} =
            bindParams(OdbcStmt(stmt), `paramNames`, params)
            execInternal(OdbcStmt(stmt))

        template bindParams[T: object or tuple](
            stmt: OdbcGenTyPreparedResultSet | OdbcGenTyPreparedStmt,
            params: T,
        ) {.used.} =
            bindParams(stmt, params, `paramNames`)

        template exec(stmt: sink OdbcGenTyPreparedStmt,
                      params: varargs[untyped]): OdbcGenTyPreparedResultSet {.used.} =
            baseExecute(stmt, params)
            transit(stmt, OdbcGenTyPreparedResultSet)

        proc unbind(ds: sink OdbcGenTyPreparedResultSet): OdbcGenTyPreparedStmt {.used.} =
            unbindAll(OdbcStmt(ds))
            transit(ds, OdbcGenTyPreparedStmt)

        template execOnly(stmt: OdbcGenTyPreparedStmt, params: varargs[untyped]) {.used.} =
            baseExecute(stmt, params)
            discardResults(OdbcFastResultSet(stmt))
            closeCursor(OdbcStmt(stmt))
            unbindParams(OdbcStmt(stmt))

        when OdbcGenRow isnot void:
            proc initRowSet(stmt: OdbcGenTyPreparedStmt | OdbcGenTyPreparedResultSet): OdbcGenRow {.used.} =
                discard

            proc next[T](ds: OdbcGenTyPreparedResultSet, ret: var T): bool {.used.} =
                if not ds.next:
                    return false
                getDatas(ds, ret, `colOrder`)
                true

            template execFirst(stmt: OdbcGenTyPreparedStmt,
                               params: varargs[untyped]): OdbcGenRow {.used.} =
                baseExecute(stmt, params)
                var ret: OdbcGenRow
                doAssert next(OdbcGenTyPreparedResultSet(stmt), ret)
                closeCursor(OdbcStmt(stmt))
                unbindParams(OdbcStmt(stmt))
                ret

            iterator items(ds: OdbcGenTyPreparedResultSet): OdbcGenRow {.used.} =
                var row: OdbcGenRow
                while ds.next(row):
                    yield row

            iterator items[T](ds: OdbcGenTyPreparedResultSet, _: typedesc[T]): T {.used.} =
                var row: T
                while ds.next(row):
                    yield row

        const query = `normQueryUtf16`
        prepareInternal(`stmt`, cast[seq[Utf16Char]](query))
        transit(`stmt`, OdbcGenTyPreparedStmt)

template prep*(conn: OdbcConn, qry: static string,
               connString: static string = ""): untyped =
    ## Prepare a compile-time-known query.
    let stmt = conn.newStmt
    prep(stmt, qry, connString)

# {{{1 Utility ODBC functions

iterator listDrivers*(
    env = globalOdbcEnv,
): tuple[name, attrs: string] {.tags: [ReadIOEffect].} =
    ## Iterate over all ODBC drivers on the system. On Windows, these are
    ## defined in the registry. On Linux with unixodbc, these are defined in
    ## "/etc/odbcinst.ini".
    ##
    ## The `name` field is the ODBC driver name. This is used as value to the
    ## "DRIVER" attribute when connecting to a server.
    ##
    ## The `attrs` field is a list delimited by null-bytes describing the
    ## attributes of the driver. Each element is a key-value pair delimited by
    ## a `=`. The `attrs` field may look like this:
    ## `FileUsage=1\0FileExtns=*.dbf\0`. The `\0` is a null-byte character.
    var
        driver {.noinit.}: array[256, TSqlChar]
        attr {.noinit.}: array[2048, TSqlChar]
        driverLen, attrLen: TSqlSmallInt
    template doSqlDrivers(direction): TSqlSmallInt =
        odbcCheckRet env, SqlHandle(env).SQLDrivers(
            direction, driver[0].addr, driver.len.TSqlSmallInt, driverLen.addr,
            attr[0].addr, attr.len.TSqlSmallInt, attrLen.addr)
    var rc = doSqlDrivers(SQL_FETCH_FIRST)
    while rc != SQL_NO_DATA:
        yield (driver.toOpenArray(0, driverLen - 1).toString,
               attr.toOpenArray(0, attrLen - 1).toString)
        rc = doSqlDrivers(SQL_FETCH_NEXT)

type DataSourcesFilter* = enum dsfAll, dsfUser, dsfSystem
const filterAsC = [
    dsfAll: SQL_FETCH_FIRST.TSqlUSmallInt,
    dsfUser: SQL_FETCH_FIRST_USER,
    dsfSystem: SQL_FETCH_FIRST_SYSTEM,
]

iterator listDataSources*(env: OdbcEnv = globalOdbcEnv,
                          filter: DataSourcesFilter = dsfAll
                          ): tuple[server, driver: string] {.tags: [ReadIOEffect].} =
    ## Iterate over all "data sources", or DSNs, on the system. On Windows,
    ## these are defined in the registry. On Linux with unixodbc, these are
    ## defined in "/etc/odbc.ini" or "~/.odbc.ini".
    ##
    ## The `server` field is the DSN name. This is used as value to the DSN
    ## attribute when connecting to a server.
    ##
    ## The `driver` field is the ODBC driver used when connecting to the DSN.
    var
        server {.noinit.}: array[256, TSqlChar]
        driver {.noinit.}: array[256, TSqlChar]
        serverLen, driverLen: TSqlSmallInt
    template doSql(direction): TSqlSmallInt =
        odbcCheckRet env, SqlHandle(env).SQLDataSources(
            direction, server[0].addr, server.len.TSqlSmallInt, serverLen,
            driver[0].addr, driver.len.TSqlSmallInt, driverLen)
    var rc = doSql(filterAsC[filter])
    while rc != SQL_NO_DATA:
        yield (server.toOpenArray(0, serverLen - 1).toString,
               driver.toOpenArray(0, driverLen - 1).toString)
        rc = doSql(SQL_FETCH_NEXT)

{.pop.}
