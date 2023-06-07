## Provides the typed schema with which data is received from or sent to ODBC
## drivers. This contains the implementation of all the overloads for
## `bindParam`, `bindCol` and `getData` procs. Importing this module is
## unneeded except for helping overload these procs. It is intended that
## overloading these procs can be done by wrapping them, because they cover the
## low-level schema with the ODBC drivers.
##
## See `inttests/tdataschema.nim` for examples.
##
## General documentation follow:
##
## # `bindParam`
##
## This proc is used for binding parameters to a query, i.e. the arguments to
## `exec`.
##
## Make a template in scope with the name `bindParam` and the signature:
##
## ```nim
## template bindParam(stmt: SqlHandle, i: TSqlUSmallInt, param: ptr T)
## ```
##
## The signature is prone to change, and may change from template to proc in
## the future. This routine must be a template, else the binding may not work.
## `T` is the type to overload. Then
##
## 1. Convert `T` to a type supported by `bindParam`s in this module
## 2. Call `bindParam(stmt, i, convertedVal)`
##
## # `getData`
##
## This proc is used for getting data from a row in a result set, e.g. `next`
## calls or using `items(T)` iterator.
##
## Make a proc in scope with the name `getData` and the signature:
##
## ```nim
## proc getData(stmt: SqlHandle, i: TSqlUSmallInt, ret: var T)
## ```
##
## `T` is the type to overload. Then
##
## 1. Declare a variable of a type that is supported by `getData`s in this
##    module
## 2. Call `getData(stmt, i, myVar)`
## 3. Convert `myVar` to `T` and assign to `ret`
##
## # `bindCol`
##
## This proc is used for the `bindCols` proc. See documentation on that proc.
## It has more niche use, and overloading this proc will mean calling ODBC C
## API directly, because no transformation of retrieved values can occur. You
## know what you're doing if you're overloading this.

import std/[
    options,
]

import "."/[
    errors,
    wrapper,
    widestrx,
]

export wrapper

type
    OdbcArrayType* = byte | char | Utf16Char
        ## Valid element types for `array` and `seq`.

    OdbcFixedLenType* = SomeNumber | bool | enum | GUID | OdbcDate |
        OdbcTime | OdbcTimestamp
        ## Non-array types that are first-class citizens in ODBC's type system

    ResizableContainer*[T] = concept var x
        x[int] is var T
        x.setLen(int)

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
        of otByteArray: bytes*: seq[byte]
        of otCharArray: chars*: seq[char]
        of otWideArray: wchars*: seq[Utf16Char]
        of odbcIntKinds: i64*: int64
        of otFloat32: f32*: float32
        of otFloat64: f64*: float64
        of otDate: date*: OdbcDate
        of otTime: time*: OdbcTime
        of otTimestamp: datetime*: OdbcTimestamp
        of otGuid: guid*: GUID

# {{{2 Primitive type mappings

const
    odbcTyAsPrimTy = [
        ## Describes the "SQL data type" most appropriate for a "C data type".
        ## This mapping differs from `primTyAsOdbcTy` in that this is used to
        ## determine the "SQL data type" a query parameter should have, instead
        ## of determining the "C data type" to use when retrieving data.
        otByteArray: SQL_VARBINARY.TSqlSmallInt,
        otCharArray: SQL_VARCHAR,
        otWideArray: SQL_WVARCHAR,
        otBool: SQL_BIT,
        otInt8: SQL_TINYINT,
        otInt16: SQL_SMALLINT,
        otInt32: SQL_INTEGER,
        otInt64: SQL_BIGINT,
        otFloat32: SQL_REAL,
        otFloat64: SQL_FLOAT,
        otDate: SQL_TYPE_DATE,
        otTime: SQL_TYPE_TIME,
        otTimestamp: SQL_TYPE_TIMESTAMP,
        otGuid: SQL_GUID,
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

func intSizeToOdbcTy(sz: int): OdbcPrimType =
    case sz
    of 1: otInt8
    of 2: otInt16
    of 4: otInt32
    of 8: otInt64
    else: raise newException(OdbcException, "not implemented integer size " & $sz)

func elemToPrimTy(_: typedesc[byte]): OdbcPrimType = otByteArray
func elemToPrimTy(_: typedesc[char]): OdbcPrimType = otCharArray
func elemToPrimTy(_: typedesc[Utf16Char]): OdbcPrimType = otWideArray
func elemToPrimTy[T](ty: typedesc[T]): OdbcPrimType =
    {.error ty.name & " not implemented for `elemToPrimTy`".}

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

func toPrimTy[T](_: typedesc[openArray[T]]): OdbcPrimType = elemToPrimTy(T)
func toPrimTy[T](_: typedesc[seq[T]]): OdbcPrimType = elemToPrimTy(T)
func toPrimTy[I, T](_: typedesc[array[I, T]]): OdbcPrimType = elemToPrimTy(T)

func toPrimTy[T](ty: typedesc[T]): OdbcPrimType =
    {.error: $ty & " not implemented for `toPrimTy`".}

func toCTy(x: OdbcPrimType): TSqlSmallInt = primTyAsCTy[x]
func toBestOdbcTy(x: OdbcPrimType): TSqlSmallInt = odbcTyAsPrimTy[x]
func toCTy(x: typedesc): TSqlSmallInt = x.toPrimTy.toCTy

template myOdbcCheck(stmt, call) =
    odbcCheck(stmt, SQL_HANDLE_STMT, call)

# {{{1 bindParam

# NOTE: Ignoring `StrLen_or_IndPtr` for binding columns. It is usually required
# for checking if a value is `NULL` after a `SQLFetch`. Reasons not to check
# this:
# * Given a normalized database environment, no columns may contain `NULL`
#   values to begin with
# * It is impractical for `bindCols`, which is intended not to store any state
#   other than the data pointers themselves
# This could be implemented for `Option[T]` types.

proc bindParamPtr(stmt: SqlHandle, idx: TSqlUSmallInt, cTy: TSqlSmallInt,
                  odbcTy: TSqlSmallInt, param: pointer, paramLen: int,
                  indPtr: ptr TSqlLen) =
    # FAQ: Why is there redundancy in how the buffer size is given
    # (StrLen_or_IndPtr and ColumnSize)? Different drivers get the buffer size
    # in different ways:
    # * ODBC Driver 17 and 18 (and maybe older) use `StrLen_or_IndPtr` argument
    #   to determine size in bytes of buffer; ColumnSize and BufferLength are
    #   ignored
    # * FreeTDS use ColumnSize argument to determine size in length (number of
    #   elements, e.g. UTF16 string has byteSize/2 length); BufferLength and
    #   StrLen_or_IndPtr are ignored
    myOdbcCheck(stmt, SQLBindParameter(
        stmt, idx, SQL_PARAM_INPUT, cTy, odbcTy, TSqlULen(paramLen),
        0, param, 0, indPtr))

var emptyBuf: array[1, Utf16Char]

proc bindParamPtrGeneric[T: OdbcArrayType](
    stmt: SqlHandle,
    idx: TSqlUSmallInt,
    param: openArray[T],
    indPtr: ptr TSqlLen,
) =
    const
        primTy = toPrimTy(param.type)
        cTy = toCTy(primTy)
        odbcTy = toBestOdbcTy(primTy)
    let
        len = param.len
        buf = if len == 0: cast[ptr T](emptyBuf[0].addr) else: param[0].unsafeAddr
        colLen = if len == 0: 1 else: len # Workaround for bug in "SQL Server" driver
    bindParamPtr(stmt, idx, cTy, odbcTy, buf, colLen, indPtr)

template bindParam*(stmt: SqlHandle, idx: TSqlUSmallInt, param: openArray[OdbcArrayType]) =
    var len {.gensym.} = if param.len == 0: 0 else: param.len * sizeof(param[0])
    bindParamPtrGeneric(stmt, idx, param, len.addr)

template bindParam*(stmt: SqlHandle, idx: TSqlUSmallInt, param: openArray[char]) =
    var wideParam = utf8To16(param)
    bindParam(stmt, idx, wideParam)

let sqlnts = SQL_NTS

proc bindParam*[I](stmt: SqlHandle, idx: TSqlUSmallInt, param: ptr array[I, char | Utf16Char]) =
    bindParamPtrGeneric(stmt, idx, param[], sqlnts.unsafeAddr)

proc bindParam*[I](stmt: SqlHandle, idx: TSqlUSmallInt, param: ptr array[I, byte]) =
    var len {.global.}: TSqlLen = sizeof(param[]) # instantiated per `I`
    bindParamPtrGeneric(stmt, idx, param[], len.addr)

proc bindParam*(stmt: SqlHandle, idx: TSqlUSmallInt, param: ptr SomeFloat) =
    const
        primTy = toPrimTy(param[].type)
        cTy = toCTy(primTy)
        odbcTy = toBestOdbcTy(primTy)
    myOdbcCheck(stmt, SQLBindParameter(
        stmt, idx, SQL_PARAM_INPUT, cTy, odbcTy, 27, 7, param, 0, nil))

proc bindParam*(stmt: SqlHandle, idx: TSqlUSmallInt,
               param: ptr (OdbcFixedLenType and not SomeFloat)) =
    const
        primTy = toPrimTy(param[].type)
        cTy = toCTy(primTy)
        odbcTy = toBestOdbcTy(primTy)
    myOdbcCheck(stmt, SQLBindParameter(
        stmt, idx, SQL_PARAM_INPUT, cTy, odbcTy, 0, 0, param, 0, nil))

let nullInd = SQL_NULL_DATA

proc bindParam*[T](stmt: SqlHandle, idx: TSqlUSmallInt, param: ptr Option[T]) =
    if param[].isSome:
        bindParam(stmt, idx, param[].get.addr)
    else:
        const
            primTy = toPrimTy(T)
            cTy = toCTy(primTy)
            odbcTy = toBestOdbcTy(primTy)
        myOdbcCheck(stmt, SQLBindParameter(
            stmt, idx, SQL_PARAM_INPUT, cTy, odbcTy, 0, 0, nil, 0, nullInd.unsafeAddr))

proc bindParam*[T](stmt: SqlHandle, idx: TSqlUSmallInt, param: ptr T) =
    {.error: "Type `" & $T.type  & "` is not a supported type " &
        "for binding parameters. See `typed_overload` module for supporting".}

# {{{1 getData

{.push sideEffect, tags: [IOEffect].}

proc getData*[T: OdbcArrayType](ds: SqlHandle, colIdx: TSqlUSmallInt, ret: var openArray[T]) =
    const cTy = toCTy(ret.type)
    let chunkSize = ret.len * sizeof(T)
    var
        ind: TSqlLen
    myOdbcCheck(ds, SQLGetData(
        ds, colIdx, cTy, ret[0].addr, chunkSize, ind.addr))
    assert ind != SQL_NO_TOTAL
    case ind
    of SQL_NULL_DATA: ret[0] = T(0)
    else: discard

proc getData*[I; T: OdbcArrayType](ds: SqlHandle, colIdx: TSqlUSmallInt, ret: var array[I, T]) =
    getData(ds, colIdx, ret.toOpenArray(0, ret.len-1))

proc getDataResizable*[T](
    ds: SqlHandle,
    colIdx: TSqlUSmallInt,
    ret: var ResizableContainer[T],
) =
    ## `getData` into a resizable container, such as `seq`.
    ##
    ## Reason for not making this an overload of `getData` is that it produces
    ## a lot of XDeclaredButNotUsed hints that I can't get rid of.
    const
        chunkSize = 1024
        cTy = toCTy(elemToPrimTy(T))
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
        let rc = odbcCheckRet(ds, SQL_HANDLE_STMT, SQLGetData(
            ds, colIdx, cTy, bufPtr, chunkSize, ind.addr))
        if rc != SQL_SUCCESS_WITH_INFO:
            ind += curOffset
            break
        when isWide:
            bufLen -= 2
        else:
            if ret[bufLen-1].char == '\0': bufLen -= 1
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

proc getData*[T](
    ds: SqlHandle,
    colIdx: TSqlUSmallInt,
    ret: var seq[T],
) =
    getDataResizable(ds, colIdx, ret)

proc getData*(ds: SqlHandle, colIdx: TSqlUSmallInt, ret: var string) =
    var wideVal = newSeqOfCap[Utf16Char](ret.len * 2)
    getData(ds, colIdx, wideVal)
    ret = utf16To8(wideVal)

proc getDataTry(ds: SqlHandle, colIdx: TSqlUSmallInt, ret: var OdbcFixedLenType): bool =
    const cTy = toCTy(ret.type)
    var ind: TSqlLen
    myOdbcCheck ds, SQLGetData(
        ds, colIdx, cTy, ret.addr, 0, ind.addr)
    result = ind != SQL_NULL_DATA

proc getData*(ds: SqlHandle, colIdx: TSqlUSmallInt, ret: var OdbcFixedLenType) =
    if not getDataTry(ds, colIdx, ret):
        ret.reset

proc getData*[T: OdbcFixedLenType](ds: SqlHandle, colIdx: TSqlUSmallInt, ret: var Option[T]) =
    var val: T
    if getDataTry(ds, colIdx, val):
        ret = some(val)
    else:
        ret = none(T)

proc getData*(ds: SqlHandle, colIdx: TSqlUSmallInt, ret: var OdbcValue) =
    case ret.kind
    of otByteArray  : ds.getData(colIdx, ret.bytes)
    of otCharArray  : ds.getData(colIdx, ret.chars)
    of otWideArray  : ds.getData(colIdx, ret.wchars)
    of odbcIntKinds : ds.getData(colIdx, ret.i64)
    of otFloat32    : ds.getData(colIdx, ret.f32)
    of otFloat64    : ds.getData(colIdx, ret.f64)
    of otDate       : ds.getData(colIdx, ret.date)
    of otTime       : ds.getData(colIdx, ret.time)
    of otTimestamp  : ds.getData(colIdx, ret.datetime)
    of otGuid       : ds.getData(colIdx, ret.guid)

proc getData*[T](ds: SqlHandle, colIdx: TSqlUSmallInt, ret: var T) =
    {.error: "Type `" & $ret.type  & "` is not a supported type " &
        "for getting data into.".}

{.pop.}

# {{{1 bindCol

# Wrapper to reduce generic code generation.
proc bindColPointer(stmt: SqlHandle, colIdx: TSqlUSmallInt, cTy: TSqlSmallInt,
                    ret: pointer, size: int) =
    myOdbcCheck stmt, stmt.SQLBindCol(colIdx, cTy, ret, size, nil)

proc bindCol*[I; T: OdbcArrayType](stmt: SqlHandle, colIdx: TSqlUSmallInt, ret: var array[I, T]) =
    bindColPointer(stmt, colIdx, toCTy(ret.type), ret[0].addr, sizeof(ret))

proc bindCol*(stmt: SqlHandle, colIdx: TSqlUSmallInt, ret: var OdbcFixedLenType) =
    myOdbcCheck stmt, stmt.SQLBindCol(colIdx, ret.type.toCTy, ret.addr, 0, nil)

proc bindCol*[T](stmt: SqlHandle, colIdx: TSqlUSmallInt, ret: var T) =
    {.error: "Type `" & $ret.type  & "` is not a supported type " &
        "for binding columns.".}
