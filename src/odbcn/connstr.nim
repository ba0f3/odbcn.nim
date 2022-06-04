import std/[tables, strutils]

# Important that it is "Ordered", because order matters for certain ODBC
# attributes. E.g. "SAVEFILE" must be specified before "DRIVER", and only first
# of "FILEDSN" and "DSN" is used for the connection. See `SQLDriverConnect`
# docs for details.
type ConnString* = distinct OrderedTable[string, string]

proc `$`*(x: ConnString): string =
    for key, val in OrderedTable[string, string](x).pairs:
        result &= ";" & key & "=" & val

proc `[]`*(t: var ConnString, key: string): var string =
    OrderedTable[string, string](t)[key.toUpperAscii]

proc `[]`*(t: ConnString, key: string): string =
    OrderedTable[string, string](t)[key.toUpperAscii]

proc `[]=`*(t: var ConnString, key, val: string) =
    OrderedTable[string, string](t)[key.toUpperAscii] = val

proc contains*(t: ConnString, key: string): bool =
    OrderedTable[string, string](t).contains(key.toUpperAscii)

proc getOrDefault*(t: ConnString, key: string, default = ""): string =
    OrderedTable[string, string](t).getOrDefault(key.toUpperAscii)

proc del*(t: var ConnString, key: string) =
    OrderedTable[string, string](t).del(key.toUpperAscii)

proc hasKeyOrPut*(t: var ConnString, key, val: string): bool =
    OrderedTable[string, string](t).hasKeyOrPut(key.toUpperAscii, val)

proc mgetOrPut*(t: var ConnString, key, val: string): var string =
    OrderedTable[string, string](t).mgetOrPut(key.toUpperAscii, val)

proc pop*(t: var ConnString, key: string, val: var string): bool =
    OrderedTable[string, string](t).pop(key.toUpperAscii, val)

proc clear*(s: var ConnString) {.borrow.}
proc `==`*(s, t: ConnString): bool {.borrow.}
proc len*(s: ConnString): int {.borrow.}

proc initConnString*(keyValues: varargs[tuple[key, val: string]]): ConnString =
    result = initOrderedTable[string, string](keyValues.varargsLen).ConnString
    for (key, val) in keyValues:
        result[key] = val

proc initConnString*(connString: string): ConnString =
    result = initConnString()
    for attr in connString.split(';'):
        let keyVal = attr.split('=', 1)
        if keyVal.len != 2:
            continue
        result[keyVal[0]] = keyVal[1]

when isMainModule:
    import unittest
    test "Empty is not an error":
        discard initConnString ""
    test "Single semicolon is not an error":
        discard initConnString ";"
    test "Empty attributes is not an error":
        discard initConnString ";Server=localhost;;Database=test;"

    test "Attributes are added for single string constructor":
        let connStr = initConnString ";Server=localhost;Database=test"
        check connStr["Server"] == "localhost"
        check connStr["Database"] == "test"
        check $connStr == ";SERVER=localhost;DATABASE=test"

    test "Attributes are added for tuple seq constructor":
        let connStr = initConnString {"Driver": "FreeTDS", "DSN": "MyDsn"}
        check connStr["Driver"] == "FreeTDS"
        check connStr["DSN"] == "MyDsn"
        check $connStr == ";DRIVER=FreeTDS;DSN=MyDsn"

    test "Attributes are case insensitive":
        let connStr = initConnString ";Server=localhost"
        check "SERvER" in connStr
