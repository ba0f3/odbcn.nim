import std/[strtabs, strutils]

type ConnString* = distinct StringTableRef

proc newConnString*(keyValues: varargs[tuple[key, val: string]]): ConnString =
    newStringTable(keyValues, mode = modeCaseInsensitive).ConnString

proc `$`*(x: ConnString): string =
    for (key, val) in x.StringTableRef.pairs:
        result &= ";" & key & "=" & val

proc `[]`*(t: ConnString, key: string): var string =
    t.StringTableRef[key]
proc `[]=`*(t: ConnString, key, val: string) {.borrow.}
proc clear*(s: ConnString) {.borrow.}
proc contains*(t: ConnString, key: string): bool {.borrow.}
proc del*(t: ConnString, key: string) {.borrow.}
proc getOrDefault*(t: ConnString, key: string, default = ""): string {.borrow.}

proc newConnString*(connString: string): ConnString =
    result = newConnString()
    for attr in connString.split(';'):
        let keyVal = attr.split('=', 1)
        if attr.len != 2:
            continue
        result[keyVal[0]] = keyVal[1]
