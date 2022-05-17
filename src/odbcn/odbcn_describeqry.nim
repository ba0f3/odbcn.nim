import std/os
import ../odbcn

proc main =
    ## Output information for the result set columns of the given `qry`. The
    ## format is `<column name>\n<OdbcType as ordinal>\n` for each column in
    ## the result set.
    if paramCount() != 2:
        echo "Exactly 2 arguments required: <query> <connection string>"
        quit 1
    let
        qry = paramStr(1)
        connString = paramStr(2)
        conn = connString.newOdbcConn
        stmt = conn.prep qry
    var row: OdbcRowSet
    stmt.initRowSet row
    for i in 0..<row.len:
        echo row.names[i]
        echo row[i].kind.ord

when isMainModule:
    main()
