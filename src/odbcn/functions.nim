## Functions that return result sets of schema information on a database
## server. These result sets are specialized in ODBC.
##
## Example of usage follows.
##
## ```nim
## type OdbcTable = object
##   catalog: array[256, char]
##   table {.odbcCol: 3.}: array[256, char]
##   ty {.odbcCol: 4.}: array[16, char]
## let conn = newConn(...)
## let stmt = conn.newStmt
## var row: OdbcTable
## stmt.bindCols row
## let rs = stmt.selectTables(...)
## while rs.next:
##   # Do stuff with `row` here. It is updated for each `next` call.
## ```
##
## This demonstrates usage of `bindCols`, which can be used in the same way for
## normal query situations also. `odbcCol` is used above to skip column 2
## (indexed from 1) in the result set. This behavior is advised for
## specialization. The return columns is described in each relevant function in
## this module.

import "."/[
    private/core {.all.},
    wrapper,
]

const
    selectNone* = ""
    selectAll* = "%"
        ## Use this as value to `select*` arguments to select all of that
        ## kind.

{.push tags: [ReadIOEffect].}

type TablesType* = enum
    ## Input to `selectTables`, and also return value of 4th column in
    ## `selectTables`.
    ttAll
    ttTable = "TABLE"
    ttView = "VIEW"
    ttSysTable = "SYSTEM TABLE"
    ttGlobalTemp = "GLOBAL TEMPORARY"
    ttLocalTemp = "LOCAL TEMPORARY"
    ttAlias = "ALIAS"
    ttSynonym = "SYNONYM"

proc selectTables*(
    stmt: sink OdbcStmt,
    catalog, schema, table = selectNone,
    tableType: set[TablesType] = {ttAll},
): OdbcFastResultSet =
    var tableTypesStr: string
    if ttAll in tableType:
        tableTypesStr = "%"
    else:
        for ty in tableType:
            tableTypesStr &= $ty & ","
    odbcCheck stmt, SqlHandle(stmt).SQLTables(
        catalog.cstring, TSqlSmallInt(catalog.len),
        schema.cstring, TSqlSmallInt(schema.len),
        table.cstring, TSqlSmallInt(table.len),
        tableTypesStr.cstring, TSqlSmallInt(tableTypesStr.len),
    )
    stmt.transit OdbcFastResultSet

proc selectTablePrivileges*(
    stmt: sink OdbcStmt,
    catalog, schema: string, table = selectAll,
): OdbcFastResultSet =
    ## Patterns such as "%" are not allowed for `catalog` and `schema`.
    ## `catalog` and `schema` may be empty if they do not apply.
    odbcCheck stmt, SqlHandle(stmt).SQLTablePrivileges(
        catalog.cstring, TSqlSmallInt(catalog.len),
        schema.cstring, TSqlSmallInt(schema.len),
        table.cstring, TSqlSmallInt(table.len),
    )
    stmt.transit OdbcFastResultSet

proc selectColumns*(
    stmt: sink OdbcStmt,
    catalog, schema, table, column = selectNone,
): OdbcFastResultSet =
    odbcCheck stmt, SqlHandle(stmt).SQLColumns(
        catalog.cstring, catalog.len.TSqlSmallInt,
        schema.cstring, schema.len.TSqlSmallInt,
        table.cstring, table.len.TSqlSmallInt,
        column.cstring, column.len.TSqlSmallInt,
    )
    stmt.transit OdbcFastResultSet

proc selectColumnPrivileges*(
    stmt: sink OdbcStmt,
    catalog, schema, table, column = selectNone,
): OdbcFastResultSet =
    odbcCheck stmt, SqlHandle(stmt).SQLColumnPrivileges(
        catalog.cstring, catalog.len.TSqlSmallInt,
        schema.cstring, schema.len.TSqlSmallInt,
        table.cstring, table.len.TSqlSmallInt,
        column.cstring, column.len.TSqlSmallInt,
    )
    stmt.transit OdbcFastResultSet

type
    IndexUnique* = enum iuUnique, iuAll
    IndexReserved* = enum irEnsure, irQuick
const
    iuConv = [iuUnique: SQL_INDEX_UNIQUE.TSqlUSmallInt, iuAll: SQL_INDEX_ALL]
    irConv = [irEnsure: SQL_ENSURE.TSqlUSmallInt, irQuick: SQL_QUICK]

proc selectIndexes*(
    stmt: sink OdbcStmt,
    catalog, schema, table = selectNone,
    unique: IndexUnique = iuAll,
    reserved: IndexReserved = irQuick
): OdbcFastResultSet =
    odbcCheck stmt, SqlHandle(stmt).SQLStatistics(
        catalog.cstring, catalog.len.TSqlSmallInt,
        schema.cstring, schema.len.TSqlSmallInt,
        table.cstring, table.len.TSqlSmallInt,
        iuConv[unique], irConv[reserved],
    )
    stmt.transit OdbcFastResultSet

proc selectProcedures*(
    stmt: sink OdbcStmt,
    catalog, schema: string, procName = selectNone
): OdbcFastResultSet =
    ## Patterns such as "%" are not allowed for `catalog` and `schema`.
    ## `catalog` and `schema` may be empty if they do not apply.
    odbcCheck stmt, SqlHandle(stmt).SQLProcedures(
        catalog.cstring, catalog.len.TSqlSmallInt,
        schema.cstring, schema.len.TSqlSmallInt,
        procName.cstring, procName.len.TSqlSmallInt,
    )
    stmt.transit OdbcFastResultSet

proc selectProcedureColumns*(
    stmt: sink OdbcStmt,
    catalog, schema: string, procName, column = selectNone,
): OdbcFastResultSet =
    ## Patterns such as "%" are not allowed for `catalog` and `schema`.
    ## `catalog` and `schema` may be empty if they do not apply.
    odbcCheck stmt, SqlHandle(stmt).SQLProcedureColumns(
        catalog.cstring, catalog.len.TSqlSmallInt,
        schema.cstring, schema.len.TSqlSmallInt,
        procName.cstring, procName.len.TSqlSmallInt,
        column.cstring, column.len.TSqlSmallInt,
    )
    stmt.transit OdbcFastResultSet

proc selectPrimaryKeys*(
    stmt: sink OdbcStmt,
    catalog, schema, table: string,
): OdbcFastResultSet =
    ## Patterns such as "%" are not allowed for any of the string arguments.
    ## Catalog and schema may be empty if they do not apply.
    odbcCheck stmt, SqlHandle(stmt).SQLPrimaryKeys(
        catalog.cstring, catalog.len.TSqlSmallInt,
        schema.cstring, schema.len.TSqlSmallInt,
        table.cstring, table.len.TSqlSmallInt,
    )
    stmt.transit OdbcFastResultSet

proc selectForeignKeys*(
    stmt: sink OdbcStmt,
    pkCatalog, pkSchema, pkTable, fkCatalog, fkSchema, fkTable: string,
): OdbcFastResultSet =
    ## Patterns such as "%" are not allowed for any of the string arguments.
    ## Catalog and schema may be empty if they do not apply.
    odbcCheck stmt, SqlHandle(stmt).SQLForeignKeys(
        pkCatalog.cstring, pkCatalog.len.TSqlSmallInt,
        pkSchema.cstring, pkSchema.len.TSqlSmallInt,
        pkTable.cstring, pkTable.len.TSqlSmallInt,
        fkCatalog.cstring, fkCatalog.len.TSqlSmallInt,
        fkSchema.cstring, fkSchema.len.TSqlSmallInt,
        fkTable.cstring, fkTable.len.TSqlSmallInt
    )
    stmt.transit OdbcFastResultSet

type
    SpecialColumnsIdentifierType* = enum scitBestRowid, scitRowver
    SpecialColumnsScope* = enum scsCurrow, scsTransaction, scsSession
    SpecialColumnsNullable* = enum scnNullable, scnNoNulls
const
    scitConv = [
        scitBestRowid: SQL_BEST_ROWID.TSqlUSmallInt,
        scitRowver: SQL_BEST_ROWID,
    ]
    scsConv = [
        scsCurrow: SQL_SCOPE_CURROW.TSqlUSmallInt,
        scsTransaction: SQL_SCOPE_TRANSACTION,
        scsSession: SQL_SCOPE_SESSION,
    ]
    scnConv = [
        scnNullable: SQL_NULLABLE.TSqlUSmallInt,
        scnNoNulls: SQL_NO_NULLS,
    ]

proc selectSpecialColumns*(
    stmt: sink OdbcStmt,
    catalog, schema, table: string,
    identifierType: SpecialColumnsIdentifierType,
    scope: SpecialColumnsScope,
    nullable: SpecialColumnsNullable,
): OdbcFastResultSet =
    ## Patterns such as "%" are not allowed for any of the string arguments.
    ## Catalog and schema may be empty if they do not apply.
    odbcCheck stmt, SqlHandle(stmt).SQLSpecialColumns(
        scitConv[identifierType],
        catalog.cstring, catalog.len.TSqlSmallInt,
        schema.cstring, schema.len.TSqlSmallInt,
        table.cstring, table.len.TSqlSmallInt,
        scsConv[scope], scnConv[nullable],
    )
    stmt.transit OdbcFastResultSet

{.pop.}
