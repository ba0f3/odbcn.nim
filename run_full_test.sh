#!/usr/bin/env sh
set -e
nimble install -y
nimble test
nim r -f inttests/odbc.nim
nim gen_docs.nims
