## Ripped code from `system/widestrs` made more flexible with primitive types.
## This makes conversion between UTF-8 and UTF-16 much easier, possible to do
## at compile-time, and involve less allocations. These are possible to use at
## compile-time, as opposed to `system/widestrs.newWideCString`, which uses raw
## allocation. Raw allocations are not permitted in nimvm.

import system/widestrs {.all.}

# Copy of `system/widestrs.fastRuneAt` except type of `s` changed from `cstring`
# to `openArray[char]`.
template fastRuneAt(s: openArray[char], i: int, result: untyped, doInc = true) =
    ## Returns the unicode character `s[i]` in `result`. If `doInc == true`
    ## `i` is incremented by the number of bytes that have been processed.
    bind ones

    if ord(s[i]) <= 127:
        result = ord(s[i])
        when doInc: inc(i)
    elif ord(s[i]) shr 5 == 0b110:
        #assert(ord(s[i+1]) shr 6 == 0b10)
        if i <= s.len - 2:
            result = (ord(s[i]) and (ones(5))) shl 6 or (ord(s[i+1]) and ones(6))
            when doInc: inc(i, 2)
        else:
            result = UNI_REPL
            when doInc: inc(i)
    elif ord(s[i]) shr 4 == 0b1110:
        if i <= s.len - 3:
            #assert(ord(s[i+1]) shr 6 == 0b10)
            #assert(ord(s[i+2]) shr 6 == 0b10)
            result = (ord(s[i]) and ones(4)) shl 12 or
                     (ord(s[i+1]) and ones(6)) shl 6 or
                     (ord(s[i+2]) and ones(6))
            when doInc: inc(i, 3)
        else:
            result = UNI_REPL
            when doInc: inc(i)
    elif ord(s[i]) shr 3 == 0b11110:
        if i <= s.len - 4:
            #assert(ord(s[i+1]) shr 6 == 0b10)
            #assert(ord(s[i+2]) shr 6 == 0b10)
            #assert(ord(s[i+3]) shr 6 == 0b10)
            result = (ord(s[i]) and ones(3)) shl 18 or
                     (ord(s[i+1]) and ones(6)) shl 12 or
                     (ord(s[i+2]) and ones(6)) shl 6 or
                     (ord(s[i+3]) and ones(6))
            when doInc: inc(i, 4)
        else:
            result = UNI_REPL
            when doInc: inc(i)
    else:
        result = 0xFFFD
        when doInc: inc(i)

# Copy of `system/widestrs.runes` except type of `s` changed from `cstring`
# to `openArray[char]`.
iterator runes*(s: openArray[char]): int =
    ## Iterates over any rune of the string ``s`` returning runes.
    var
        i = 0
        result: int
    while i < len(s):
        fastRuneAt(s, i, result, true)
        yield result

proc utf8To16*(s: openArray[char]): seq[Utf16Char] =
    for chRune in runes(s):
        let ch = chRune.int
        if ch <= UNI_MAX_BMP:
            if ch >= UNI_SUR_HIGH_START and ch <= UNI_SUR_LOW_END:
                result.add UNI_REPLACEMENT_CHAR
            else:
                result.add Utf16Char(ch)
        elif ch > UNI_MAX_UTF16:
            result.add UNI_REPLACEMENT_CHAR
        else:
            let ch = ch - halfBase
            result.add Utf16Char((ch shr halfShift) + UNI_SUR_HIGH_START)
            result.add Utf16Char((ch and halfMask) + UNI_SUR_LOW_START)

proc utf16To8*(w: openArray[Utf16Char]): string =
    const replacement = 0xFFFD
    var i = 0
    let len = w.len
    while i < len:
        var ch = ord(w[i])
        inc i
        if ch >= UNI_SUR_HIGH_START and ch <= UNI_SUR_HIGH_END:
            # If the 16 bits following the high surrogate are in the source buffer...
            let ch2 = ord(w[i])

            # If it's a low surrogate, convert to UTF32:
            if ch2 >= UNI_SUR_LOW_START and ch2 <= UNI_SUR_LOW_END:
                ch = (((ch and halfMask) shl halfShift) + (ch2 and halfMask)) + halfBase
                inc i
            else:
                #invalid UTF-16
                ch = replacement
        elif ch >= UNI_SUR_LOW_START and ch <= UNI_SUR_LOW_END:
            #invalid UTF-16
            ch = replacement

        if ch < 0x80:
            result.add chr(ch)
        elif ch < 0x800:
            result.add chr((ch shr 6) or 0xc0)
            result.add chr((ch and 0x3f) or 0x80)
        elif ch < 0x10000:
            result.add chr((ch shr 12) or 0xe0)
            result.add chr(((ch shr 6) and 0x3f) or 0x80)
            result.add chr((ch and 0x3f) or 0x80)
        elif ch <= 0x10FFFF:
            result.add chr((ch shr 18) or 0xf0)
            result.add chr(((ch shr 12) and 0x3f) or 0x80)
            result.add chr(((ch shr 6) and 0x3f) or 0x80)
            result.add chr((ch and 0x3f) or 0x80)
        else:
            # replacement char(in case user give very large number):
            result.add chr(0xFFFD shr 12 or 0b1110_0000)
            result.add chr(0xFFFD shr 6 and ones(6) or 0b10_0000_00)
            result.add chr(0xFFFD and ones(6) or 0b10_0000_00)

when isMainModule:
    import unittest

    test "Simple convert":
        check "Hey".utf8To16.utf16To8 == "Hey"
    test "Unicode convert":
        check "Heyø".utf8To16.utf16To8 == "Heyø"
