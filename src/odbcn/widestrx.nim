## Ripped code from `system/widestrs` made more flexible with primitive types.
## This makes conversion between UTF-8 and UTF-16 much easier, possible to do
## at compile-time, and involve less allocations. These are possible to use at
## compile-time, as opposed to `system/widestrs.newWideCString`, which uses raw
## allocation. Raw allocations are not permitted in nimvm.

import system/widestrs {.all.}
import std/unicode

proc utf8To16*(s: string): seq[Utf16Char] =
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
