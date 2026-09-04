namespace Fason

open System
open System.Text
#if !FABLE_COMPILER
open System.Globalization
#endif

/// Marks a type for which Fason will generate JSON encoders and decoders for. It will also
/// generate encoders and decoders for any other types the annotated type depends on. You
/// can also apply this to interfaces, and encoders & decoders will be generated for its
/// member function parameters and return values.
type FasonSerializableAttribute() =
    inherit Attribute()

/// Marks a type that shouldn't be encoded / decoded as-is, but should instead be unwrapped
/// and decoders & encoders should be generated for types passed as generic parameters to it.
/// For example, applying this to something like `MyType<T>` means that only encoders and
/// decoders for `T` will be generated instead of `MyType<T>` as a whole.
type FasonUnwrapAttribute() =
    inherit Attribute()

#nowarn "42"

[<AutoOpen>]
module Unsafe =
    /// Reinterprets a value as another type with no runtime cost. Used to strip and
    /// reapply units of measure, which are erased at runtime.
    let inline retype<'a, 'b> (x: 'a) : 'b =
#if FABLE_COMPILER
        unbox x
#else
        (# "" x : 'b #)
#endif

module Platform =
#if FABLE_COMPILER
    [<Fable.Core.Emit("$0.charCodeAt($1)")>]
    let codeAt (s: string, i: int) : int = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("\"\" + $0")>]
    let jsString (value: obj) : string = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("new Date($0)")>]
    let parseDate (s: string) : DateTime = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("$0.toISOString()")>]
    let toIsoString (d: DateTime) : string = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("isNaN($0.getTime())")>]
    let isInvalidDate (d: DateTime) : bool = Fable.Core.Util.jsNative

    /// Marks a Date as UTC in place, where ToUniversalTime would allocate a second one.
    [<Fable.Core.Emit("$0.kind = 1")>]
    let markUtc (d: DateTime) : unit = Fable.Core.Util.jsNative

    /// Array access without Fable's bounds-checking.
    [<Fable.Core.Emit("$0[$1]")>]
    let at (arr: obj[], i: int) : obj = Fable.Core.Util.jsNative

    // Field access on Fable's list class, where the List module makes two calls.
    [<Fable.Core.Emit("$0.tail == null")>]
    let listIsEmpty (l: 'a list) : bool = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("$0.head")>]
    let listHead (l: 'a list) : 'a = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("$0.tail")>]
    let listTail (l: 'a list) : 'a list = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("JSON.parse($0)")>]
    let jsonParse (s: string) : obj = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("JSON.stringify($0)")>]
    let jsonStringify (v: obj) : string = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("{}")>]
    let newObject () : obj = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("$0[$1]")>]
    let getField (o: obj, name: string) : obj = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("$0[$1] = $2")>]
    let setField (o: obj, name: string, v: obj) : unit = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("Object.keys($0)")>]
    let objectKeys (o: obj) : string[] = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("Array.isArray($0)")>]
    let isArray (v: obj) : bool = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("typeof $0")>]
    let typeOf (v: obj) : string = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("$0 == null")>]
    let isNullish (v: obj) : bool = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("BigInt($0)")>]
    let inline toInt64 (v: obj) : int64 = Fable.Core.Util.jsNative

    [<Fable.Core.Emit("BigInt($0)")>]
    let inline toUInt64 (v: obj) : uint64 = Fable.Core.Util.jsNative

    /// A UTC date from its parts, carrying the kind Fable's DateTime expects.
    [<Fable.Core.Emit("(() => { const d = new Date(Date.UTC($0, $1, $2, $3, $4, $5, $6)); d.kind = 1; return d; })()")>]
    let utcDate (year: int, month0: int, day: int, hour: int, minute: int, second: int, millisecond: int) : DateTime =
        Fable.Core.Util.jsNative
#else
    let inline codeAt (s: string, i: int) = int s[i]
#endif

module DateText =
    /// Two decimal digits of `text` at `i`, or -1.
    let private twoDigits (text: string, i: int) =
        let a = Platform.codeAt (text, i) - 48 // '0'
        let b = Platform.codeAt (text, i + 1) - 48

        if a < 0 || a > 9 || b < 0 || b > 9 then -1 else a * 10 + b

    /// Dates are read as UTC, as Thoth does. Anything not in the round-trip format goes
    /// to the platform parser.
    let parse (text: string) : DateTime =
        let n = text.Length

        let isRoundTrip =
            n >= 20
            && Platform.codeAt (text, 4) = 45 // '-'
            && Platform.codeAt (text, 7) = 45
            && Platform.codeAt (text, 10) = 84 // 'T'
            && Platform.codeAt (text, 13) = 58 // ':'
            && Platform.codeAt (text, 16) = 58
            && Platform.codeAt (text, n - 1) = 90 // 'Z'
            && (n = 20 || (Platform.codeAt (text, 19) = 46 && n <= 28)) // '.'

        let mutable fraction = 0
        let mutable digits = 0
        let mutable valid = isRoundTrip

        if valid && n > 20 then
            // Fractional seconds in units of 100 ns, like .NET.
            for i in 20 .. n - 2 do
                let d = Platform.codeAt (text, i) - 48 // '0'

                if d < 0 || d > 9 then
                    valid <- false
                else
                    fraction <- fraction * 10 + d
                    digits <- digits + 1

            for _ in digits..6 do
                fraction <- fraction * 10

        let year =
            if valid then
                twoDigits (text, 0) * 100 + twoDigits (text, 2)
            else
                -1

        let month = if valid then twoDigits (text, 5) else -1
        let day = if valid then twoDigits (text, 8) else -1
        let hour = if valid then twoDigits (text, 11) else -1
        let minute = if valid then twoDigits (text, 14) else -1
        let second = if valid then twoDigits (text, 17) else -1

        if
            valid
            && year >= 0
            && month >= 1
            && month <= 12
            && day >= 1
            && day <= 31
            && hour >= 0
            && hour <= 23
            && minute >= 0
            && minute <= 59
            && second >= 0
            && second <= 59
        then
#if FABLE_COMPILER
            Platform.utcDate (year, month - 1, day, hour, minute, second, fraction / 10000)
#else
            DateTime(year, month, day, hour, minute, second, DateTimeKind.Utc).AddTicks(int64 fraction)
#endif
        else
#if FABLE_COMPILER
            let parsed = Platform.parseDate text

            if Platform.isInvalidDate parsed then
                DateTime.Parse(text).ToUniversalTime()
            else
                parsed.ToUniversalTime()
#else
            DateTime.Parse(text, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind).ToUniversalTime()
#endif

#if FABLE_COMPILER
    let private pad2 (n: int) =
        if n < 10 then
            "0" + Platform.jsString n
        else
            Platform.jsString n

    /// What Fable's "O" format returns for a UTC date.
    let format (value: DateTime) : string =
        if value.Kind = DateTimeKind.Utc && value.Year >= 1000 then
            let ms = value.Millisecond

            Platform.jsString value.Year
            + "-"
            + pad2 value.Month
            + "-"
            + pad2 value.Day
            + "T"
            + pad2 value.Hour
            + ":"
            + pad2 value.Minute
            + ":"
            + pad2 value.Second
            + (if ms < 10 then ".00"
               elif ms < 100 then ".0"
               else ".")
            + Platform.jsString ms
            + "Z"
        else
            value.ToString("O")

    let parseNative (text: string) : DateTime =
        let parsed = Platform.parseDate text

        if Platform.isInvalidDate parsed then
            DateTime.Parse(text).ToUniversalTime()
        else
            Platform.markUtc parsed
            parsed

    let formatNative (value: DateTime) : string =
        if value.Kind = DateTimeKind.Utc then
            Platform.toIsoString value
        else
            format value
#endif

#if !FABLE_COMPILER
type JsonWriter() =
    let builder = StringBuilder(256)
#if NET6_0_OR_GREATER
    let numberBuffer = Array.zeroCreate<char> 64
#endif

    static let hexDigits = "0123456789abcdef"

    member inline private this.WriteInternal(value: string) = builder.Append(value) |> ignore

#if NET6_0_OR_GREATER
    /// Formats a number straight into the output, without an intermediate string.
    member inline private this.WriteNumber<'T when 'T :> ISpanFormattable and 'T :> IFormattable>(value: 'T) =
        let mutable written = 0

        if value.TryFormat(Span(numberBuffer), &written, ReadOnlySpan.Empty, CultureInfo.InvariantCulture) then
            builder.Append(numberBuffer, 0, written) |> ignore
        else
            builder.Append(value.ToString(null, CultureInfo.InvariantCulture)) |> ignore

    /// Like WriteNumber, but whole values get a ".0" suffix the way Newtonsoft prints them.
    member inline private this.WriteFloat<'T when 'T :> ISpanFormattable and 'T :> IFormattable>(value: 'T) =
        let mutable written = 0

        if value.TryFormat(Span(numberBuffer), &written, ReadOnlySpan.Empty, CultureInfo.InvariantCulture) then
            builder.Append(numberBuffer, 0, written) |> ignore

            if MemoryExtensions.IndexOfAny(ReadOnlySpan(numberBuffer, 0, written), '.', 'E') < 0 then
                builder.Append(".0") |> ignore
        else
            builder.Append(value.ToString(null, CultureInfo.InvariantCulture)) |> ignore
#else
    member inline private this.WriteNumber<'T when 'T :> IFormattable>(value: 'T) =
        builder.Append(value.ToString(null, CultureInfo.InvariantCulture)) |> ignore

    member inline private this.WriteFloat<'T when 'T :> IFormattable>(value: 'T) =
        let text = value.ToString("R", CultureInfo.InvariantCulture)
        builder.Append(text) |> ignore

        if text.IndexOfAny([| '.'; 'E' |]) < 0 then
            builder.Append(".0") |> ignore
#endif

    static member private nonFinite(isNaN: bool, isPositive: bool) =
        if isNaN then "\"NaN\""
        elif isPositive then "\"Infinity\""
        else "\"-Infinity\""

    static member private escapeFor(c: int) =
        match c with
        | 34 -> "\\\""
        | 92 -> "\\\\"
        | 10 -> "\\n"
        | 13 -> "\\r"
        | 9 -> "\\t"
        | 8 -> "\\b"
        | 12 -> "\\f"
        | _ -> "\\u00" + string hexDigits[c >>> 4] + string hexDigits[c &&& 0xF]

    /// Writes raw text, without quoting or escaping.
    member this.WritePlain(value: string) = this.WriteInternal(value)

    member this.Write(value: string) =
        this.WriteInternal("\"")

        // Runs that need no escaping are copied in one go.
        let mutable start = 0

        for i in 0 .. value.Length - 1 do
            let c = Platform.codeAt (value, i)

            if c < 32 || c = 34 || c = 92 then // control, '"', '\\'
                if i > start then
                    this.WriteInternal(value.Substring(start, i - start))

                this.WriteInternal(JsonWriter.escapeFor c)
                start <- i + 1

        if start = 0 then
            this.WriteInternal(value)
        elif start < value.Length then
            this.WriteInternal(value.Substring(start))

        this.WriteInternal("\"")

    member this.Write(value: bool) =
        this.WriteInternal(if value then "true" else "false")

    member this.Write(value: char) = this.Write(string value)

    member this.Write(value: byte) = this.WriteNumber(value)
    member this.Write(value: sbyte) = this.WriteNumber(value)
    member this.Write(value: int16) = this.WriteNumber(value)
    member this.Write(value: int32) = this.WriteNumber(value)
    member this.Write(value: uint16) = this.WriteNumber(value)
    member this.Write(value: uint32) = this.WriteNumber(value)

    member this.Write(value: single) =
        if Single.IsFinite value then
            this.WriteFloat(value)
        else
            this.WriteInternal(JsonWriter.nonFinite (Single.IsNaN value, value > 0.0f))

    member this.Write(value: double) =
        if Double.IsFinite value then
            this.WriteFloat(value)
        else
            this.WriteInternal(JsonWriter.nonFinite (Double.IsNaN value, value > 0.0))

    // 64-bit integers are quoted, as Thoth does, so JavaScript never loses precision.
    member this.Write(value: int64) =
        this.WriteInternal("\"")
        this.WriteNumber(value)
        this.WriteInternal("\"")

    member this.Write(value: uint64) =
        this.WriteInternal("\"")
        this.WriteNumber(value)
        this.WriteInternal("\"")

    member this.Write(value: Guid) =
        this.WriteInternal("\"")
        this.WriteInternal(string value)
        this.WriteInternal("\"")

    member this.Write(value: DateTime) =
        this.WriteInternal("\"")
#if NET6_0_OR_GREATER
        let mutable written = 0

        if value.TryFormat(Span(numberBuffer), &written, "O".AsSpan(), CultureInfo.InvariantCulture) then
            builder.Append(numberBuffer, 0, written) |> ignore
        else
            this.WriteInternal(value.ToString("O"))
#else
        this.WriteInternal(value.ToString("O"))
#endif
        this.WriteInternal("\"")

    member this.Write(value: TimeSpan) =
        this.WriteInternal("\"")
        this.WriteInternal(string value)
        this.WriteInternal("\"")

    // Decimals are quoted, as Thoth does, so JavaScript never loses precision.
    member this.Write(value: decimal) =
        this.WriteInternal("\"")
        this.WriteInternal(value.ToString(CultureInfo.InvariantCulture))
        this.WriteInternal("\"")

    member this.Write(value: DateTimeOffset) =
        this.WriteInternal("\"")
        this.WriteInternal(value.ToString("O", CultureInfo.InvariantCulture))
        this.WriteInternal("\"")

#if NET6_0_OR_GREATER
    member this.Write(value: DateOnly) =
        this.WriteInternal("\"")
        this.WriteInternal(value.ToString("O", CultureInfo.InvariantCulture))
        this.WriteInternal("\"")

    member this.Write(value: TimeOnly) =
        this.WriteInternal("\"")
        this.WriteInternal(value.ToString("O", CultureInfo.InvariantCulture))
        this.WriteInternal("\"")
#endif

    override this.ToString() = builder.ToString()

type JsonReader(json: string) =
    let mutable index = 0

    static let isDigit (c: int) = c >= 48 && c <= 57 // '0'..'9'

    static let isWhitespace (c: int) = c = 32 || c = 9 || c = 10 || c = 13 // space, tab, LF, CR

    static let hexValue (c: int) =
        if c >= 48 && c <= 57 then c - 48 // '0'..'9'
        elif c >= 97 && c <= 102 then c - 97 + 10 // 'a'..'f'
        elif c >= 65 && c <= 70 then c - 65 + 10 // 'A'..'F'
        else -1

    /// The char code at the current position, or -1 at the end of the input.
    member this.PeekCode() =
        if index < json.Length then
            Platform.codeAt (json, index)
        else
            -1

    /// The next character, or '\000' at the end of the input.
    member this.Peek() =
        if index < json.Length then json[index] else '\000'

    /// The character `offset` positions ahead, or '\000' past the end of the input.
    member this.PeekAt(offset: int) =
        if index + offset < json.Length then
            json[index + offset]
        else
            '\000'

    member this.Skip() = index <- index + 1
    member this.SkipCount(count: int) = index <- index + count

    member this.SkipWhitespace() =
        while index < json.Length && isWhitespace (Platform.codeAt (json, index)) do
            index <- index + 1

    member this.StartsWith(value: string) =
        index + value.Length <= json.Length
        && json.AsSpan(index).StartsWith(value.AsSpan())

    /// Skips whitespace, then consumes the given text if it comes next.
    member private this.TryRead(value: string) =
        this.SkipWhitespace()

        if this.StartsWith(value) then
            index <- index + value.Length
            true
        else
            false

    /// Skips whitespace, then consumes the character if it comes next.
    member this.ReadUntil(value: char) =
        this.SkipWhitespace()

        if index < json.Length && Platform.codeAt (json, index) = int value then
            index <- index + 1
            true
        else
            false

    /// Raises an error that shows where in the input the reader is.
    member this.Fail(message: string) : 'a = failwith $"{message}\n{this.GetHint()}"

    /// Skips whitespace, then consumes the character or fails.
    member this.Expect(value: char) =
        if not (this.ReadUntil value) then
            this.Fail $"expected '{value}'"

    member this.ReadBool() =
        if this.TryRead "true" then true
        elif this.TryRead "false" then false
        else this.Fail "expected true or false"

    member this.ReadNull() =
        if not (this.TryRead "null") then
            this.Fail "expected 'null'"

    member private this.SkipDigits() =
        while index < json.Length && isDigit (Platform.codeAt (json, index)) do
            index <- index + 1

    member private this.ReadDigits() =
        let start = index
        let mutable value = 0UL

        while index < json.Length && isDigit (Platform.codeAt (json, index)) do
            let digit = uint64 (Platform.codeAt (json, index) - 48) // '0'

            if value > (UInt64.MaxValue - digit) / 10UL then
                this.Fail "number out of range"

            value <- value * 10UL + digit
            index <- index + 1

        if index = start then
            this.Fail "expected a digit"

        value

    /// A signed integer within [min, max].
    member private this.ReadInteger(min: int64, max: int64) =
        this.SkipWhitespace()
        let negative = this.PeekCode() = 45 // '-'

        if negative then
            index <- index + 1

        let magnitude = this.ReadDigits()

        let value =
            if not negative then
                if magnitude > uint64 max then
                    this.Fail "number out of range"
                else
                    int64 magnitude
            elif magnitude > uint64 Int64.MaxValue + 1UL then
                this.Fail "number out of range"
            else
                let value =
                    if magnitude = uint64 Int64.MaxValue + 1UL then
                        Int64.MinValue
                    else
                        -(int64 magnitude)

                if value < min then
                    this.Fail "number out of range"
                else
                    value

        value

    member this.ReadInt8() = int8 (this.ReadInteger(-128L, 127L))

    member this.ReadInt16() =
        int16 (this.ReadInteger(-32768L, 32767L))

    member this.ReadInt32() =
        int32 (this.ReadInteger(-2147483648L, 2147483647L))

    member this.ReadUInt8() = uint8 (this.ReadInteger(0L, 255L))
    member this.ReadUInt16() = uint16 (this.ReadInteger(0L, 65535L))

    member this.ReadUInt32() =
        uint32 (this.ReadInteger(0L, 4294967295L))

    /// Consumes an opening quote if there is one, for numbers that may be quoted.
    member private this.SkipOpeningQuote() =
        this.SkipWhitespace()

        if this.PeekCode() = 34 then // '"'
            index <- index + 1
            true
        else
            false

    member this.ReadInt64() =
        let quoted = this.SkipOpeningQuote()
        let value = this.ReadInteger(Int64.MinValue, Int64.MaxValue)

        if quoted then
            this.Expect '"'

        value

    member this.ReadUInt64() =
        let quoted = this.SkipOpeningQuote()
        let value = this.ReadDigits()

        if quoted then
            this.Expect '"'

        value

    member private this.ReadRealNumber() =
        this.SkipWhitespace()

        match this.PeekCode() with
        | 110 -> // 'n'
            this.ReadNull()
            Double.NaN
        | 34 -> // '"'
            match this.ReadString() with
            | "NaN" -> Double.NaN
            | "Infinity" -> Double.PositiveInfinity
            | "-Infinity" -> Double.NegativeInfinity
            | other -> this.Fail $"expected a number, got '{other}'"
        | _ -> this.ReadFiniteNumber()

    /// The text of the number at the current position.
    member private this.ScanNumber() =
        let start = index

        if this.PeekCode() = 45 then // '-'
            index <- index + 1

        this.SkipDigits()

        if this.PeekCode() = 46 then // '.'
            index <- index + 1
            this.SkipDigits()

        let e = this.PeekCode()

        if e = 101 || e = 69 then // 'e', 'E'
            index <- index + 1
            let sign = this.PeekCode()

            if sign = 43 || sign = 45 then // '+', '-'
                index <- index + 1

            this.SkipDigits()

        if index = start then
            this.Fail "expected a number"

        json.Substring(start, index - start)

    member private this.ReadFiniteNumber() =
        let text = this.ScanNumber()

        match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, value -> value
        | _ -> this.Fail $"expected a number, got '{text}'"

    member this.ReadDecimal() =
        let quoted = this.SkipOpeningQuote()
        let text = this.ScanNumber()

        if quoted then
            this.Expect '"'

        match Decimal.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, value -> value
        | _ -> this.Fail $"expected a decimal, got '{text}'"

    member this.ReadSingle() = single (this.ReadRealNumber())
    member this.ReadDouble() = this.ReadRealNumber()

    member private this.ReadHex4() =
        if index + 4 > json.Length then
            this.Fail "expected a unicode escape sequence"

        let mutable value = 0

        for i in 0..3 do
            let digit = hexValue (Platform.codeAt (json, index + i))

            if digit < 0 then
                this.Fail "expected a unicode escape sequence"

            value <- (value <<< 4) ||| digit

        index <- index + 4
        char value

    /// Index of the next quote or backslash, or -1.
    member private this.FindStringEnd() =
        let found = MemoryExtensions.IndexOfAny(json.AsSpan(index), '"', '\\')
        if found < 0 then -1 else index + found

    member this.ReadString() =
        while index < json.Length && isWhitespace (Platform.codeAt (json, index)) do
            index <- index + 1

        if index >= json.Length || Platform.codeAt (json, index) <> 34 then // '"'
            this.Fail "expected a string"

        index <- index + 1
        let start = index
        let stop = this.FindStringEnd()

        if stop < 0 then
            this.Fail "reached the end of the input inside a string"

        index <- stop

        if Platform.codeAt (json, stop) = 34 then // '"'
            // Fast path: a string without escapes is a single substring.
            index <- stop + 1
            json.Substring(start, stop - start)
        else
            let builder = StringBuilder()
            builder.Append(json.Substring(start, stop - start)) |> ignore
            let mutable finished = false

            while not finished do
                if index >= json.Length then
                    this.Fail "reached the end of the input inside a string"

                let c = json[index]
                index <- index + 1

                match c with
                | '"' -> finished <- true
                | '\\' ->
                    if index >= json.Length then
                        this.Fail "reached the end of the input inside an escape sequence"

                    let e = json[index]
                    index <- index + 1

                    match e with
                    | '"' -> builder.Append('"') |> ignore
                    | '\\' -> builder.Append('\\') |> ignore
                    | '/' -> builder.Append('/') |> ignore
                    | 'b' -> builder.Append('\b') |> ignore
                    | 'f' -> builder.Append('\012') |> ignore
                    | 'n' -> builder.Append('\n') |> ignore
                    | 'r' -> builder.Append('\r') |> ignore
                    | 't' -> builder.Append('\t') |> ignore
                    | 'u' -> builder.Append(this.ReadHex4()) |> ignore
                    | _ -> this.Fail $"unknown escape sequence '\\{e}'"
                | c -> builder.Append(c) |> ignore

            builder.ToString()

    member this.ReadGuid() = Guid.Parse(this.ReadString())

    member this.ReadDateTime() = DateText.parse (this.ReadString())

    member this.ReadTimeSpan() =
        TimeSpan.Parse(this.ReadString(), CultureInfo.InvariantCulture)

    member this.ReadDateTimeOffset() =
        DateTimeOffset.Parse(this.ReadString(), CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)

#if NET6_0_OR_GREATER
    member this.ReadDateOnly() =
        DateOnly.ParseExact(this.ReadString(), "yyyy-MM-dd", CultureInfo.InvariantCulture)

    member this.ReadTimeOnly() =
        TimeOnly.Parse(this.ReadString(), CultureInfo.InvariantCulture)
#endif

    member this.ReadChar() =
        let str = this.ReadString()

        if str.Length <> 1 then
            this.Fail $"expected a single character, got a string of length {str.Length}"

        str[0]

    /// Skips the rest of a field name (the opening quote has been consumed) and its value.
    member this.SkipObjectField() =
        let quote = json.IndexOf('"', index)

        if quote < 0 then
            this.Fail "reached the end of the input inside a field name"

        index <- quote + 1
        this.Expect ':'
        this.SkipValue()

    member this.SkipValue() =
        this.SkipWhitespace()

        match this.Peek() with
        | 'n' -> this.ReadNull()
        | 't'
        | 'f' -> this.ReadBool() |> ignore
        | '"' -> this.ReadString() |> ignore
        | '[' -> this.SkipArray()
        | '{' -> this.SkipObject()
        | c when isDigit (int c) || c = '-' -> this.ReadFiniteNumber() |> ignore
        | _ -> this.Fail "expected a value"

    member this.SkipObject() =
        this.Expect '{'
        let mutable first = true

        while not (this.ReadUntil '}') do
            if first then first <- false else this.Expect ','
            this.Expect '"'
            this.SkipObjectField()

    member this.SkipArray() =
        this.Expect '['
        let mutable first = true

        while not (this.ReadUntil ']') do
            if first then first <- false else this.Expect ','
            this.SkipValue()

    member this.GetHint() =
        let hintLength = 20
        let before = json.Substring(max 0 (index - hintLength), min hintLength index)

        let after = json.Substring(index, min hintLength (json.Length - index))

        let prefix = if index > hintLength then "..." else "<start>"
        let suffix = if json.Length - index > hintLength then "..." else "<eof>"
        let marker = String.replicate (prefix.Length + before.Length) " " + "^"

        $"[json reader; length={json.Length}; index={index}]\n{prefix}{before}{after}{suffix}\n{marker}"
#endif

#if FABLE_COMPILER
/// The key of a type in the generated codec table. A `Type` would work as a key, but
/// Fable hashes it structurally on every lookup, which ends up being expensive for simple
/// types. Generic and anonymous record types get a composed name, since their `FullName`
/// is folded at compile time into a form that differs from the runtime one, or is empty.
module TypeKey =
    let rec ofType (t: Type) : string =
        let generics = t.GetGenericArguments()

        if generics.Length = 0 && not t.IsArray then
            let full = t.FullName

            if full <> "" then
                full
            else
                let fields =
                    FSharp.Reflection.FSharpType.GetRecordFields t
                    |> Array.map (fun f -> f.Name + ":" + ofType f.PropertyType)
                    |> String.concat ";"

                "{|" + fields + "|}"
        elif t.IsArray then
            ofType (t.GetElementType()) + "[]"
        else
            t.Namespace
            + "."
            + t.Name
            + "["
            + (generics |> Array.map ofType |> String.concat ",")
            + "]"

/// Conversions between JSON-shaped JavaScript values and F# primitives.
module JsValue =
    let fail (expected: string) (value: obj) : 'a =
        failwith ("expected " + expected + ", got " + Platform.jsString value)

    let inline toBool (v: obj) : bool =
        if Platform.typeOf v = "boolean" then
            unbox v
        else
            fail "a boolean" v

    let inline toInt32 (v: obj) : int =
        if Platform.typeOf v = "number" then
            unbox v
        else
            fail "a number" v

    let toFloat (v: obj) : float =
        if Platform.typeOf v = "number" then
            unbox v
        elif Platform.isNullish v then
            Double.NaN
        else
            match Platform.typeOf v, unbox<string> v with
            | "string", "NaN" -> Double.NaN
            | "string", "Infinity" -> Double.PositiveInfinity
            | "string", "-Infinity" -> Double.NegativeInfinity
            | _ -> fail "a number" v

    let inline toString (v: obj) : string =
        if Platform.typeOf v = "string" then
            unbox v
        else
            fail "a string" v

    let inline toChar (v: obj) : char =
        let s = toString v

        if s.Length = 1 then
            unbox s
        else
            fail "a single character" v

    let toInt64 (v: obj) : int64 =
        let t = Platform.typeOf v

        if t = "string" || t = "number" then
            Platform.toInt64 v
        else
            fail "a 64-bit integer" v

    let toUInt64 (v: obj) : uint64 =
        let t = Platform.typeOf v

        if t = "string" || t = "number" then
            Platform.toUInt64 v
        else
            fail "a 64-bit integer" v

    let inline toDateTime (v: obj) : DateTime = DateText.parse (toString v)
    let inline toDateTimeNative (v: obj) : DateTime = DateText.parseNative (toString v)
    let inline toGuid (v: obj) : Guid = Guid.Parse(toString v)
    let inline toTimeSpan (v: obj) : TimeSpan = TimeSpan.Parse(toString v)
    let inline toDateOnly (v: obj) : DateOnly = DateOnly.Parse(toString v)
    let inline toTimeOnly (v: obj) : TimeOnly = TimeOnly.Parse(toString v)
    let inline toDateTimeOffset (v: obj) : DateTimeOffset = DateTimeOffset.Parse(toString v)

    let toDecimal (v: obj) : decimal =
        let t = Platform.typeOf v

        if t = "string" || t = "number" then
            Decimal.Parse(Platform.jsString v)
        else
            fail "a decimal" v

    let inline toArray (v: obj) : obj[] =
        if Platform.isArray v then unbox v else fail "an array" v

    let inline toObject (v: obj) : obj =
        if
            not (Platform.isNullish v)
            && Platform.typeOf v = "object"
            && not (Platform.isArray v)
        then
            v
        else
            fail "an object" v

    let missing (field: string) (typeName: string) : 'a =
        failwith ("missing required field " + field + " of " + typeName)

    let inline ofDateTime (d: DateTime) : obj = box (DateText.format d)
    let inline ofDateTimeNative (d: DateTime) : obj = box (DateText.formatNative d)
    let inline ofInt64 (v: int64) : obj = box (Platform.jsString v)
    let inline ofUInt64 (v: uint64) : obj = box (Platform.jsString v)
    let inline ofGuid (g: Guid) : obj = box (string g)
    let inline ofTimeSpan (t: TimeSpan) : obj = box (string t)

    let inline ofDateOnly (d: DateOnly) : obj =
        box (d.ToString("O", System.Globalization.CultureInfo.InvariantCulture))

    let inline ofTimeOnly (t: TimeOnly) : obj =
        box (t.ToString("O", System.Globalization.CultureInfo.InvariantCulture))

    let inline ofDateTimeOffset (d: DateTimeOffset) : obj =
        box (d.ToString("O", System.Globalization.CultureInfo.InvariantCulture))

    let inline ofDecimal (d: decimal) : obj = box (string d)
    let inline ofChar (c: char) : obj = box (string c)
#endif

#if FABLE_COMPILER
type ICodec =
    abstract ToJsObj: value: obj -> obj
    abstract FromJsObj: js: obj -> obj

type ICodec<'T> =
    inherit ICodec
    abstract ToJs: value: 'T -> obj
    abstract FromJs: js: obj -> 'T

type Codec<'T>(toJs: Func<'T, obj>, fromJs: Func<obj, 'T>) =
    interface ICodec<'T> with
        member _.ToJs(value) = toJs.Invoke(value)
        member _.FromJs(js) = fromJs.Invoke(js)
        member _.ToJsObj(value) = toJs.Invoke(unbox value)
        member _.FromJsObj(js) = box (fromJs.Invoke(js))
#else
type ICodec =
    abstract SerializeObj: value: obj * writer: JsonWriter -> unit
    abstract DeserializeObj: reader: JsonReader -> obj

type ICodec<'T> =
    inherit ICodec
    abstract Serialize: value: 'T * writer: JsonWriter -> unit
    abstract Deserialize: reader: JsonReader -> 'T

type Codec<'T>(serialize: Func<'T, JsonWriter, unit>, deserialize: Func<JsonReader, 'T>) =
    interface ICodec<'T> with
        member _.Serialize(value, writer) = serialize.Invoke(value, writer)
        member _.Deserialize(reader) = deserialize.Invoke(reader)
        member _.SerializeObj(value, writer) = serialize.Invoke(unbox value, writer)
        member _.DeserializeObj(reader) = box (deserialize.Invoke(reader))
#endif

/// Tracks which fields a generated deserializer has seen, for records with more than
/// 32 fields. Bits of fields that may be missing start out set.
type BitSet(values: uint32 array) =
    member this.Set(index: int) =
        values[index / 32] <- values[index / 32] ||| (1u <<< (index % 32))

    member this.AnyFalse() =
        values |> Array.exists (fun v -> v <> 0xffffffffu)

module Json =
#if FABLE_COMPILER
    let private codecs = Collections.Generic.Dictionary<string, ICodec>()

    /// Used to register codecs. Do not call manually. Call the `Register()` method on the generated code instead.
    let register (typ: Type) (codec: ICodec) : unit = codecs[TypeKey.ofType typ] <- codec

    /// The codec for a runtime type, for callers that only have a System.Type.
    let codecFor (typ: Type) : ICodec =
        match codecs.TryGetValue(TypeKey.ofType typ) with
        | true, codec -> codec
        | _ -> failwith ("no JSON codec is registered for type " + typ.FullName)

    let inline codecOf<'T> () : ICodec<'T> = codecFor typeof<'T> :?> ICodec<'T>

    /// Serialize a value of a concrete type `'T` to a JSON string.
    let inline serialize<'T> (value: 'T) : string =
        let codec = codecOf<'T> ()
        Platform.jsonStringify (codec.ToJs value)

    /// Deserialize a JSON string into a value of type `'T`.
    let inline deserialize<'T> (json: string) : 'T =
        let codec = codecOf<'T> ()
        codec.FromJs(Platform.jsonParse json)

    /// Serialize a value of the given `Type` to a JSON string.
    let serializeObj (typ: Type) (value: obj) : string =
        let codec = codecFor typ
        Platform.jsonStringify (codec.ToJsObj value)

    /// Deserialize a JSON string into a value of type `Type`.
    let deserializeObj (typ: Type) (json: string) : obj =
        let codec = codecFor typ
        codec.FromJsObj(Platform.jsonParse json)
#else
    let private codecs = Collections.Generic.Dictionary<Type, ICodec>()

    /// Used to register codecs. Do not call manually. Call the `Register()` method on the generated code instead.
    let register (typ: Type) (codec: ICodec) : unit = codecs[typ] <- codec

    /// The codec for a runtime type, for callers that only have a System.Type.
    let codecFor (typ: Type) : ICodec =
        match codecs.TryGetValue typ with
        | true, codec -> codec
        | _ -> failwith ("no JSON codec is registered for type " + typ.FullName)

    type CodecCache<'T>() =
        static member val Codec = codecFor typeof<'T> :?> ICodec<'T> with get

    let inline codecOf<'T> () : ICodec<'T> = CodecCache<'T>.Codec

    /// Serialize a value of a concrete type `'T` to a JSON string.
    let inline serialize<'T> (value: 'T) : string =
        let writer = JsonWriter()
        let codec = codecOf<'T> ()
        codec.Serialize(value, writer)
        writer.ToString()

    /// Deserialize a JSON string into a value of type `'T`.
    let inline deserialize<'T> (json: string) : 'T =
        let codec = codecOf<'T> ()
        codec.Deserialize(JsonReader json)

    /// Serialize a value of the given `Type` to a JSON string.
    let serializeObj (typ: Type) (value: obj) : string =
        let writer = JsonWriter()
        let codec = codecFor typ
        codec.SerializeObj(value, writer)
        writer.ToString()

    /// Deserialize a JSON string into a value of type `Type`.
    let deserializeObj (typ: Type) (json: string) : obj =
        let codec = codecFor typ
        codec.DeserializeObj(JsonReader json)
#endif
