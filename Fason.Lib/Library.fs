namespace Fason

open System
open System.IO

type FasonSerializableAttribute() =
    inherit Attribute()

type JsonWriter() =
#if FABLE_COMPILER
    let mutable json = ""
#else
    let writer = new StringWriter()
#endif

#if FABLE_COMPILER
    // We emit a regular JS string concatenation here as it's more efficient.
    [<Fable.Core.Emit("$0 += $1")>]
    static member inline concat(target: string, value: string) = Fable.Core.Util.jsNative
#endif

    member inline private this.WriteInternal(value: string) =
#if FABLE_COMPILER
        JsonWriter.concat (json, value)
#else
        writer.Write(value)
#endif

    member this.WritePlain(value: string) = this.WriteInternal(value)

    member this.Write(value: string) =
        this.WriteInternal("\"")

        value.ToCharArray()
        |> Array.iter (function
            | '"' -> this.WriteInternal("\\\"")
            | '\\' -> this.WriteInternal("\\\\")
            | '/' -> this.WriteInternal("\\/")
            | '\b' -> this.WriteInternal("\\b")
            | '\f' -> this.WriteInternal("\\f")
            | '\n' -> this.WriteInternal("\\n")
            | '\r' -> this.WriteInternal("\\r")
            | '\t' -> this.WriteInternal("\\t")
            | c when c >= '\u0000' && c <= '\u001F' -> this.WriteInternal($"\\u{uint16 c:X4}")
            | c -> this.WriteInternal(c.ToString()))

        this.WriteInternal("\"")

    member this.Write(value: bool) =
        if value then
            this.WriteInternal("true")
        else
            this.WriteInternal("false")

    member this.Write(value: byte) = this.WriteInternal(value.ToString())
    member this.Write(value: sbyte) = this.WriteInternal(value.ToString())

    member this.Write(value: char) =
        this.WriteInternal("\"")
        this.WriteInternal(value.ToString())
        this.WriteInternal("\"")

    member this.Write(value: int16) = this.WriteInternal(value.ToString())
    member this.Write(value: int32) = this.WriteInternal(value.ToString())
    member this.Write(value: int64) = this.WriteInternal(value.ToString())
    member this.Write(value: uint16) = this.WriteInternal(value.ToString())
    member this.Write(value: uint32) = this.WriteInternal(value.ToString())
    member this.Write(value: uint64) = this.WriteInternal(value.ToString())
    member this.Write(value: single) = this.WriteInternal(value.ToString())
    member this.Write(value: double) = this.WriteInternal(value.ToString())

    member this.Write(value: Guid) =
        this.WriteInternal("\"")
        this.WriteInternal(value.ToString("N"))
        this.WriteInternal("\"")

    member this.Write(value: DateTime) =
        this.WriteInternal("\"")
        this.WriteInternal(value.ToString("O"))
        this.WriteInternal("\"")

    member this.Write(value: TimeSpan) =
        this.WriteInternal("\"")
        this.WriteInternal(value.ToString())
        this.WriteInternal("\"")

    override this.ToString() =
#if FABLE_COMPILER
        json
#else
        writer.Flush()
        writer.ToString()
#endif

    interface IDisposable with
        member this.Dispose() =
#if FABLE_COMPILER
            // No-op for Fable
            ()
#else
            writer.Dispose()
#endif

type JsonReader(json: string) =
    let mutable index = 0

    member this.Read() =
        if index >= json.Length then
            None
        else
            let c = json[index]
            index <- index + 1
            //printfn $"Read character '{c}'"
            Some c

    member this.ReadCount(count: int) =
        if index + count > json.Length then
            None
        else
            let value = json.Substring(index, count)
            index <- index + count
            //printfn $"Read {count} characters '{value}'"
            Some value

    member this.Peek() =
        if index >= json.Length then None else Some json[index]

    member this.PeekCount(count: int) =
        if index + count > json.Length then
            None
        else
            Some(json.Substring(index, count))

    member this.Skip() =
        //printfn $"Skipping character '{this.Peek()}'"
        index <- index + 1

    member this.SkipCount(count: int) =
        //printfn $"Skipping {count} characters '{this.PeekCount(count)}'"
        index <- index + count

    member this.StartsWith(value: string) =
        //printfn $"Checking if reader starts with '{value}'"

        if index + value.Length > json.Length then
            //printfn "No, not enough characters left"
            false
        else
            let matches = json.Substring(index, value.Length) = value
            //printfn $"Result: {matches}"
            matches

    member this.SkipWhitespace() =
        let isWhitespace c =
            c = ' ' || c = '\t' || c = '\n' || c = '\r'

        let prevIndex = index

        while this.Peek() |> Option.exists isWhitespace do
            this.Skip()

        //let skipped = index - prevIndex
        //printfn $"Skipped {skipped} whitespace characters."

    /// Ignores all whitespace until it finds a non-whitespace character.
    /// If the following characters match the provided value, returns true.
    member this.ReadUntil(value: string) =
        this.SkipWhitespace()
        let found = this.StartsWith(value)

        if found then
            this.SkipCount(value.Length)

        found

    member this.ReadBool() =
        this.SkipWhitespace()

        match this.Peek() with
        | Some 't' ->
            let read = this.ReadCount(4)

            if read <> Some "true" then
                failwith $"expected true or false, got '{read}'"

            true

        | Some 'f' ->
            let read = this.ReadCount(5)

            if read <> Some "false" then
                failwith $"expected true or false, got '{read}'"

            false

        | Some c -> failwith $"expected true or false, got something starting with '{c}'"
        | None -> failwith "expected true or false, got end of string"

    member private this.ReadInteger<'a>(allowNegative: bool) =
        this.SkipWhitespace()

        let negative =
            if allowNegative then
                let negative = this.Peek() = Some '-'

                if negative then
                    this.Skip()

                negative
            else
                false

        let mutable value = 0UL
        let mutable digitsRead = 0
        let mutable finished = false

        // Read digits one by one.
        while not finished do
            match this.Peek() with
            | Some c when c >= '0' && c <= '9' ->
                this.Skip()
                value <- (value * 10UL) + (uint64 c - uint64 '0')
                digitsRead <- digitsRead + 1
            | _ -> finished <- true

        if digitsRead = 0 then
            failwith $"expected a digit, got '{this.Peek()}'"

        let finalValue =
            if negative then
                let value = -int64 value
                Convert.ChangeType(value, typeof<'a>) :?> 'a
            else
                Convert.ChangeType(value, typeof<'a>) :?> 'a

        finalValue

    member private this.ReadRealNumber() =
        this.SkipWhitespace()
        let mutable value = ""

        // Allow an optional negative sign.
        if this.Peek() = Some '-' then
            value <- value + "-"
            this.Skip()

        // Read digits one by one.
        let mutable readingDigits = true

        while readingDigits do
            match this.Peek() with
            | Some c when c >= '0' && c <= '9' ->
                value <- value + string c
                this.Skip()
            | _ -> readingDigits <- false

        // Read the decimal point and digits after it.
        if this.Peek() = Some '.' then
            value <- value + "."
            this.Skip()

            let mutable readingDigits = true

            while readingDigits do
                match this.Peek() with
                | Some c when c >= '0' && c <= '9' ->
                    value <- value + string c
                    this.Skip()
                | _ -> readingDigits <- false

        // Read the exponent, if there is one.
        if this.Peek() = Some 'e' || this.Peek() = Some 'E' then
            value <- value + "e"
            this.Skip()

            value <- value + (this.ReadInteger<int> true).ToString()

        value

    member this.ReadByte() = this.ReadInteger<byte>(false)
    member this.ReadSByte() = this.ReadInteger<sbyte>(true)
    member this.ReadInt8() = this.ReadInteger<int8>(true)
    member this.ReadInt16() = this.ReadInteger<int16>(true)
    member this.ReadInt32() = this.ReadInteger<int32>(true)
    member this.ReadInt64() = this.ReadInteger<int64>(true)
    member this.ReadUInt8() = this.ReadInteger<uint8>(false)
    member this.ReadUInt16() = this.ReadInteger<uint16>(false)
    member this.ReadUInt32() = this.ReadInteger<uint32>(false)
    member this.ReadUInt64() = this.ReadInteger<uint64>(false)

    member this.ReadSingle() =
        let str = this.ReadRealNumber()

        if str.Length = 0 then
            failwith $"expected a number, got '{this.Peek()}'"

        match Single.TryParse str with
        | false, _ -> failwith $"expected a number, got '{str}'"
        | true, value -> value

    member this.ReadDouble() =
        let str = this.ReadRealNumber()

        if str.Length = 0 then
            failwith $"expected a number, got '{this.Peek()}'"

        match Double.TryParse str with
        | false, _ -> failwith $"expected a number, got '{str}'"
        | true, value -> value

    member this.ReadString() =
        if this.ReadUntil("\"") |> not then
            failwith $"expected a string opening quote, got '{this.Peek()}'"

        let mutable value = ""

        while this.Peek() <> Some '"' do
            match this.Peek() with
            | Some '\\' ->
                this.Skip()

                match this.Peek() with
                | Some '"' -> value <- value + "\""
                | Some '\\' -> value <- value + "\\"
                | Some '/' -> value <- value + "/"
                | Some 'b' -> value <- value + "\b"
                | Some 'f' -> value <- value + "\f"
                | Some 'n' -> value <- value + "\n"
                | Some 'r' -> value <- value + "\r"
                | Some 't' -> value <- value + "\t"
                | Some 'x' ->
                    this.Skip()

                    match this.ReadCount(2) with
                    | Some hex when hex.Length = 2 ->
                        // Check if we have hex digits.
                        let isHex c =
                            c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F'

                        if hex |> Seq.forall isHex then
                            value <-
                                value
                                + Char.ConvertFromUtf32(Int32.Parse(hex, System.Globalization.NumberStyles.HexNumber))
                        else
                            failwith $"expected a unicode escape sequence, got '\\u{hex}'"

                | Some 'u' ->
                    this.Skip()

                    match this.ReadCount(4) with
                    | Some hex when hex.Length = 4 ->
                        // Check if we have hex digits.
                        let isHex c =
                            c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F'

                        if hex |> Seq.forall isHex then
                            value <-
                                value
                                + Char.ConvertFromUtf32(Int32.Parse(hex, System.Globalization.NumberStyles.HexNumber))
                        else
                            failwith $"expected a unicode escape sequence, got '\\u{hex}'"

                    | Some hex -> failwith $"expected a unicode escape sequence, got '\\u{hex}'"

                    | None -> failwith "expected a unicode escape sequence, an empty `\\u` was found instead"

                | Some c -> failwith $"unknown escape sequence '\\{c}'"
                | None -> failwith "got an incomplete escape sequence"

            | Some c ->
                this.Skip()
                value <- value + string c
            | None -> failwith "reached the end of the stream with an unclosed string"

        if this.Peek() <> Some '"' then
            failwith "expected a closing quote, but reached the end of the stream instead"

        this.Skip()

        value

    member this.ReadGuid() = Guid.Parse(this.ReadString())
    member this.ReadDateTime() = DateTime.Parse(this.ReadString())
    member this.ReadTimeSpan() = TimeSpan.Parse(this.ReadString())

    member this.ReadChar() =
        let str = this.ReadString()

        if str.Length <> 1 then
            failwith $"expected a character, got a string of length {str.Length}"

        str[0]


    member this.SkipObjectField() =
        // This function is called when we encounter a field that we don't know how to parse.
        // Keep reading the name of the field, until we find a closing quote.
        while this.Peek().IsSome && this.Peek() <> Some '"' do
            this.Skip()

        // Check if we've reached EOF.
        if this.Peek() = None then
            failwith "reached the end of the stream while skipping an object field"

        // Skip the closing quote.
        this.Skip()

        if this.ReadUntil(":") |> not then
            failwith "expected a colon after the field name, but got something else"

        this.SkipValue()

    member this.ReadNull() =
        if this.ReadUntil("null") |> not then
            failwith "expected null, but got something else"

        None

    member this.ReadTrue() =
        if this.ReadUntil("true") |> not then
            failwith "expected true, but got something else"

        true

    member this.ReadFalse() =
        if this.ReadUntil("false") |> not then
            failwith "expected false, but got something else"

        false

    member this.SkipValue() =
        match this.Peek() with
        | Some 'n' -> this.ReadNull() |> ignore
        | Some 't' -> this.ReadTrue() |> ignore
        | Some 'f' -> this.ReadFalse() |> ignore
        | Some '"' -> this.ReadString() |> ignore
        | Some c when c >= '0' && c <= '9' || c = '-' -> this.ReadDouble() |> ignore
        | Some '[' -> this.SkipArray()
        | Some '{' -> this.SkipObject()
        | Some c -> failwith $"expected a value, got '{c}'"
        | None -> failwith "expected a value, but reached the end of the stream instead"

    member this.SkipObject() =
        if this.ReadUntil("{") |> not then
            failwith $"expected an object opening brace, but instead got '{this.Peek()}'"

        let mutable hasSeenField = false

        while this.Peek().IsSome && this.Peek() <> Some '}' do
            if hasSeenField then
                if this.ReadUntil(",") |> not then
                    failwith "expected a comma after the field value, but got something else"

            this.SkipObjectField()
            this.SkipWhitespace()
            hasSeenField <- true

        if this.Peek() <> Some '}' then
            failwith "expected an object closing brace, but got something else"

        this.Skip()
        this.SkipWhitespace()

    member this.SkipArray() =
        if this.ReadUntil("[") |> not then
            failwith $"expected an array opening brace, but instead got '{this.Peek()}'"

        let mutable hasSeenValue = false

        while this.Peek().IsSome && this.Peek() <> Some ']' do
            if hasSeenValue then
                if this.ReadUntil(",") |> not then
                    failwith "expected a comma after the array value, but got something else"

            this.SkipValue()
            this.SkipWhitespace()
            hasSeenValue <- true

        if this.Peek() <> Some ']' then
            failwith "expected an array closing brace, but got something else"

        this.Skip()
        this.SkipWhitespace()

    member this.GetHint() =
        let isAtEof = index >= json.Length

        // Get 10 characters before and after the current position (if possible)
        let hintLength = 20
        let before = json.Substring(max 0 (index - hintLength), min hintLength index)

        let after =
            if isAtEof then
                ""
            else
                json.Substring(index, min hintLength (json.Length - index))

        let hintText =
            match before.Length < hintLength, after.Length < hintLength with
            | true, true -> $"<start>" + before + after + "<eof>"
            | true, false -> $"<start>" + before + after + "..."
            | false, true -> "..." + before + after + "<eof>"
            | false, false -> "..." + before + after + "..."

        // Find the index of the current character (based on index) in the hintText
        let indexLocation =
            let offset =
                if before.Length < 10 then
                    "<start>".Length
                else
                    "...".Length

            before.Length + offset

        let secondLine =
            Seq.init (indexLocation + 1) (fun i -> if i = indexLocation then "^" else " ")
            |> String.concat ""

        let envText =
            $"[json reader; length={json.Length}; index={index}; isAtEof={isAtEof}; peek={this.Peek()}]"

        $"{envText}\n{hintText}\n{secondLine}"

type BitSet(values: uint32 array) =
    let mutable values = values

    new(capacity: int, ?defaultValue: bool) =
        // Calculate the number of 32-bit values needed to store the given number of bits.
        // CAVEAT: resulting bitarray will often be larger in actual capacity than requested.
        let valueCount = (capacity + 31) / 32
        let value = if defaultValue = Some true then 0xffffffffu else 0u
        BitSet(Array.create valueCount value)

    static member fromBoolSeq (defaultValue: bool) (values: bool array) =
        let bitset = BitSet(values.Length, defaultValue = defaultValue)
        values |> Array.iteri (fun i v -> bitset.Set(i, v))
        bitset

    member val Length = values.Length * 32 with get, set

    /// Get the value of the bit at the given index.
    member this.Get(index: int) =
        (values[index / 32] &&& (1u <<< (index % 32))) <> 0u

    /// Set the value of the bit at the given index.
    member this.Set(index: int, value: bool) =
        values[index / 32] <-
            if value then
                values[index / 32] ||| (1u <<< (index % 32))
            else
                values[index / 32] &&& ~~~(1u <<< (index % 32))

    /// Get the underlying array of 32-bit values.
    member this.ToArray() = values

    /// Check if all bits are set to true.
    member this.AllTrue() =
        values |> Array.forall (fun v -> v = 0xffffffffu)

    /// Check if all bits are set to false.
    member this.AllFalse() =
        values |> Array.forall (fun v -> v = 0u)

    /// Check if any bits are set to true.
    member this.AnyTrue() =
        values |> Array.exists (fun v -> v <> 0u)

    /// Check if any bits are set to false.
    member this.AnyFalse() =
        values |> Array.exists (fun v -> v <> 0xffffffffu)

    /// Get or set the value of the bit at the given index.
    member this.Item
        with get index = this.Get index
        and set index value = this.Set(index, value)

    override this.Equals(obj) =
        obj :? BitSet && (obj :?> BitSet).ToArray() = this.ToArray()

    override this.GetHashCode() = this.ToArray().GetHashCode()
