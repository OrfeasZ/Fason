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
