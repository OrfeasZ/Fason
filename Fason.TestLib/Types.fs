namespace Fason.TestLib

open System
open Fason
open FSharp.UMX
open FSharp.Data.UnitSystems.SI.UnitSymbols

module OtherModule =
    type OtherRecord = { first: bool; second: string }

[<FasonSerializable>]
module TestModule =
    type TestRecordSimple = { a: int; b: string }

    type TestRecord =
        { a: int
          b: string
          c: string list
          d: uint option
          e: float list option
          f: string array
          g: Map<string, int>
          h: string array option
          i: string option array list option
          j: string option option
          k: string option option option
          l: Set<char> }

        member this.x = $"{this.a} {this.b}"
        static member val y = "dong"
        static member z = 42

    type TestAllBasicTypes =
        { a: bool
          b: byte
          c: sbyte
          d: char
          e: int8
          f: int16
          g: int32
          h: int64
          i: uint8
          j: uint16
          k: uint32
          l: uint64
          m: single
          n: double
          o: string
          p: Guid
          q: DateTime
          r: TimeSpan }

    type TestAnonymousRecord = {| a: int; b: string |}

    type TestTuple =
        int *
        string *
        string list *
        uint option *
        float list option *
        string array *
        Map<string, int> *
        string array option *
        string option array list option *
        string option option *
        string option option option

    [<Measure>]
    type strMeasure

    type TestNestedRecord =
        { x: int
          y: string
          c: TestRecordSimple
          d: OtherModule.OtherRecord }

    type TestRecordWithMeasure =
        { kg: float<kg>
          str: string<strMeasure> }

    type TestUnion =
        | A
        | B of int
        | C of int * string
        | D of TestRecord
        | E of x: int * y: string

        member self.name = self.ToString()

    type TestEnumUint64 =
        | Black = 0UL
        | White = 1UL
        | Gray = 2UL

    type TestEnum =
        | Red = 0
        | Green = 1
        | Blue = 2

module TestModule2 =
    type TestModule.TestUnion with
        member self.otherName = self.ToString()
