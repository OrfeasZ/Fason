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

    type TestAdvanced =
        { r: TestRecord
          nr: TestNestedRecord
          mr: TestRecordWithMeasure
          u: TestUnion
          ul: TestUnion list
          e: TestEnum
          el: TestEnum list
          e64: TestEnumUint64
          t: TestTuple }

    type Weird =
        {| small: int
           smaller: int
           smile: int option
           address: int
           ad: int
           banana: int
           sad: int |}

    /// A type that contains itself.
    type TestRecursive =
        { name: string
          children: TestRecursive list }

    /// A generic record, instantiated more than once and with a unit of measure.
    type TestGeneric<'a> = { value: 'a; other: 'a option }

    type TestGenericUnion<'a> =
        | Nothing
        | Just of 'a

    /// A union case named like its type.
    type TestSameName = TestSameName of uint64 * string

    /// Units of measure nested in collections, options and generics.
    type TestUomNested =
        { ids: string<strMeasure> list
          maybeId: string<strMeasure> option
          weights: Map<string<strMeasure>, float<kg>>
          generic: TestGeneric<string<strMeasure>>
          pair: string<strMeasure> * float<kg> }

    /// Tuples inside collections, and as map keys.
    type TestTupleNested =
        { pairs: (int * string) list
          byPair: Map<(int * string), TestSameName> }

    type TestGenerics =
        { ints: TestGeneric<int>
          strings: TestGeneric<string>
          nested: TestGeneric<TestGeneric<bool>>
          result: Result<int, string>
          results: Result<TestRecordSimple, string list> list
          maybe: TestGenericUnion<int>
          maybeString: TestGenericUnion<string<strMeasure>> }

    type TestUnit = { nothing: unit; list: unit list }

    type TestDate = { at: DateTime }

    /// Stands for its argument when collecting, like an async result does by default.
    [<FasonUnwrap>]
    type Deferred<'T>() = class end

    type TestActionResult = { ok: bool }
    type TestCurriedArg = { arg: int }
    type TestCurriedResult = { result: string }
    type TestActionError = { reason: string }

    /// Only reachable through the interface, whose members' types are collected.
    [<FasonSerializable>]
    type TestActions =
        abstract member Ping: id: int -> Deferred<TestActionResult list>
        abstract member Pong: unit -> Async<Result<TestActionResult, TestActionError>>

        abstract member Curried:
            first: TestActionResult -> second: TestCurriedArg -> System.Threading.Tasks.Task<TestCurriedResult>

    type TestStrings =
        { plain: string
          escapes: string
          control: string
          unicode: string
          empty: string }

    /// More than 32 fields, so the deserializer uses a BitSet instead of a mask.
    type TestWide =
        { f00: int
          f01: int
          f02: int
          f03: int
          f04: int
          f05: int
          f06: int
          f07: int
          f08: int
          f09: int
          f10: int
          f11: int
          f12: int
          f13: int
          f14: int
          f15: int
          f16: int
          f17: int
          f18: int
          f19: int
          f20: int
          f21: int
          f22: int
          f23: int
          f24: int
          f25: int
          f26: int
          f27: int
          f28: int
          f29: int
          f30: int
          f31: int
          f32: int
          f33: int option
          f34: int }

    /// Gets no codec: a class field is unsupported.
    type TestUnsupported =
        { name: string
          stream: System.IO.Stream }

    /// Gets no codec either, since it depends on TestUnsupported.
    type TestDependsOnUnsupported = { inner: TestUnsupported; ok: bool }

module TestModule2 =
    type TestModule.TestUnion with
        member self.otherName = self.ToString()
