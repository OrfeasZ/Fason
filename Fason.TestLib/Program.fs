module Fason.TestLib.Program

open System
open Fason.TestLib.TestModule
open Fason.TestLib.Serialization
open FSharp.UMX
open FSharp.Data.UnitSystems.SI.UnitSymbols
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let mutable failures = 0

let check (name: string) (condition: bool) (detail: string) =
    if condition then
        printfn $"ok    {name}"
    else
        failures <- failures + 1
        eprintfn $"FAIL  {name}\n      {detail}"

/// Serializes and deserializes through both the typed and the obj/Type entry points.
let inline roundTrip<'T when 'T: equality> (name: string) (original: 'T) =
    try
        let json = Json.serialize original
        let parsed = Json.deserialize<'T> json
        check $"{name} (typed)" (parsed = original) $"json: {json}\n      parsed: %A{parsed}"

        let jsonObj = Json.serializeObj (box original, typeof<'T>)
        let parsedObj = Json.deserializeObj (jsonObj, typeof<'T>) :?> 'T
        check $"{name} (obj)" (jsonObj = json && parsedObj = original) $"json: {jsonObj}\n      parsed: %A{parsedObj}"
    with ex ->
        check name false $"%A{ex}"

let inline expectJson (name: string) (value: 'T) (expected: string) =
    try
        let json = Json.serialize value
        check name (json = expected) $"expected: {expected}\n      actual:   {json}"
    with ex ->
        check name false $"%A{ex}"

let thothExtra = Extra.empty |> Extra.withInt64 |> Extra.withUInt64

/// The generated JSON must be what Thoth's auto coders produce, and both must read each other's output.
let inline thothCompatible<'T when 'T: equality> (name: string) (value: 'T) =
    try
        let fasonJson = Json.serialize value
        let thothJson = Encode.Auto.toString (0, value, extra = thothExtra)
        check $"{name} (same json as thoth)" (fasonJson = thothJson) $"fason: {fasonJson}\n      thoth: {thothJson}"

        let fromThoth = Json.deserialize<'T> thothJson
        check $"{name} (reads thoth json)" (fromThoth = value) $"parsed: %A{fromThoth}"

        let byThoth = Decode.Auto.unsafeFromString<'T> (fasonJson, extra = thothExtra)
        check $"{name} (thoth reads fason json)" (byThoth = value) $"parsed: %A{byThoth}"
    with ex ->
        check name false $"%A{ex}"

let expectFailure (name: string) (action: unit -> unit) =
    try
        action ()
        check name false "no exception was raised"
    with ex ->
        check name true ""

let testRecord: TestRecord =
    { a = 123
      b = "abc"
      c = [ "hello"; "world" ]
      d = Some 456u
      e = None
      f = [| "big"; "doinks" |]
      g = Map [ "seven", 7; "eight", 8 ]
      h = Some [| "in"; "amish" |]
      i = Some [ [| Some "wow" |] ]
      j = Some(Some "j")
      k = Some(Some(Some "k"))
      l = Set [ 'a'; 'a'; 'b'; 'c' ] }

let testAdvanced: TestAdvanced =
    { r = testRecord
      nr =
        { x = 1
          y = "2"
          c = { a = 3; b = "4" }
          d = { first = true; second = "6" } }
      mr = { kg = 7.0<kg>; str = %"8" }
      u = TestUnion.A
      ul =
        [ TestUnion.A
          TestUnion.B 1
          TestUnion.C(1, "2")
          TestUnion.D testRecord
          TestUnion.E(1, "2") ]
      e = TestEnum.Red
      el = [ TestEnum.Red; TestEnum.Green; TestEnum.Blue ]
      e64 = TestEnumUint64.Black
      t =
        (123,
         "123",
         [ "hello"; "world" ],
         Some 456u,
         None,
         [| "big"; "doinks" |],
         Map [ "seven", 7; "eight", 8 ],
         Some [| "in"; "amish" |],
         Some [ [| Some "wow" |] ],
         Some(Some "j"),
         Some(Some(Some "k"))) }

let testGenerics: TestGenerics =
    { ints = { value = 1; other = Some 2 }
      strings = { value = "a"; other = None }
      nested =
        { value = { value = true; other = None }
          other = Some { value = false; other = Some true } }
      result = Error "bad"
      results = [ Ok { a = 1; b = "x" }; Error [ "y"; "z" ] ]
      maybe = Just 5
      maybeString = Just %"id" }

let testUomNested: TestUomNested =
    { ids = [ %"a"; %"b" ]
      maybeId = Some %"c"
      weights = Map [ %"d", 1.5<kg>; %"e", 2.5<kg> ]
      generic = { value = %"f"; other = Some %"g" }
      pair = %"h", 3.0<kg> }

let testTuplesNested: TestTupleNested =
    { pairs = [ 1, "one"; 2, "two" ]
      byPair = Map [ (1, "one"), TestSameName(1UL, "x"); (2, "two"), TestSameName(2UL, "y") ] }

let testStrings: TestStrings =
    { plain = "hello world"
      escapes = "quote\" backslash\\ slash/ newline\n tab\t return\r"
      control = "\u0001\u001f"
      unicode = "ελληνικά 日本語 😀"
      empty = "" }

[<EntryPoint>]
let realMain argv =
    roundTrip "advanced" testAdvanced

    let basics: TestAllBasicTypes =
        { a = true
          b = 1uy
          c = -2y
          d = 'x'
          e = -3y
          f = -4s
          g = -5
          h = -6L
          i = 7uy
          j = 8us
          k = 9u
          l = 10UL
          m = 1.5f
          n = -2.25
          o = "text"
          p = Guid.NewGuid()
          q = DateTime(2026, 9, 3, 12, 34, 56, DateTimeKind.Utc)
          r = TimeSpan.FromMinutes 90.0 }

    roundTrip "all basic types" basics

    roundTrip "anonymous record" ({| a = 1; b = "b" |}: TestAnonymousRecord)

    roundTrip
        "weird field names"
        ({| small = 1
            smaller = 2
            smile = Some 3
            address = 4
            ad = 5
            banana = 6
            sad = 7 |}
        : Weird)

    let weirdNone: Weird =
        {| small = 1
           smaller = 2
           smile = None
           address = 4
           ad = 5
           banana = 6
           sad = 7 |}

#if FABLE_COMPILER
    // Fable omits the key of a None field from an anonymous record literal but not from one
    // built from variables, so structural equality fails there. Compare through the JSON instead.
    check
        "anonymous record with None field"
        (Json.serialize (Json.deserialize<Weird> (Json.serialize weirdNone)) = Json.serialize weirdNone)
        ""
#else
    roundTrip "anonymous record with None field" weirdNone
#endif

    roundTrip
        "recursive record"
        { name = "root"
          children =
            [ { name = "a"; children = [] }
              { name = "b"
                children = [ { name = "c"; children = [] } ] } ] }

    roundTrip "generic instantiations" testGenerics
    roundTrip "union case named like its type" (TestSameName(42UL, "node"))
    roundTrip "units of measure nested" testUomNested
    roundTrip "tuples nested" testTuplesNested
    roundTrip "unit" { nothing = (); list = [ (); () ] }
    roundTrip "type reached through an interface and an unwrapped wrapper" [ { ok = true }; { ok = false } ]

    roundTrip
        "type reached through an interface and an async result"
        (Error { reason = "no" }: Result<TestActionResult, TestActionError>)

    roundTrip "strings" testStrings

    roundTrip
        "wide record"
        { f00 = 0
          f01 = 1
          f02 = 2
          f03 = 3
          f04 = 4
          f05 = 5
          f06 = 6
          f07 = 7
          f08 = 8
          f09 = 9
          f10 = 10
          f11 = 11
          f12 = 12
          f13 = 13
          f14 = 14
          f15 = 15
          f16 = 16
          f17 = 17
          f18 = 18
          f19 = 19
          f20 = 20
          f21 = 21
          f22 = 22
          f23 = 23
          f24 = 24
          f25 = 25
          f26 = 26
          f27 = 27
          f28 = 28
          f29 = 29
          f30 = 30
          f31 = 31
          f32 = 32
          f33 = None
          f34 = 34 }

    roundTrip "enum outside declared values" [ enum<TestEnum> 99; TestEnum.Blue ]
    roundTrip "top-level list of unions" [ TestUnion.B 1; TestUnion.C(2, "y") ]
    roundTrip "top-level uom value" (%"plain": string<strMeasure>)
    roundTrip "top-level option" (Some 3)

    expectJson "record json shape" ({ a = 1; b = "x\"y" }: TestRecordSimple) """{"a":1,"b":"x\"y"}"""
    expectJson "union json shape" [ TestUnion.A; TestUnion.C(1, "2") ] """["A",["C",1,"2"]]"""
    expectJson "enum json shape" [ TestEnum.Green; enum<TestEnum> 99 ] """[1,99]"""
    expectJson "option json shape" (Some 1u) "1"
    expectJson "none json shape" (None: uint option) "null"
    expectJson "map json shape" (Map [ "a", 1 ]) """{"a":1}"""
    expectJson "none field omitted" ({ value = 1; other = None }: TestGeneric<int>) """{"value":1}"""
    expectJson "int64 quoted" (TestSameName(42UL, "node")) """["TestSameName","42","node"]"""
#if FABLE_COMPILER
    expectJson "whole float" { kg = 7.0<kg>; str = %"8" } """{"kg":7,"str":"8"}"""
#else
    expectJson "whole float" { kg = 7.0<kg>; str = %"8" } """{"kg":7.0,"str":"8"}"""
#endif

    check
        "map object form and pair form both read"
        (Json.deserialize<Map<string, int32>> """{"a":1,"b":2}""" = Json.deserialize<Map<string, int32>>
            """[["a",1],["b",2]]""")
        ""

    check
        "enum name still read"
        (Json.deserialize<TestEnum list> """["Green",2]""" = [ TestEnum.Green; TestEnum.Blue ])
        ""

    thothCompatible "thoth: record" testRecord
    thothCompatible "thoth: basic types" basics

    thothCompatible
        "thoth: unions"
        [ TestUnion.A
          TestUnion.B 1
          TestUnion.C(1, "2")
          TestUnion.D testRecord
          TestUnion.E(1, "2") ]

    thothCompatible "thoth: enums" [ TestEnum.Red; TestEnum.Blue ]
    thothCompatible "thoth: nested" testAdvanced.nr
    thothCompatible "thoth: measure" testAdvanced.mr
    thothCompatible "thoth: tuple" testAdvanced.t

    thothCompatible "thoth: generics" testGenerics
    thothCompatible "thoth: uom nested" testUomNested
    thothCompatible "thoth: tuples nested" testTuplesNested
    thothCompatible "thoth: strings" testStrings

    expectFailure "type outside the model fails" (fun () -> Json.serialize [ Some 1 ] |> ignore)

    check
        "unknown fields are skipped and whitespace is allowed"
        (Json.deserialize<TestRecordSimple> """ { "zzz" : [1, {"q": null}], "b" : "x" , "a" : 5 , "ab": true } """ = { a =
                                                                                                                         5
                                                                                                                       b =
                                                                                                                         "x" })
        ""

    check
        "missing optional field reads as None"
        (Json.deserialize<TestGeneric<int>> """{"value": 1}""" = { value = 1; other = None })
        ""

    expectFailure "missing required field fails" (fun () -> Json.deserialize<TestRecordSimple> """{"a": 1}""" |> ignore)
    expectFailure "unknown union tag fails" (fun () -> Json.deserialize<TestUnion> "\"Q\"" |> ignore)
    expectFailure "unregistered type fails" (fun () -> Json.serializeObj (box 1uy, typeof<decimal>) |> ignore)

#if !FABLE_COMPILER
    // The streaming reader checks integer ranges. The JavaScript path takes what JSON.parse gives it.
    check
        "int32 boundaries read"
        (Json.deserialize<TestRecordSimple> """{"a":-2147483648,"b":""}""" = { a = Int32.MinValue; b = "" })
        ""

    expectFailure "int32 overflow fails" (fun () ->
        Json.deserialize<TestRecordSimple> """{"a":2147483648,"b":""}""" |> ignore)

    expectFailure "int64 overflow fails" (fun () ->
        Json.deserialize<TestSameName> """["TestSameName","18446744073709551616","x"]"""
        |> ignore)
#endif

    // Dates: every kind is written the way Thoth writes it and read back as the same UTC instant.
    let utcDate = DateTime(2026, 9, 3, 12, 34, 56, 789, DateTimeKind.Utc)

    for kindName, date in
        [ "utc", utcDate
          "local", DateTime(2026, 9, 3, 12, 34, 56, 789, DateTimeKind.Local)
          "unspecified", DateTime(2026, 9, 3, 12, 34, 56, 789, DateTimeKind.Unspecified)
          "early", DateTime(999, 1, 2, 3, 4, 5, DateTimeKind.Utc)
          "local midnight", DateTime(2026, 1, 1, 0, 0, 0, DateTimeKind.Local) ] do
        let original: TestDate = { at = date }
        let instant = date.ToUniversalTime()
        let fasonJson = Json.serialize original
        let thothJson = Encode.Auto.toString (0, original)

        check
            $"{kindName} date: same json as thoth"
            (fasonJson = thothJson)
            $"fason: {fasonJson}\n      thoth: {thothJson}"

        // Readers return UTC, so a local original is compared as an instant, not by ticks.
        let parsed = Json.deserialize<TestDate> fasonJson
        let fromThoth = Json.deserialize<TestDate> thothJson
        let byThoth = Decode.Auto.unsafeFromString<TestDate> fasonJson

        check
            $"{kindName} date reads back as the same instant in UTC"
            (parsed.at.Kind = DateTimeKind.Utc
             && parsed.at = instant
             && fromThoth.at = instant)
            $"original: {instant:O}\n      parsed:   {parsed.at:O}\n      from thoth json: {fromThoth.at:O}"

        check
            $"thoth reads {kindName} date json as the same instant"
            (byThoth.at.ToUniversalTime() = instant)
            $"original: {instant:O}\n      thoth:    {byThoth.at.ToUniversalTime():O}"

    for text, expected in
        [ "2026-09-03T12:34:56Z", utcDate.AddMilliseconds -789.0
          "2026-09-03T12:34:56.789Z", utcDate
          "2026-09-03T12:34:56.7Z", utcDate.AddMilliseconds -89.0
          "2026-09-03T12:34:56.789000Z", utcDate
          "2026-09-03T14:34:56.789+02:00", utcDate
          "2026-09-03T07:34:56.789-05:00", utcDate
          "2026-09-03T12:34:56.789+00:00", utcDate ] do
        let parsed = Json.deserialize<TestDate> ("{\"at\":\"" + text + "\"}")

        check
            $"date text {text} reads as UTC"
            (parsed.at.Kind = DateTimeKind.Utc && parsed.at = expected)
            $"expected: {expected:O}\n      parsed:   {parsed.at:O}"

#if FABLE_COMPILER
    expectJson "utc date json" ({ at = utcDate }: TestDate) """{"at":"2026-09-03T12:34:56.789Z"}"""
#else
    expectJson "utc date json" ({ at = utcDate }: TestDate) """{"at":"2026-09-03T12:34:56.7890000Z"}"""

    check
        "seven-digit fraction reads exactly"
        (Json.deserialize<TestDate> """{"at":"2026-09-03T12:34:56.1234567Z"}""" = { at =
                                                                                      DateTime(
                                                                                          2026,
                                                                                          9,
                                                                                          3,
                                                                                          12,
                                                                                          34,
                                                                                          56,
                                                                                          DateTimeKind.Utc
                                                                                      )
                                                                                          .AddTicks
                                                                                          1234567L })
        ""
#endif

    if failures = 0 then
        printfn "All tests passed."
        0
    else
        eprintfn $"{failures} test(s) failed."
        1
