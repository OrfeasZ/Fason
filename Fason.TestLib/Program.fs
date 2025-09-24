module Fason.TestLib.Program

open System
open Fason.TestLib.TestModule
open FSharp.UMX
open FSharp.Data.UnitSystems.SI.UnitSymbols

[<EntryPoint>]
let realMain argv =
    let original: TestAdvanced =
        { r =
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
              TestUnion.D
                  { a = 3
                    b = "4"
                    c = [ "5"; "6" ]
                    d = Some 7u
                    e = None
                    f = [| "8"; "9" |]
                    g = Map [ "ten", 10; "eleven", 11 ]
                    h = Some [| "twelve"; "thirteen" |]
                    i = Some [ [| Some "fourteen" |] ]
                    j = Some(Some "fifteen")
                    k = Some(Some(Some "sixteen"))
                    l = Set [ 'a'; 'a'; 'b'; 'c' ] }
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

    // Serialize to JSON and back.
    use writer = new Fason.JsonWriter()
    Fason.JsonSerializer.serialize (original, writer)

    let json = writer.ToString()
    let reader = Fason.JsonReader(json)

    try
        let parsed =
            Fason.JsonDeserializer.deserialize (Fason.TypeIdentifier<TestAdvanced>(), reader)

        if original <> parsed then
            failwith "deserialized value does not match original"
        else
            printfn "Deserialized value matches original!"
    with ex ->
        eprintfn $"\nFailed to deserialize: %A{ex}\n\n{reader.GetHint()}"

    Console.ReadLine() |> ignore

    0
