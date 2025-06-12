module Fason.TestLib.Program

open System
open Fason.TestLib.TestModule

[<EntryPoint>]
let realMain argv =
    do
        let x: TestAllBasicTypes =
            { a = true
              b = 1uy
              c = 2y
              d = '3'
              e = 4y
              f = 5s
              g = 6
              h = 7L
              i = 8uy
              j = 9us
              k = 10u
              l = 11UL
              m = 12.5f
              n = 13.5
              o = "14!"
              p = Guid.NewGuid()
              q = DateTime.Now
              r = TimeSpan.FromHours(2.0) }

        use writer = new Fason.JsonWriter()
        Fason.JsonSerializer.serialize (x, writer)
        printfn $"{writer}"

    do
        let x: TestRecord =
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

        use writer = new Fason.JsonWriter()
        Fason.JsonSerializer.serialize (x, writer)
        printfn $"{writer}"

    0
