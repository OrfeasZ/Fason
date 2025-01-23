namespace Fason.TestLib

//[<FasonSerializable>]
module TestModule =
    type TestRecord = { a: int; b: string }

    type TestAnonymousRecord = {| a: int; b: string |}

    type TestNestedRecord = { x: int; y: string; c: TestRecord }

    type TestUnion =
        | A
        | B of int
        | C of string
        | D of TestRecord
        | E of x: int * y: string
