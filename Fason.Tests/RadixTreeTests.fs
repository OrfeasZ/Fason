module Fason.Tests.RadixTreeTests

open NUnit.Framework
open Fason

[<TestFixture>]
type RadixTreeTests() =

    [<Test>]
    member _.``Empty tree after sort should have empty root``() =
        let tree = RadixTree.fromStrings [] |> RadixTree.sort

        let expected =
            { data = ""
              isLeaf = false
              children = [] }

        Assert.AreEqual(expected, tree.root)

    [<Test>]
    member _.``Single string tree after sort should have correct structure``() =
        let tree = RadixTree.fromStrings [ "hello" ] |> RadixTree.sort

        let expected =
            { data = ""
              isLeaf = false
              children =
                [ { data = "hello"
                    isLeaf = true
                    children = [] } ] }

        Assert.AreEqual(expected, tree.root)

    [<Test>]
    member _.``Multiple unrelated strings should be sorted alphabetically``() =
        let tree = RadixTree.fromStrings [ "dog"; "cat"; "bird" ] |> RadixTree.sort

        let expected =
            { data = ""
              isLeaf = false
              children =
                [ { data = "bird"
                    isLeaf = true
                    children = [] }
                  { data = "cat"
                    isLeaf = true
                    children = [] }
                  { data = "dog"
                    isLeaf = true
                    children = [] } ] }

        Assert.AreEqual(expected, tree.root)

    [<Test>]
    member _.``Strings with common prefix should have sorted children``() =
        let tree = RadixTree.fromStrings [ "testing"; "test"; "tester" ] |> RadixTree.sort

        let expected =
            { data = ""
              isLeaf = false
              children =
                [ { data = "test"
                    isLeaf = true
                    children =
                      [ { data = "er"
                          isLeaf = true
                          children = [] }
                        { data = "ing"
                          isLeaf = true
                          children = [] } ] } ] }

        Assert.AreEqual(expected, tree.root)

    [<Test>]
    member _.``Complex tree with multiple levels should be fully sorted``() =
        let tree =
            RadixTree.fromStrings [ "romulus"; "romane"; "romanus"; "rubicon"; "rubens"; "rubicundus" ]
            |> RadixTree.sort

        // The actual structure based on how the radix tree works
        let expected =
            { data = ""
              isLeaf = false
              children =
                [ { data = "r"
                    isLeaf = false
                    children =
                      [ { data = "om"
                          isLeaf = false
                          children =
                            [ { data = "an"
                                isLeaf = false
                                children =
                                  [ { data = "e"
                                      isLeaf = true
                                      children = [] }
                                    { data = "us"
                                      isLeaf = true
                                      children = [] } ] }
                              { data = "ulus"
                                isLeaf = true
                                children = [] } ] }
                        { data = "ub"
                          isLeaf = false
                          children =
                            [ { data = "ens"
                                isLeaf = true
                                children = [] }
                              { data = "ic"
                                isLeaf = false
                                children =
                                  [ { data = "on"
                                      isLeaf = true
                                      children = [] }
                                    { data = "undus"
                                      isLeaf = true
                                      children = [] } ] } ] } ] } ] }

        Assert.AreEqual(expected, tree.root)

    [<Test>]
    member _.``Tree with prefix relationships should maintain sorted order``() =
        let tree =
            RadixTree.fromStrings [ "application"; "app"; "apply"; "apple" ]
            |> RadixTree.sort

        // The actual structure - "apply" creates "appl" + "y", not "app" + "ly"
        let expected =
            { data = ""
              isLeaf = false
              children =
                [ { data = "app"
                    isLeaf = true
                    children =
                      [ { data = "l"
                          isLeaf = false
                          children =
                            [ { data = "e"
                                isLeaf = true
                                children = [] }
                              { data = "ication"
                                isLeaf = true
                                children = [] }
                              { data = "y"
                                isLeaf = true
                                children = [] } ] } ] } ] }

        Assert.AreEqual(expected, tree.root)

    [<Test>]
    member _.``Single character strings should be sorted correctly``() =
        let tree = RadixTree.fromStrings [ "c"; "a"; "b" ] |> RadixTree.sort

        let expected =
            { data = ""
              isLeaf = false
              children =
                [ { data = "a"
                    isLeaf = true
                    children = [] }
                  { data = "b"
                    isLeaf = true
                    children = [] }
                  { data = "c"
                    isLeaf = true
                    children = [] } ] }

        Assert.AreEqual(expected, tree.root)

    [<Test>]
    member _.``Length sorting should work correctly``() =
        let tree =
            RadixTree.fromStrings [ "small"; "smaller"; "smile"; "address"; "ad"; "banana"; "sad" ]
            |> RadixTree.sortByLength

        let expected =
            { data = ""
              isLeaf = false
              children =
                [ { data = "s"
                    isLeaf = false
                    children =
                      [ { data = "m"
                          isLeaf = false
                          children =
                            [ { data = "all"
                                isLeaf = true
                                children =
                                  [ { data = "er"
                                      isLeaf = true
                                      children = [] } ] }
                              { data = "ile"
                                isLeaf = true
                                children = [] } ] }
                        { data = "ad"
                          isLeaf = true
                          children = [] } ] }
                  { data = "ad"
                    isLeaf = true
                    children =
                      [ { data = "dress"
                          isLeaf = true
                          children = [] } ] }
                  { data = "banana"
                    isLeaf = true
                    children = [] } ] }

        Assert.AreEqual(expected, tree.root)
