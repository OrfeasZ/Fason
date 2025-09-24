namespace Fason

open System
open System.Collections
open System.IO
open System.Linq
open System.Reflection
open System.Text
open Fason.TypeCollector
open Fabulous.AST
open type Fabulous.AST.Ast
open FSharp.UMX
open Fantomas.Core.SyntaxOak

[<AutoOpen>]
module AstHelpers =
    // TODO: Send this as PR (new EscapedString builder) to Fabulous.AST.
    let EscapedString (value: string) =
        let sb =
            value.ToCharArray()
            |> Seq.fold
                (fun (sb: StringBuilder) (c: char) ->
                    match c with
                    | '\n'
                    | '\u2028'
                    | '\u2028' -> sb.Append("\\n")
                    | '\r' -> sb.Append("\\r")
                    | '\t' -> sb.Append("\\t")
                    | '\'' -> sb.Append("\\'")
                    | '\\' -> sb.Append("\\\\")
                    | '"' -> sb.Append("\\\"")
                    | '\u0000' -> sb.Append("\\0")
                    | _ -> sb.Append(c))
                (StringBuilder())

        sb.ToString() |> String


[<AutoOpen>]
module rec Radix =
    type RadixTreeNode =
        { data: string
          isLeaf: bool
          children: RadixTreeNode list }

    /// A radix tree, used to store a set of strings.
    ///
    /// Given the strings "small", "smaller", "smile", "address", "ad", and "banana",
    /// the tree would look like this:
    ///
    ///         |--- "ad" (leaf)
    ///         |     |--- "dress" (leaf)
    ///         |
    /// (root)--|--- "banana" (leaf)
    ///         |
    ///         |--- "sm" (not a leaf)
    ///               |--- "all" (leaf)
    ///               |     |--- "er" (leaf)
    ///               |
    ///               |--- "ile" (leaf)
    type RadixTree = { root: RadixTreeNode }

    module RadixTree =
        /// An empty tree.
        let empty =
            { root =
                { data = ""
                  isLeaf = false
                  children = [] } }

        /// Insert a string into the tree.
        let insert (s: string) (tree: RadixTree) =
            if String.IsNullOrEmpty(s) then
                { tree with root.isLeaf = true }
            else
                let rec insertRec (key: string) (node: RadixTreeNode) =
                    // Find the child that shares a prefix with the key
                    let rec findMatchingChild children =
                        match children with
                        | [] -> None
                        | child :: rest ->
                            let commonPrefixLen =
                                let minLen = min key.Length child.data.Length
                                let mutable i = 0

                                while i < minLen && key[i] = child.data[i] do
                                    i <- i + 1

                                i

                            if commonPrefixLen > 0 then
                                Some(child, commonPrefixLen)
                            else
                                findMatchingChild rest

                    match findMatchingChild node.children with
                    | None ->
                        // No matching child found, create a new leaf node.
                        let newChild =
                            { data = key
                              isLeaf = true
                              children = [] }

                        { node with
                            children = newChild :: node.children }

                    | Some(matchingChild, commonPrefixLen) ->
                        // Get all children except the matching one.
                        let otherChildren = node.children |> List.filter (fun c -> c <> matchingChild)

                        if commonPrefixLen = matchingChild.data.Length then
                            // The entire edge label matches
                            if commonPrefixLen = key.Length then
                                // Exact match - mark as leaf
                                let updatedChild = { matchingChild with isLeaf = true }

                                { node with
                                    children = updatedChild :: otherChildren }
                            else
                                // Continue with remaining key
                                let remainingKey = key.Substring(commonPrefixLen)
                                let updatedChild = insertRec remainingKey matchingChild

                                { node with
                                    children = updatedChild :: otherChildren }
                        else
                            // Need to split the edge
                            let commonPrefix = matchingChild.data.Substring(0, commonPrefixLen)
                            let childSuffix = matchingChild.data.Substring(commonPrefixLen)

                            // Create the split child (preserve original child's properties but with new data)
                            let splitChild =
                                { data = childSuffix
                                  isLeaf = matchingChild.isLeaf
                                  children = matchingChild.children }

                            if commonPrefixLen = key.Length then
                                // Key ends at split point
                                let splitNode =
                                    { data = commonPrefix
                                      isLeaf = true
                                      children = [ splitChild ] }

                                { node with
                                    children = splitNode :: otherChildren }
                            else
                                // Key continues beyond split point
                                let remainingKey = key.Substring(commonPrefixLen)

                                let newLeaf =
                                    { data = remainingKey
                                      isLeaf = true
                                      children = [] }

                                let splitNode =
                                    { data = commonPrefix
                                      isLeaf = false
                                      children = [ splitChild; newLeaf ] }

                                { node with
                                    children = splitNode :: otherChildren }

                { tree with
                    root = insertRec s tree.root }

        /// Create a tree from a sequence of strings.
        let fromStrings (strings: string seq) =
            strings |> Seq.fold (fun tree str -> insert str tree) empty

        /// Sort the tree alphabetically.
        let sort (tree: RadixTree) =
            let rec sortNode (node: RadixTreeNode) =
                let sortedChildren = node.children |> List.map sortNode |> List.sortBy _.data

                { node with children = sortedChildren }

            { tree with root = sortNode tree.root }

        /// Sort the tree by length, then alphabetically.
        let sortByLength (tree: RadixTree) =
            let rec sortNode (node: RadixTreeNode) =
                let sortedChildren =
                    node.children
                    |> List.map sortNode
                    |> List.sortBy (fun n -> n.data.Length, n.data)

                { node with children = sortedChildren }

            { tree with root = sortNode tree.root }

type JsonEncoderCodegen =
    static member private generateBasicEncoder(b: BasicType) =
        match b with
        | BasicType.Bool -> ()
        | _ -> ()

    static member private getTypeName(typ: SerializableType ref) =
        match typ.Value with
        | SerializableType.Basic b -> b.typeName
        | SerializableType.AnonymousRecord r ->
            let fields =
                r.fields
                |> List.map (fun f -> %($"{f.name}: {f.fieldType |> JsonEncoderCodegen.getTypeName}"))
                |> String.concat "; "

            %("{| " + fields + " |}")

        | SerializableType.Record r -> r.name
        | SerializableType.Union u -> u.name
        | SerializableType.Enum e -> e.name
        | SerializableType.Tuple t ->
            t.values
            |> List.map (fun t -> t.valueType |> JsonEncoderCodegen.getTypeName |> string)
            |> String.concat " * "
            |> (fun s -> %s)

        | SerializableType.Array a -> %($"{a |> JsonEncoderCodegen.getTypeName} array")
        | SerializableType.List l -> %($"{l |> JsonEncoderCodegen.getTypeName} list")
        | SerializableType.Set s -> %($"Set<{s |> JsonEncoderCodegen.getTypeName}>")
        | SerializableType.Map(k, v) ->
            %($"Map<{k |> JsonEncoderCodegen.getTypeName}, {v |> JsonEncoderCodegen.getTypeName}>")

        | SerializableType.UnitOfMeasure uom ->
            %($"{uom.baseType |> JsonEncoderCodegen.getTypeName}<{uom.unitOfMeasure}>")

        | SerializableType.Optional o -> %($"{o |> JsonEncoderCodegen.getTypeName} option")

    static member private jsonEncodeString(value: string) =
        use writer = new StringWriter()
        writer.Write("\"")

        for character in value do
            match character with
            | '"' -> writer.Write("\\\"")
            | '\\' -> writer.Write("\\\\")
            | '/' -> writer.Write("\\/")
            | '\b' -> writer.Write("\\b")
            | '\f' -> writer.Write("\\f")
            | '\n' -> writer.Write("\\n")
            | '\r' -> writer.Write("\\r")
            | '\t' -> writer.Write("\\t")
            | c when c >= '\u0000' && c <= '\u001F' -> writer.Write($"\\u{character:X4}")
            | c -> writer.Write(c)

        writer.Write("\"")
        writer.ToString()

    static member private generateBasicSerializer(typ: BasicType) =
        // Writer has overloads for all basic types, so we can just pass the value directly.
        AppExpr("writer.Write", ParenExpr("value"))

    static member private generateFieldSerializer(fields: RecordField list) =
        [ for i, field in fields |> List.indexed do
              // TODO: Depending on configuration, completely omit optional fields.
              let prologue = if i > 0 then "," else ""

              AppExpr("writer.WritePlain", EscapedString $"{prologue}{JsonEncoderCodegen.jsonEncodeString field.name}:")

              let value =
                  // If the field is a uom, we need to strip the uom type from the value.
                  match field.fieldType.Value with
                  | SerializableType.UnitOfMeasure _ -> $"%%value.{field.name}"
                  | _ -> $"value.{field.name}"

              AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ value; "writer" ]))) ]

    static member private generateAnonymousRecordSerializer(typ: AnonymousRecordType) =
        CompExprBodyExpr(
            [ AppExpr("writer.WritePlain", EscapedString "{")
              yield! JsonEncoderCodegen.generateFieldSerializer typ.fields
              AppExpr("writer.WritePlain", EscapedString "}") ]
        )

    static member private generateRecordSerializer(typ: RecordType) =
        CompExprBodyExpr(
            [ AppExpr("writer.WritePlain", EscapedString "{")
              yield! JsonEncoderCodegen.generateFieldSerializer typ.fields
              AppExpr("writer.WritePlain", EscapedString "}") ]
        )

    static member private generateUnionSerializer(typ: UnionType) =
        MatchExpr(
            "value",
            [ for case in typ.cases do
                  let clauseParams =
                      case.fields
                      |> List.mapi (fun i _ -> $"p{i}")
                      |> String.concat ", "
                      |> (fun s -> if String.IsNullOrEmpty s then "" else $"({s})")

                  let innerExpr =
                      if case.fields.Length = 0 then
                          AppExpr("writer.WritePlain", EscapedString $"\"{case.name}\"")
                      else
                          CompExprBodyExpr(
                              [ AppExpr("writer.WritePlain", EscapedString "[")
                                AppExpr("writer.WritePlain", EscapedString $"\"{case.name}\"")

                                for i, field in case.fields |> List.indexed do
                                    AppExpr("writer.WritePlain", EscapedString ",")
                                    AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ $"p{i}"; "writer" ])))

                                AppExpr("writer.WritePlain", EscapedString "]") ]
                          )

                  MatchClauseExpr($"{typ.name}.{case.name}{clauseParams}", innerExpr) ]
        )

    static member private generateEnumSerializer(typ: EnumType) =
        let baseTypeName =
            SerializableType.Basic typ.valueType |> ref |> JsonEncoderCodegen.getTypeName

        MatchExpr(
            "value",
            [ for value in typ.values do
                  MatchClauseExpr(
                      $"{typ.name}.{value.name}",
                      AppExpr("writer.WritePlain", EscapedString $"\"{value.name}\"")
                  )

              MatchClauseExpr(
                  "other",
                  AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ $"{baseTypeName} other"; "writer" ])))
              ) ]
        )

    static member private generateTupleSerializer(typ: TupleType) =
        if typ.values.Length = 0 then
            AppExpr("writer.WritePlain", EscapedString "[]")
        else if typ.values.Length = 1 then
            AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ "value"; "writer" ])))
        else
            CompExprBodyExpr
                [
                  // Destructure the tuple into its individual values.
                  let values = typ.values |> List.mapi (fun i _ -> $"v{i}")

                  LetOrUseExpr(Value(TuplePat(values |> List.map NamedPat), IdentExpr("value")))

                  OtherExpr(AppExpr("writer.WritePlain", EscapedString "["))

                  for i, value in values |> List.indexed do
                      if i > 0 then
                          OtherExpr(AppExpr("writer.WritePlain", EscapedString ","))

                      OtherExpr(AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ value; "writer" ]))))

                  OtherExpr(AppExpr("writer.WritePlain", EscapedString "]")) ]

    static member private generateArraySerializer(typ: SerializableType ref) =
        CompExprBodyExpr
            [ AppExpr("writer.WritePlain", EscapedString "[")

              ForEachDoExpr(
                  "i, item",
                  "value |> Seq.indexed",
                  CompExprBodyExpr
                      [ IfThenExpr("i > 0", AppExpr("writer.WritePlain", EscapedString ","))
                        AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ "item"; "writer" ]))) ]
              )

              AppExpr("writer.WritePlain", EscapedString "]") ]

    static member private generateListSerializer(typ: SerializableType ref) =
        CompExprBodyExpr
            [ AppExpr("writer.WritePlain", EscapedString "[")

              ForEachDoExpr(
                  "i, item",
                  "value |> Seq.indexed",
                  CompExprBodyExpr
                      [ IfThenExpr("i > 0", AppExpr("writer.WritePlain", EscapedString ","))
                        AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ "item"; "writer" ]))) ]
              )

              AppExpr("writer.WritePlain", EscapedString "]") ]

    static member private generateSetSerializer(typ: SerializableType ref) =
        CompExprBodyExpr
            [ AppExpr("writer.WritePlain", EscapedString "[")

              ForEachDoExpr(
                  "i, item",
                  "value |> Seq.indexed",
                  CompExprBodyExpr
                      [ IfThenExpr("i > 0", AppExpr("writer.WritePlain", EscapedString ","))
                        AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ "item"; "writer" ]))) ]
              )

              AppExpr("writer.WritePlain", EscapedString "]") ]

    static member private generateMapSerializer(keyType: SerializableType ref, valueType: SerializableType ref) =
        CompExprBodyExpr
            [ AppExpr("writer.WritePlain", EscapedString "[")

              ForEachDoExpr(
                  "i, (key, value)",
                  "value |> Map.toSeq |> Seq.indexed",
                  CompExprBodyExpr
                      [ IfThenExpr("i > 0", AppExpr("writer.WritePlain", EscapedString ","))
                        AppExpr("writer.WritePlain", EscapedString "[")
                        AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ "key"; "writer" ])))
                        AppExpr("writer.WritePlain", EscapedString ",")
                        AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ "value"; "writer" ])))
                        AppExpr("writer.WritePlain", EscapedString "]") ]
              )

              AppExpr("writer.WritePlain", EscapedString "]") ]

    static member private generateUnitOfMeasureSerializer(typ: UomType) =
        CompExprBodyExpr [ AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ "%value"; "writer" ]))) ]

    static member private generateOptionalSerializer(typ: SerializableType ref) =
        // TODO: Support for nested optionals?
        MatchExpr(
            "value",
            [ MatchClauseExpr("Some v", AppExpr("JsonSerializer.serialize", ParenExpr(TupleExpr([ "v"; "writer" ]))))
              MatchClauseExpr("None", AppExpr("writer.WritePlain", EscapedString "null")) ]
        )

    static member private generateTypeSerializer(typ: SerializableType ref) =
        let valueType = JsonEncoderCodegen.getTypeName typ

        let serializerExpr =
            match typ.Value with
            | SerializableType.Basic b -> JsonEncoderCodegen.generateBasicSerializer b
            | SerializableType.AnonymousRecord r -> JsonEncoderCodegen.generateAnonymousRecordSerializer r
            | SerializableType.Record r -> JsonEncoderCodegen.generateRecordSerializer r
            | SerializableType.Union u -> JsonEncoderCodegen.generateUnionSerializer u
            | SerializableType.Enum e -> JsonEncoderCodegen.generateEnumSerializer e
            | SerializableType.Tuple t -> JsonEncoderCodegen.generateTupleSerializer t
            | SerializableType.Array a -> JsonEncoderCodegen.generateArraySerializer a
            | SerializableType.List l -> JsonEncoderCodegen.generateListSerializer l
            | SerializableType.Set s -> JsonEncoderCodegen.generateSetSerializer s
            | SerializableType.Map(k, v) -> JsonEncoderCodegen.generateMapSerializer (k, v)
            | SerializableType.UnitOfMeasure uom -> JsonEncoderCodegen.generateUnitOfMeasureSerializer uom
            | SerializableType.Optional o -> JsonEncoderCodegen.generateOptionalSerializer o


        let serializerParams =
            ParenPat(
                TuplePat(
                    [ ParameterPat("value", valueType.ToString())
                      ParameterPat("writer", "Fason.JsonWriter") ]
                )
            )

        Member("serialize", serializerParams, serializerExpr).toStatic ()

    static member private generateBasicDeserializer(b: BasicType) =
        CompExprBodyExpr
            [ match b with
              | BasicType.Bool -> AppExpr("reader.ReadBool", "()")
              | BasicType.Byte -> AppExpr("reader.ReadByte", "()")
              | BasicType.SByte -> AppExpr("reader.ReadSByte", "()")
              | BasicType.Char -> AppExpr("reader.ReadChar", "()")
              | BasicType.Int8 -> AppExpr("reader.ReadInt8", "()")
              | BasicType.Int16 -> AppExpr("reader.ReadInt16", "()")
              | BasicType.Int32 -> AppExpr("reader.ReadInt32", "()")
              | BasicType.Int64 -> AppExpr("reader.ReadInt64", "()")
              | BasicType.UInt8 -> AppExpr("reader.ReadUInt8", "()")
              | BasicType.UInt16 -> AppExpr("reader.ReadUInt16", "()")
              | BasicType.UInt32 -> AppExpr("reader.ReadUInt32", "()")
              | BasicType.UInt64 -> AppExpr("reader.ReadUInt64", "()")
              | BasicType.Single -> AppExpr("reader.ReadSingle", "()")
              | BasicType.Double -> AppExpr("reader.ReadDouble", "()")
              | BasicType.String -> AppExpr("reader.ReadString", "()")
              | BasicType.Guid -> AppExpr("reader.ReadGuid", "()")
              | BasicType.DateTime -> AppExpr("reader.ReadDateTime", "()")
              | BasicType.TimeSpan -> AppExpr("reader.ReadTimeSpan", "()") ]

    /// Generate code that deserializes a field of the given type.
    static member private parserForType(typ: SerializableType ref, withOptional: bool) =
        // If the field is optional, we don't need to wrap it in Some.
        let wrapperType =
            if typ.Value.IsOptional || not withOptional then
                ""
            else
                "Some"

        let secondWrapper = if typ.Value.IsUnitOfMeasure then "%" else ""

        let typeName =
            match typ.Value with
            | SerializableType.UnitOfMeasure uom -> uom.baseType
            | _ -> typ
            |> JsonEncoderCodegen.getTypeName

        $"({wrapperType} ({secondWrapper} (JsonDeserializer.deserialize (TypeIdentifier<{typeName}>(), reader))))"

    static member private generateFieldDeserializer
        (fields: RecordField list, typeName: string<typeName>, isAnonymous: bool)
        =
        CompExprBodyExpr
            [
              // Create a temporary variable for each field to read.
              for i in 0 .. fields.Length - 1 do
                  LetOrUseExpr(Value(NamedPat($"f{i}"), IdentExpr("None")) |> _.toMutable())

              // Also keep a bitset of which fields have been found and read.
              // Calculate the initial bitset by setting each optional field to 1 / true.
              // We then try to parse each field, and if we find it and the parsing is successful,
              // we set the corresponding bit to 1 / true.
              // If at the end any bit is still 0 / false, we throw an error.
              let initialBitset =
                  fields
                  |> List.map _.fieldType.Value.IsOptional
                  |> Array.ofList
                  |> BitSet.fromBoolSeq true

              // Convert the bit array to a list of 64-bit integers.
              // This way we can embed it directly into the generated code and skip runtime initialization.
              let bitsetValues =
                  initialBitset.ToArray()
                  |> Array.map (fun v -> $"0x{v:X8}u")
                  |> String.concat "; "

              // let mutable found = Fason.BitSet [| 0x12345678u; ... |]
              LetOrUseExpr(
                  Value(NamedPat("found"), IdentExpr($"Fason.BitSet [| {bitsetValues} |]"))
                  |> _.toMutable()
              )

              // let mutable doneReading = false
              LetOrUseExpr(Value(NamedPat("doneReading"), IdentExpr("false")) |> _.toMutable())

              // let mutable readField = false
              LetOrUseExpr(Value(NamedPat("readField"), IdentExpr("false")) |> _.toMutable())

              OtherExpr(AppExpr("reader.SkipWhitespace", "()"))

              // Expect to find an opening brace.
              OtherExpr(
                  IfThenExpr(
                      "reader.ReadUntil(\"{\") |> not",
                      AppExpr("failwith", EscapedString "expected the start of an object, but got something else")
                  )
              )

              let fieldByName = fields |> List.mapi (fun i f -> f.name, (f, i)) |> Map.ofList

              let rec treeNodeToParser (node: RadixTreeNode, parentText: string, elseIf: bool) : WidgetBuilder<Expr> =
                  let mainIfExpr =
                      AppLongIdentAndSingleParenArgExpr("reader.StartsWith", EscapedString node.data)

                  let fieldReader (field, index) =
                      // reader.Skip() // Skip the closing quote that we just read.
                      //
                      // if reader.ReadUntil(":") |> not then
                      //     failwithf $"expected a colon after the field name, but got something else"
                      //
                      // fX <- Some (JsonDeserializer.deserialize (TypeIdentifier<T>(), reader))
                      // found.Set(X, true)

                      CompExprBodyExpr
                          [ AppExpr("reader.Skip", "()")
                            IfThenExpr(
                                "reader.ReadUntil(\":\") |> not",
                                AppExpr(
                                    "failwith",
                                    EscapedString "expected a colon after the field name, but got something else"
                                )
                            )

                            let parser = JsonEncoderCodegen.parserForType (field.fieldType, withOptional = true)
                            AppExpr($"f{index}", $"<- {parser}")
                            AppExpr("found.Set", ParenExpr(TupleExpr([ index.ToString(); "true" ]))) ]

                  // TODO: Implement reader field skipping

                  let mainIfBody =
                      // reader.SkipCount({node.data.Length}) // Skip the characters of the field name that were just read.
                      CompExprBodyExpr
                          [ AppExpr("reader.SkipCount", ConstantExpr(node.data.Length.ToString()))

                            let fieldName = parentText + node.data

                            // Figure out the construct we need.
                            match node.isLeaf, node.children.Length > 0 with
                            | true, true ->
                                // Is a leaf, but has children too.
                                // Check for leaf node first, then children.
                                // Otherwise, if nothing matches, have the
                                // reader completely skip this field.
                                let field, index = fieldByName[fieldName]

                                IfThenElifExpr(
                                    [ IfThenExpr("reader.Peek() = Some '\"'", fieldReader (field, index))

                                      for child in node.children do
                                          treeNodeToParser (child, fieldName, true) ],
                                    AppExpr("reader.SkipObjectField", "()")
                                )

                            | true, false ->
                                // Is a leaf, and has NO children.
                                // Check for leaf node. If not found,
                                // skip the field.
                                let field, index = fieldByName[fieldName]

                                IfThenElseExpr(
                                    "reader.Peek() = Some '\"'",
                                    fieldReader (field, index),
                                    AppExpr("reader.SkipObjectField", "()")
                                )

                            | false, true ->
                                // Not leaf node, but has children.
                                // Check all children.
                                // If none match, skip the field.
                                IfThenElifExpr(
                                    [ for i, child in node.children |> Seq.indexed do
                                          treeNodeToParser (child, fieldName, i > 0) ],
                                    AppExpr("reader.SkipObjectField", "()")
                                )
                            | false, false ->
                                // Not a leaf node, and has no children.
                                // Nothing to do here, so just skip.
                                AppExpr("reader.SkipObjectFields", "()") ]

                  if elseIf then
                      ElIfThenExpr(mainIfExpr, mainIfBody)
                  else
                      IfThenExpr(mainIfExpr, mainIfBody)

              // Create a radix tree from the field names, and then sort it from
              // shortest node data length to longest, with ties broken by alphabetical order.
              // We then use this tree to generate efficient if/else chains for parsing.
              // TODO: Escape the names.
              let fieldTree =
                  fields |> List.map _.name |> RadixTree.fromStrings |> RadixTree.sortByLength

              // while not doneReading do
              OtherExpr(
                  WhileExpr(
                      AppExpr("not", "doneReading"),
                      CompExprBodyExpr
                          [
                            // Look for a quote.
                            IfThenElifExpr(
                                [
                                  // if reader.ReadUntil("}") then
                                  //     doneReading <- true
                                  IfThenExpr("reader.ReadUntil(\"}\")", AppExpr("doneReading", "<- true"))

                                  // elif readField then
                                  //   if reader.ReadUntil(",") |> not then
                                  //     failwith "expected a comma after the field value, but got something else"
                                  //   readField <- false
                                  ElIfThenExpr(
                                      "readField",
                                      CompExprBodyExpr
                                          [ IfThenExpr(
                                                "reader.ReadUntil(\",\") |> not",
                                                AppExpr(
                                                    "failwith",
                                                    EscapedString
                                                        "expected a comma after the field value, but got something else"
                                                )
                                            )

                                            AppExpr("readField", "<- false") ]
                                  )

                                  // elif reader.ReadUntil("\"") then
                                  //   mainIfBody
                                  //   readField <- true
                                  ElIfThenExpr(
                                      AppExpr("reader.ReadUntil", EscapedString "\""),
                                      CompExprBodyExpr
                                          [ IfThenElifExpr(
                                                [ for i, child in fieldTree.root.children |> Seq.indexed do
                                                      treeNodeToParser (child, "", i > 0) ],
                                                AppExpr("reader.SkipObjectField", "()")
                                            )

                                            AppExpr("readField", "<- true") ]
                                  ) ],
                                CompExprBodyExpr
                                    [
                                      // else
                                      //   let latest = reader.Peek()
                                      //   failwithf $"expected a quote before a field name, or an object closing brace, but instead got '{latest}'"
                                      // TODO: Actually print the character.
                                      //LetOrUseExpr(Value(NamedPat("latest"), AppExpr("reader.Peek", "()")))
                                      OtherExpr(
                                          AppExpr(
                                              "failwith",
                                              EscapedString
                                                  "expected a quote before a field name, or an object closing brace, but instead got something else"
                                          )
                                      ) ]
                            ) ]
                  )
              )

              // if found.AnyFalse() then
              //   failwith "missing required field"
              // TODO: Better error message
              OtherExpr(IfThenExpr("found.AnyFalse()", AppExpr("failwith", EscapedString "missing required field")))

              // Now create the record to return.
              let recordFields =
                  [ for i, field in fields |> Seq.indexed do
                        let suffix = if field.fieldType.Value.IsOptional then "" else ".Value"
                        RecordFieldExpr(field.name, $"f{i}{suffix}") ]

              OtherExpr(
                  AppExpr(
                      $"let result: {typeName} = ",
                      (if isAnonymous then
                           AnonRecordExpr(recordFields)
                       else
                           RecordExpr(recordFields))
                  )
              )

              OtherExpr(IdentExpr("result")) ]

    static member private generateAnonymousRecordDeserializer(typ: AnonymousRecordType, typeName: string<typeName>) =
        CompExprBodyExpr [ JsonEncoderCodegen.generateFieldDeserializer (typ.fields, typeName, isAnonymous = true) ]

    static member private generateRecordDeserializer(typ: RecordType, typeName: string<typeName>) =
        CompExprBodyExpr [ JsonEncoderCodegen.generateFieldDeserializer (typ.fields, typeName, isAnonymous = false) ]

    static member private generateUnionCaseDeserializer(case: UnionCase, typeName: string<typeName>) =
        if case.fields.Length = 0 then
            IdentExpr($"{typeName}.{case.name}")
        else
            CompExprBodyExpr
                [
                  // if not inArray then
                  //   failwith "tried reading a union case with values, but the tag was not in an array"
                  //
                  // if reader.ReadUntil(",") |> not then
                  //   failwith "expected a comma after the tag, but got something else"
                  //
                  // let u0 = JsonDeserializer.deserialize (TypeIdentifier<int32>(), reader)
                  //
                  // if reader.ReadUntil(",") |> not then
                  //   failwith "expected a comma after the field value, but got something else"
                  //
                  // let u1 JsonDeserializer.deserialize (TypeIdentifier<string>(), reader)
                  //
                  // if reader.ReadUntil(",") |> not then
                  //   failwith "expected a comma after the field value, but got something else"
                  //
                  // let u2 = JsonDeserializer.deserialize (TypeIdentifier<string list option>(), reader)
                  //
                  // Case(u0, u1, u2)

                  OtherExpr(
                      IfThenExpr(
                          "not inArray",
                          AppExpr(
                              "failwith",
                              EscapedString "tried reading a union case with values, but the tag was not in an array"
                          )
                      )
                  )

                  OtherExpr(
                      IfThenExpr(
                          "reader.ReadUntil(\",\") |> not",
                          AppExpr("failwith", EscapedString "expected a comma after the tag, but got something else")
                      )
                  )

                  // Create a temporary variable for each field to read.
                  for i in 0 .. case.fields.Length - 1 do
                      let field = case.fields[i]

                      if i > 0 then
                          OtherExpr(
                              IfThenExpr(
                                  "reader.ReadUntil(\",\") |> not",
                                  AppExpr(
                                      "failwith",
                                      EscapedString "expected a comma after the field value, but got something else"
                                  )
                              )
                          )

                      LetOrUseExpr(
                          Value(
                              NamedPat($"u{i}"),
                              JsonEncoderCodegen.parserForType (field.fieldType, withOptional = false)
                          )
                      )

                  OtherExpr(
                      AppExpr(
                          $"{typeName}.{case.name}",
                          ParenExpr(TupleExpr(case.fields |> List.mapi (fun i _ -> $"u{i}") |> List.map IdentExpr))
                      )
                  ) ]

    static member private generateUnionTagParser(typ: UnionType) =
        MatchExpr(
            "tag",
            [ for case in typ.cases do
                  MatchClauseExpr($"\"{case.name}\"", JsonEncoderCodegen.generateUnionCaseDeserializer (case, typ.name))

              MatchClauseExpr("_", AppExpr("failwith", EscapedString "unknown tag")) ]
        )

    static member private generateUnionDeserializer(typ: UnionType) =
        CompExprBodyExpr
            [
              // let inArray = reader.ReadUntil("[")
              // let tag = reader.ReadString()
              //
              // let value =
              //     match tag with
              //     | "X" -> parseX
              //     | _ -> failwith "unknown tag"
              //
              // if inArray then
              //    if reader.ReadUntil("]") |> not then
              //        failwith "expected a closing brace after the array, but got something else"
              //
              // value

              LetOrUseExpr(Value(NamedPat("inArray"), AppExpr("reader.ReadUntil", EscapedString "[")))
              LetOrUseExpr(Value(NamedPat("tag"), AppExpr("reader.ReadString", "()")))
              LetOrUseExpr(Value(NamedPat("value"), JsonEncoderCodegen.generateUnionTagParser typ))
              OtherExpr(
                  IfThenExpr(
                      "inArray",
                      IfThenExpr(
                          "reader.ReadUntil(\"]\") |> not",
                          AppExpr(
                              "failwith",
                              EscapedString "expected a closing brace after the array, but got something else"
                          )
                      )
                  )
              )
              OtherExpr(IdentExpr("value")) ]

    static member private generateEnumDeserializer(typ: EnumType) =
        CompExprBodyExpr
            [
              // match reader.ReadString() with
              // | "A" -> Enum.A
              // | _ -> failwith "unknown enum value"
              MatchExpr(
                  "reader.ReadString()",
                  [ for value in typ.values do
                        MatchClauseExpr($"\"{value.name}\"", IdentExpr($"{typ.name}.{value.name}"))

                    MatchClauseExpr("_", AppExpr("failwith", EscapedString "unknown enum value")) ]
              ) ]

    static member private generateTupleDeserializer(typ: TupleType) =
        CompExprBodyExpr
            [
              // if reader.ReadUntil("[") |> not then
              //     failwith "expected an opening brace, but got something else"
              //
              // let t0 = JsonDeserializer.deserialize (TypeIdentifier<int32>(), reader)
              //
              // if reader.ReadUntil(",") |> not then
              //     failwith "expected a comma after the tuple value, but got something else"
              //
              // let t1 = JsonDeserializer.deserialize (TypeIdentifier<string>(), reader)
              //
              // if reader.ReadUntil("]") |> not then
              //   failwith "expected a closing brace, but got something else"
              //
              // (t0, t1)

              OtherExpr(
                  IfThenExpr(
                      "reader.ReadUntil(\"[\") |> not",
                      AppExpr("failwith", EscapedString "expected an opening brace, but got something else")
                  )
              )

              for i in 0 .. typ.values.Length - 1 do
                  if i > 0 then
                      OtherExpr(
                          IfThenExpr(
                              "reader.ReadUntil(\",\") |> not",
                              AppExpr(
                                  "failwith",
                                  EscapedString "expected a comma after the tuple value, but got something else"
                              )
                          )
                      )

                  LetOrUseExpr(
                      Value(
                          NamedPat($"t{i}"),
                          JsonEncoderCodegen.parserForType (typ.values[i].valueType, withOptional = false)
                      )
                  )

              OtherExpr(
                  IfThenExpr(
                      "reader.ReadUntil(\"]\") |> not",
                      AppExpr("failwith", EscapedString "expected a closing brace, but got something else")
                  )
              )

              OtherExpr(TupleExpr(typ.values |> List.mapi (fun i _ -> $"t{i}"))) ]

    static member private generateCollectionDeserializer(typ: SerializableType ref, collectionType: string) =
        CompExprBodyExpr
            [
              // if reader.ReadUntil("[") |> not then
              //     failwith "expected an opening brace, but got something else"
              //
              // reader.SkipWhitespace()
              // let mutable values = []
              //
              // while reader.Peek().IsSome && reader.Peek() <> Some ']' do
              //   if arr.Length > 0 then
              //     if reader.ReadUntil(",") |> not then
              //       failwith "expected a comma after the array value, but got something else"
              //
              //   values -> values |> Array.append (JsonDeserializer.deserialize (TypeIdentifier<T>(), reader))
              //   reader.SkipWhitespace()
              //
              // if reader.ReadUntil("]") |> not then
              //   failwith "expected a closing brace, but got something else"
              //
              // values

              OtherExpr(
                  IfThenExpr(
                      "reader.ReadUntil(\"[\") |> not",
                      AppExpr("failwith", EscapedString "expected an opening brace, but got something else")
                  )
              )

              OtherExpr(AppExpr("reader.SkipWhitespace", "()"))
              LetOrUseExpr(Value(NamedPat("values"), $"{collectionType}.empty") |> _.toMutable())

              OtherExpr(
                  WhileExpr(
                      AppExpr("reader.Peek().IsSome", "&& reader.Peek() <> Some ']'"),
                      CompExprBodyExpr
                          [ let lengthVar = if collectionType = "Set" then "Count" else "Length"

                            IfThenExpr(
                                $"values.{lengthVar} > 0",
                                IfThenExpr(
                                    "reader.ReadUntil(\",\") |> not",
                                    AppExpr(
                                        "failwith",
                                        EscapedString "expected a comma after the array value, but got something else"
                                    )
                                )
                            )

                            let deserializer = JsonEncoderCodegen.parserForType (typ, withOptional = false)

                            match collectionType with
                            | "Array" -> AppExpr("values", $"<- Array.append values [| {deserializer} |]")
                            | "Set" -> AppExpr("values", $"<- values |> Set.add {deserializer}")
                            | _ -> AppExpr("values", $"<- {collectionType}.append values [ {deserializer} ]")

                            AppExpr("reader.SkipWhitespace", "()") ]
                  )
              )

              OtherExpr(
                  IfThenExpr(
                      "reader.ReadUntil(\"]\") |> not",
                      AppExpr("failwith", EscapedString "expected a closing brace, but got something else")
                  )
              )

              OtherExpr(IdentExpr("values")) ]

    static member private generateArrayDeserializer(typ: SerializableType ref) =
        JsonEncoderCodegen.generateCollectionDeserializer (typ, "Array")

    static member private generateListDeserializer(typ: SerializableType ref) =
        JsonEncoderCodegen.generateCollectionDeserializer (typ, "List")

    static member private generateSetDeserializer(typ: SerializableType ref) =
        JsonEncoderCodegen.generateCollectionDeserializer (typ, "Set")

    static member private generateMapDeserializer(typ: SerializableType ref * SerializableType ref) =
        CompExprBodyExpr
            [
              // if reader.ReadUntil("[") |> not then
              //     failwith "expected an opening brace, but got something else"
              //
              // let mutable values = Map.empty
              //
              // while reader.Peek().IsSome && reader.Peek() <> Some ']' do
              //    if values.Count > 0 then
              //        if reader.ReadUntil(",") |> not then
              //            failwith "expected a comma after the map value, but got something else"
              //
              //    if reader.ReadUntil("[") |> not then
              //        failwith "expected an opening brace for the map entry, but got something else"
              //
              //    let key = JsonDeserializer.deserialize (TypeIdentifier<K>(), reader)
              //    if reader.ReadUntil(",") |> not then
              //        failwith "expected a comma after the map key, but got something else"
              //
              //    let value = JsonDeserializer.deserialize (TypeIdentifier<V>(), reader)
              //
              //    if reader.ReadUntil("]") |> not then
              //        failwith "expected a closing brace for the map entry, but got something else"
              //
              //    values <- values |> Map.add key value
              //
              // if reader.ReadUntil("]") |> not then
              //     failwith "expected a closing brace, but got something else"
              //
              // values
              OtherExpr(
                  IfThenExpr(
                      "reader.ReadUntil(\"[\") |> not",
                      AppExpr("failwith", EscapedString "expected an opening brace, but got something else")
                  )
              )

              LetOrUseExpr(Value(NamedPat("values"), "Map.empty") |> _.toMutable())

              OtherExpr(
                  WhileExpr(
                      AppExpr("reader.Peek().IsSome", "&& reader.Peek() <> Some ']'"),
                      CompExprBodyExpr
                          [ OtherExpr(
                                IfThenExpr(
                                    "values.Count > 0",
                                    IfThenExpr(
                                        "reader.ReadUntil(\",\") |> not",
                                        AppExpr(
                                            "failwith",
                                            EscapedString "expected a comma after the map value, but got something else"
                                        )
                                    )
                                )
                            )
                            OtherExpr(
                                IfThenExpr(
                                    "reader.ReadUntil(\"[\") |> not",
                                    AppExpr(
                                        "failwith",
                                        EscapedString
                                            "expected an opening brace for the map entry, but got something else"
                                    )
                                )
                            )
                            LetOrUseExpr(
                                Value(NamedPat("key"), JsonEncoderCodegen.parserForType (fst typ, withOptional = false))
                            )
                            OtherExpr(
                                IfThenExpr(
                                    "reader.ReadUntil(\",\") |> not",
                                    AppExpr(
                                        "failwith",
                                        EscapedString "expected a comma after the map key, but got something else"
                                    )
                                )
                            )
                            LetOrUseExpr(
                                Value(
                                    NamedPat("value"),
                                    JsonEncoderCodegen.parserForType (snd typ, withOptional = false)
                                )
                            )
                            OtherExpr(
                                IfThenExpr(
                                    "reader.ReadUntil(\"]\") |> not",
                                    AppExpr(
                                        "failwith",
                                        EscapedString
                                            "expected a closing brace for the map entry, but got something else"
                                    )
                                )
                            )
                            OtherExpr(AppExpr("values", "<- values |> Map.add key value")) ]
                  )
              )
              OtherExpr(
                  IfThenExpr(
                      "reader.ReadUntil(\"]\") |> not",
                      AppExpr("failwith", EscapedString "expected a closing brace, but got something else")
                  )
              )
              OtherExpr(IdentExpr("values")) ]

    static member private generateUnitOfMeasureDeserializer(typ: UomType) =
        CompExprBodyExpr
            [ AppExpr(
                  "%",
                  $"(JsonDeserializer.deserialize (TypeIdentifier<{typ.baseType |> JsonEncoderCodegen.getTypeName}>(), reader))"
              ) ]

    static member private generateOptionalDeserializer(typ: SerializableType ref) =
        CompExprBodyExpr
            [
              // reader.SkipWhitespace()
              // if reader.Peek() = Some 'n' then
              //    if reader.ReadUntil("null") |> not then
              //        failwith "expected null, but got something else"
              //    None
              // else
              //    Some (JsonDeserializer.deserialize (TypeIdentifier<T>(), reader))

              let typename = JsonEncoderCodegen.getTypeName typ

              AppExpr("reader.SkipWhitespace", "()")

              IfThenElseExpr(
                  "reader.Peek() = Some 'n'",
                  CompExprBodyExpr
                      [ IfThenExpr(
                            "reader.ReadUntil(\"null\") |> not",
                            AppExpr("failwith", EscapedString "expected null or a value, but got something else")
                        )

                        IdentExpr("None") ],
                  CompExprBodyExpr
                      [ AppExpr(
                            "Some",
                            ParenExpr(
                                TupleExpr(
                                    [ AppExpr(
                                          "JsonDeserializer.deserialize",
                                          ParenExpr(TupleExpr([ $"TypeIdentifier<{typename}>()"; "reader" ]))
                                      ) ]
                                )
                            )
                        ) ]
              ) ]

    static member private generateTypeDeserializer(typ: SerializableType ref) =
        let valueType = JsonEncoderCodegen.getTypeName typ

        let deserializerExpr =
            match typ.Value with
            | SerializableType.Basic b -> JsonEncoderCodegen.generateBasicDeserializer b
            | SerializableType.AnonymousRecord r ->
                JsonEncoderCodegen.generateAnonymousRecordDeserializer (r, valueType)
            | SerializableType.Record r -> JsonEncoderCodegen.generateRecordDeserializer (r, valueType)
            | SerializableType.Union u -> JsonEncoderCodegen.generateUnionDeserializer u
            | SerializableType.Enum e -> JsonEncoderCodegen.generateEnumDeserializer e
            | SerializableType.Tuple t -> JsonEncoderCodegen.generateTupleDeserializer t
            | SerializableType.Array a -> JsonEncoderCodegen.generateArrayDeserializer a
            | SerializableType.List l -> JsonEncoderCodegen.generateListDeserializer l
            | SerializableType.Set s -> JsonEncoderCodegen.generateSetDeserializer s
            | SerializableType.Map(k, v) -> JsonEncoderCodegen.generateMapDeserializer (k, v)
            | SerializableType.UnitOfMeasure uom -> JsonEncoderCodegen.generateUnitOfMeasureDeserializer uom
            | SerializableType.Optional o -> JsonEncoderCodegen.generateOptionalDeserializer o

        let deserializerParams =
            ParenPat(
                TuplePat(
                    [ ParameterPat("_", $"TypeIdentifier<{valueType.ToString()}>")
                      ParameterPat("reader", "Fason.JsonReader") ]
                )
            )

        Member("deserialize", deserializerParams, deserializerExpr).toStatic ()

    static member private optimize(oak: Fantomas.Core.SyntaxOak.Oak) =
        // Go through children recursively, combining consecutive writer.WritePlain AppExprs into one.
        let rec optimizeNode (node: Node) =
            match node with
            | :? ExprCompExprBodyNode as compExpr ->
                // We found a comp expr body. Now combine consecutive writer.WritePlain ExprAppNodes.
                let mutable statements = compExpr.Statements
                let mutable optimizedStatements = []

                while statements.Length > 0 do
                    match statements with
                    | ComputationExpressionStatement.OtherStatement(Expr.App first) :: ComputationExpressionStatement.OtherStatement(Expr.App second) :: rest ->
                        match first.FunctionExpr, second.FunctionExpr with
                        | Expr.Constant(Constant.FromText firstFn), Expr.Constant(Constant.FromText secondFn) when
                            firstFn.Text = "writer.WritePlain" && secondFn.Text = "writer.WritePlain"
                            ->
                            // Combine the first and second writeplain parameters
                            // by removing the last character from the first, and the first from the second.

                            let firstArg, firstRange =
                                match first.Arguments with
                                | [ Expr.Constant(Constant.FromText x) ] -> x.Text, x.Range
                                | _ ->
                                    failwith $"Expected single argument to writer.WritePlain, but got {first.Arguments}"

                            let secondArg =
                                match second.Arguments with
                                | [ Expr.Constant(Constant.FromText x) ] -> x.Text
                                | _ ->
                                    failwith
                                        $"Expected single argument to writer.WritePlain, but got {second.Arguments}"

                            let combinedArg = firstArg[.. firstArg.Length - 2] + secondArg[1..]

                            let exprAppNode =
                                ExprAppNode(
                                    first.FunctionExpr,
                                    [ Expr.Constant(Constant.FromText(SingleTextNode(combinedArg, firstRange))) ],
                                    first.Range
                                )

                            // Make the new statement we just made into the next statement to process.
                            // That way, if there's more writer.WritePlain calls, we'll combine them too.
                            statements <- ComputationExpressionStatement.OtherStatement(Expr.App exprAppNode) :: rest

                        | _ ->
                            // Not two consecutive writer.WritePlain calls. Just add the first one to the optimized list
                            // and move on.
                            optimizedStatements <-
                                optimizedStatements
                                @ [ ComputationExpressionStatement.OtherStatement(Expr.App first) ]

                            statements <- ComputationExpressionStatement.OtherStatement(Expr.App second) :: rest

                    | first :: rest ->
                        // Not two consecutive writer.WritePlain calls. Just add the first one to the optimized list
                        // and move on.
                        optimizedStatements <- optimizedStatements @ [ first ]
                        statements <- rest

                    | [] -> ()

                // Reflection workaround so we can mutate the AST without having to reconstruct
                // individual nodes from scratch (since we want to access them using .Children,
                // and repacking everything individually is quite annoying).
                compExpr
                |> _.GetType()
                |> _.GetField("Statements@", BindingFlags.NonPublic ||| BindingFlags.Instance)
                |> _.SetValue(compExpr, optimizedStatements)

            | _ -> ()

            node.Children |> Seq.iter optimizeNode

        let optimizeNodes (nodes: 'a list when 'a :> Node) =
            nodes |> List.iter optimizeNode
            nodes

        Fantomas.Core.SyntaxOak.Oak(
            oak.ParsedHashDirectives |> optimizeNodes,
            oak.ModulesOrNamespaces |> optimizeNodes,
            oak.Range
        )

    static member generate(types: SerializableType ref array) =
        // If we have any non-numeric uoms, add an import to FSharp.UMX.
        // TODO: This should work recursively, for things like fields of records, lists, etc.
        // TODO: Should it? Won't we have all children types in this array anyway?
        let hasNonNumericUom =
            types
            |> Seq.exists (fun t ->
                match t.Value with
                | SerializableType.UnitOfMeasure uom ->
                    match uom.baseType.Value with
                    | SerializableType.Basic BasicType.Single
                    | SerializableType.Basic BasicType.Double
                    | SerializableType.Basic BasicType.Int8
                    | SerializableType.Basic BasicType.Int16
                    | SerializableType.Basic BasicType.Int32
                    | SerializableType.Basic BasicType.Int64
                    | SerializableType.Basic BasicType.UInt8
                    | SerializableType.Basic BasicType.UInt16
                    | SerializableType.Basic BasicType.UInt32
                    | SerializableType.Basic BasicType.UInt64 -> false
                    | _ -> true
                | _ -> false)

        // Collect the types that we need to generate serializers for.
        // To do so, we need to replace all uom types with their base type, since uoms get erased
        // and can't be used for overloads. We then make sure that we don't have any duplicates.
        // TODO: Support for tuples and anonymous records.
        let typesWithSerializer =
            types
            |> Seq.map (fun typ ->
                match typ.Value with
                | SerializableType.UnitOfMeasure uom -> uom.baseType.Value
                | SerializableType.Array a ->
                    match a.Value with
                    | SerializableType.UnitOfMeasure uom -> SerializableType.Array uom.baseType
                    | _ -> typ.Value

                | SerializableType.List l ->
                    match l.Value with
                    | SerializableType.UnitOfMeasure uom -> SerializableType.List uom.baseType
                    | _ -> typ.Value

                | SerializableType.Set s ->
                    match s.Value with
                    | SerializableType.UnitOfMeasure uom -> SerializableType.Set uom.baseType
                    | _ -> typ.Value

                | SerializableType.Map(k, v) ->
                    match k.Value, v.Value with
                    | SerializableType.UnitOfMeasure uk, SerializableType.UnitOfMeasure uv ->
                        SerializableType.Map(uk.baseType, uv.baseType)
                    | SerializableType.UnitOfMeasure uk, _ -> SerializableType.Map(uk.baseType, v)
                    | _, SerializableType.UnitOfMeasure uv -> SerializableType.Map(k, uv.baseType)
                    | _ -> typ.Value

                | SerializableType.Optional o ->
                    match o.Value with
                    | SerializableType.UnitOfMeasure uom -> SerializableType.Optional uom.baseType
                    | _ -> typ.Value
                | _ -> typ.Value)
            |> Seq.distinct

        let oak =
            Ast.Oak() {
                Namespace("Fason") {
                    if hasNonNumericUom then
                        Open("FSharp.UMX")

                    TypeDefn("JsonSerializer") {
                        for typ in typesWithSerializer do
                            JsonEncoderCodegen.generateTypeSerializer (ref typ)
                    }

                    ClassEnd ("TypeIdentifier", Constructor(UnitPat())) { }
                    |> _.typeParams(PostfixList($"'T"))

                    TypeDefn("JsonDeserializer") {
                        for typ in typesWithSerializer do
                            JsonEncoderCodegen.generateTypeDeserializer (ref typ)
                    }
                }
            }
            |> Gen.mkOak
            // Optimize the final oak.
            |> JsonEncoderCodegen.optimize


        oak |> Gen.run
