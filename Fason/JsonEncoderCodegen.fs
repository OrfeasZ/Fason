namespace Fason

open System
open System.IO
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

    static member private generateTypeDeserializer(typ: SerializableType ref) =
        let valueType = JsonEncoderCodegen.getTypeName typ

        Member(
            "deserialize",
            ParenPat(ParameterPat("_", $"TypeIdentifier<{valueType.ToString()}>")),
            String($"{valueType}")
        )
            .toStatic ()

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
