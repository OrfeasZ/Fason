namespace Fason

open Fason.TypeCollector
open Fabulous.AST
open type Fabulous.AST.Ast
open FSharp.UMX

type JsonEncoderCodegen =
    static member private generateBasicEncoder(b: BasicType) =
        match b with
        | BasicType.Bool -> ()
        | _ -> ()

    static member private getTypeName(typ: SerializableType ref) =
        match typ.Value with
        | SerializableType.Basic b -> b.typeName
        | SerializableType.AnonymousRecord r -> %($$"""{| todo: string |}""") // TODO
        | SerializableType.Record r -> r.name
        | SerializableType.Union u -> u.name
        | SerializableType.Enum e -> e.name
        | SerializableType.Tuple t -> %($$"""string * int""") // TODO
        | SerializableType.Array a -> %($"{a |> JsonEncoderCodegen.getTypeName} array")
        | SerializableType.List l -> %($"{l |> JsonEncoderCodegen.getTypeName} list")
        | SerializableType.Set s -> %($"Set<{s |> JsonEncoderCodegen.getTypeName}>")
        | SerializableType.Map(k, v) ->
            %($"Map<{k |> JsonEncoderCodegen.getTypeName}, {v |> JsonEncoderCodegen.getTypeName}>")
        | SerializableType.UnitOfMeasure uom ->
            %($"{uom.baseType |> JsonEncoderCodegen.getTypeName}<{uom.unitOfMeasure}>")
        | SerializableType.Optional o -> %($"{o |> JsonEncoderCodegen.getTypeName} option")

    static member private generateType(typ: SerializableType ref) =
        let valueType, serializeExpr = JsonEncoderCodegen.getTypeName typ, ()

        Member("serialize", ParenPat(ParameterPat("value", valueType.ToString())), String($"{valueType}"))
            .toStatic ()

    static member private generateTypeDes(typ: SerializableType ref) =
        let valueType = JsonEncoderCodegen.getTypeName typ

        Member(
            "deserialize",
            ParenPat(ParameterPat("_", $"TypeIdentifier<{valueType.ToString()}>")),
            String($"{valueType}")
        )
            .toStatic ()

    static member generate(types: SerializableType ref array) =
        // If we have any non-numeric uoms, add an import to FSharp.UMX.
        // TODO: This should work recursively, for things like fields of records, lists, etc.
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
                    | other -> other

                | SerializableType.List l ->
                    match l.Value with
                    | SerializableType.UnitOfMeasure uom -> SerializableType.List uom.baseType
                    | other -> other

                | SerializableType.Set s ->
                    match s.Value with
                    | SerializableType.UnitOfMeasure uom -> SerializableType.Set uom.baseType
                    | other -> other

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
                    | other -> other
                | other -> other)
            |> Seq.distinct

        Oak() {
            Namespace("Fason") {
                if hasNonNumericUom then
                    Open("FSharp.UMX")

                TypeDefn("JsonSerializer") {
                    for typ in typesWithSerializer do
                        JsonEncoderCodegen.generateType (ref typ)
                }

                StructEnd ("TypeIdentifier", Constructor(UnitPat())) { }
                |> _.typeParams(PostfixList($"'T"))

                TypeDefn("JsonDeserializer") {
                    for typ in typesWithSerializer do
                        JsonEncoderCodegen.generateTypeDes (ref typ)
                }
            }
        }
        |> Gen.mkOak
        |> Gen.run
