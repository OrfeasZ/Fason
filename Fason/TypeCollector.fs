module rec Fason.TypeCollector

open System
open System.Collections.Generic
open FSharp.Compiler.Symbols
open FSharp.UMX

/// A basic "built-in" type.
[<RequireQualifiedAccess>]
type BasicType =
    | Bool
    | Byte
    | SByte
    | Char
    | Int8
    | Int16
    | Int32
    | Int64
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | Single
    | Double
    | String
    | Guid
    | DateTime
    | TimeSpan
    | Unit

    member this.typeName: string<typeName> =
        match this with
        | Bool -> %"bool"
        | Byte -> %"byte"
        | SByte -> %"sbyte"
        | Char -> %"char"
        | Int8 -> %"int8"
        | Int16 -> %"int16"
        | Int32 -> %"int32"
        | Int64 -> %"int64"
        | UInt8 -> %"uint8"
        | UInt16 -> %"uint16"
        | UInt32 -> %"uint32"
        | UInt64 -> %"uint64"
        | Single -> %"single"
        | Double -> %"double"
        | String -> %"string"
        | Guid -> %"System.Guid"
        | DateTime -> %"System.DateTime"
        | TimeSpan -> %"System.TimeSpan"
        | Unit -> %"unit"

[<Measure>]
type typeName

/// A named field of a record type or union case.
type RecordField =
    { name: string
      fieldType: SerializableType ref
      defaultValue: obj option }

/// An anonymous record type.
type AnonymousRecordType = { fields: RecordField list }

/// A record type.
type RecordType =
    { name: string<typeName>
      fields: RecordField list }

/// A case of a union type.
type UnionCase =
    { name: string
      fields: RecordField list }

/// A union type.
type UnionType =
    { name: string<typeName>
      cases: UnionCase list }

/// A value of an enum type.
type EnumValue = { name: string; value: obj }

/// An enum type.
type EnumType =
    { name: string<typeName>
      values: EnumValue list
      valueType: BasicType }

/// A value of a tuple type.
type TupleValue =
    { valueType: SerializableType ref
      defaultValue: obj option }

/// A tuple type.
type TupleType = { values: TupleValue list }

/// A unit of measure type.
type UomType =
    { baseType: SerializableType ref
      unitOfMeasure: string<typeName> }

[<RequireQualifiedAccess>]
type SerializableType =
    | Basic of BasicType
    | AnonymousRecord of AnonymousRecordType
    | Record of RecordType
    | Union of UnionType
    | Enum of EnumType
    | Tuple of TupleType
    | Array of SerializableType ref
    | List of SerializableType ref
    | Set of SerializableType ref
    | Map of key: SerializableType ref * value: SerializableType ref
    | UnitOfMeasure of UomType
    | Optional of SerializableType ref

/// Collection of all types that are serializable, that we've collected so far.
let private types = Dictionary<FSharpType, SerializableType ref>()

let private basicTypeMap =
    Map
        [ typeof<bool>.FullName, BasicType.Bool
          typeof<byte>.FullName, BasicType.Byte
          typeof<sbyte>.FullName, BasicType.SByte
          typeof<char>.FullName, BasicType.Char
          typeof<int8>.FullName, BasicType.Int8
          typeof<int16>.FullName, BasicType.Int16
          typeof<int32>.FullName, BasicType.Int32
          typeof<int64>.FullName, BasicType.Int64
          typeof<uint8>.FullName, BasicType.UInt8
          typeof<uint16>.FullName, BasicType.UInt16
          typeof<uint32>.FullName, BasicType.UInt32
          typeof<uint64>.FullName, BasicType.UInt64
          typeof<single>.FullName, BasicType.Single
          typeof<double>.FullName, BasicType.Double
          typeof<string>.FullName, BasicType.String
          typeof<Guid>.FullName, BasicType.Guid
          typeof<DateTime>.FullName, BasicType.DateTime
          typeof<TimeSpan>.FullName, BasicType.TimeSpan
          typeof<unit>.FullName, BasicType.Unit ]

let private typeToTypeName (typ: FSharpType) =
    try
        %(typ.TypeDefinition :> FSharpSymbol).FullName
    with _ ->
        try
            %typ.BasicQualifiedName
        with _ ->
            failwith $"Could not get type name for {typ}"

let private tryMapBasic genericTypeArgs (typ: FSharpType) =
    try
        typ.StripAbbreviations().BasicQualifiedName
        |> basicTypeMap.TryFind
        |> Option.map SerializableType.Basic
    with ex ->
        None

let private handleGenericType genericTypeArgs (typ: FSharpType) =
    if not typ.IsGenericParameter then
        typ |> typeFromFsharpType genericTypeArgs
    else
        match genericTypeArgs |> Map.tryFind typ.GenericParameter.Name with
        | None -> failwith $"Could not find generic type argument for {typ}"
        | Some actualType -> actualType

let private tryMapOptional genericTypeArgs (typ: FSharpType) =
    let genericArgs = typ.GenericArguments |> Seq.toArray

    if genericArgs.Length <> 1 || typ.TypeDefinition.CompiledName <> "FSharpOption`1" then
        None
    else
        let elementType = typ.GenericArguments[0] |> handleGenericType genericTypeArgs
        Some(SerializableType.Optional elementType)

let private tryMapList genericTypeArgs (typ: FSharpType) =
    let genericArgs = typ.GenericArguments |> Seq.toArray

    if genericArgs.Length <> 1 || typ.TypeDefinition.CompiledName <> "FSharpList`1" then
        None
    else
        let elementType = typ.GenericArguments[0] |> handleGenericType genericTypeArgs
        Some(SerializableType.List elementType)

let private tryMapSet genericTypeArgs (typ: FSharpType) =
    let genericArgs = typ.GenericArguments |> Seq.toArray

    if genericArgs.Length <> 1 || typ.TypeDefinition.CompiledName <> "FSharpSet`1" then
        None
    else
        let elementType = typ.GenericArguments[0] |> handleGenericType genericTypeArgs
        Some(SerializableType.Set elementType)

let private tryMapUom genericTypeArgs (typ: FSharpType) =
    if typ.GenericArguments.Count = 0 || not typ.GenericArguments[0].IsMeasureType then
        None
    else
        let baseType = typ.ErasedType |> handleGenericType genericTypeArgs
        let uomType = typ.GenericArguments[0]
        let uomTypeName = uomType |> typeToTypeName

        Some(
            SerializableType.UnitOfMeasure
                { baseType = baseType
                  unitOfMeasure = uomTypeName }
        )

let private tryMapAnonRecord genericTypeArgs (typ: FSharpType) =
    if not typ.IsAnonRecordType then
        None
    else
        let argTypes = typ.GenericArguments |> Seq.toArray

        // TODO: Handle attributes
        let fields =
            typ.AnonRecordTypeDetails.SortedFieldNames
            |> Seq.mapi (fun i name ->
                { name = name
                  fieldType = argTypes[i] |> handleGenericType genericTypeArgs
                  defaultValue = None })
            |> Seq.toList

        Some(SerializableType.AnonymousRecord { fields = fields })

let private mapRecordFields genericTypeArgs (fields: FSharpField seq) =
    // TODO: Handle attributes
    fields
    |> Seq.filter (fun f -> not f.IsStatic)
    |> Seq.map (fun f ->
        { name = f.Name
          fieldType = f.FieldType |> handleGenericType genericTypeArgs
          defaultValue = None })
    |> Seq.toList

let private getGenericTypeArgs genericTypeArgs (typ: FSharpType) =
    let genericArgs = typ.TypeDefinition.GenericArguments
    let genericParams = typ.TypeDefinition.GenericParameters

    if genericArgs.Count <> genericParams.Count then
        failwith
            $"Generic arguments and parameters count mismatch for {typ}. This probably means not all generic arguments were specified, which is not supported."

    genericParams
    |> Seq.mapi (fun i p -> p.Name, genericArgs[i] |> handleGenericType genericTypeArgs)
    |> Map.ofSeq

let inline private mergeMaps (m1: Map<'a, 'b>) (m2: Map<'a, 'b>) =
    m1 |> Map.fold (fun acc k v -> acc |> Map.add k v) m2

let private tryMapRecord genericTypeArgs (typ: FSharpType) =
    if not typ.TypeDefinition.IsFSharpRecord then
        None
    else
        let ourGenericTypeArgs = typ |> getGenericTypeArgs genericTypeArgs
        let genericTypeArgs = genericTypeArgs |> mergeMaps ourGenericTypeArgs

        Some(
            SerializableType.Record
                { name = typ |> typeToTypeName
                  fields = typ.TypeDefinition.FSharpFields |> mapRecordFields genericTypeArgs }
        )

let private tryMapUnion genericTypeArgs (typ: FSharpType) =
    if not typ.TypeDefinition.IsFSharpUnion then
        None
    else
        let ourGenericTypeArgs = typ |> getGenericTypeArgs genericTypeArgs
        let genericTypeArgs = genericTypeArgs |> mergeMaps ourGenericTypeArgs

        let cases =
            typ.TypeDefinition.UnionCases
            |> Seq.map (fun uc ->
                { name = uc.Name
                  fields = uc.Fields |> mapRecordFields genericTypeArgs })
            |> Seq.toList

        Some(
            SerializableType.Union
                { name = typ |> typeToTypeName
                  cases = cases }
        )

let private tryMapEnum genericTypeArgs (typ: FSharpType) =
    if not typ.TypeDefinition.IsEnum then
        None
    else
        // TODO: Attributes
        let valueFields =
            typ.TypeDefinition.FSharpFields
            |> Seq.filter (fun f -> f.IsStatic && f.LiteralValue.IsSome)
            |> Seq.toList

        let values =
            valueFields
            |> Seq.map (fun f ->
                { name = f.Name
                  value = f.LiteralValue.Value })
            |> Seq.toList

        let enumType =
            match basicTypeMap |> Map.tryFind (values.Head.value.GetType().FullName) with
            | None ->
                failwith
                    $"Enum {typ} uses an unsupported type as its underlying type: {values.Head.value.GetType().FullName}"

            | Some t -> t

        Some(
            SerializableType.Enum
                { name = typ |> typeToTypeName
                  values = values
                  valueType = enumType }
        )

let private tryMapTuple genericTypeArgs (typ: FSharpType) =
    if not typ.IsTupleType then
        None
    else
        let types = typ.GenericArguments |> Seq.map (handleGenericType genericTypeArgs)

        // TODO: Handle attributes
        Some(
            SerializableType.Tuple
                { values = types |> Seq.map (fun t -> { valueType = t; defaultValue = None }) |> Seq.toList }
        )

let private tryMapArray genericTypeArgs (typ: FSharpType) =
    if not typ.TypeDefinition.IsArrayType then
        None
    else
        let elementType = typ.GenericArguments[0] |> handleGenericType genericTypeArgs

        Some(SerializableType.Array elementType)

let private tryMapMap genericTypeArgs (typ: FSharpType) =
    let genericArgs = typ.GenericArguments |> Seq.toArray

    if genericArgs.Length <> 2 || typ.TypeDefinition.CompiledName <> "FSharpMap`2" then
        None
    else
        let keyType = typ.GenericArguments[0] |> handleGenericType genericTypeArgs
        let valueType = typ.GenericArguments[1] |> handleGenericType genericTypeArgs

        Some(SerializableType.Map(keyType, valueType))

let private tryMapInterface genericTypeArgs (typ: FSharpType) =
    if not typ.TypeDefinition.IsInterface then
        None
    else
        // Collect all types from members.
        for memb in typ.TypeDefinition.MembersFunctionsAndValues do
            let args = memb.FullType.GenericArguments[0]

            let returnType = memb.FullType.GenericArguments[1]

            // TODO: Remove this when releasing publicly.
            // If returnType is wrapped in AsyncAction<>, unwrap it.
            let returnType =
                if returnType.TypeDefinition.CompiledName = "AsyncAction`1" then
                    returnType.GenericArguments[0]
                else
                    returnType

            // We just call these to collect the types.
            let _argTyp = args |> typeFromFsharpType genericTypeArgs
            let _returnType = returnType |> typeFromFsharpType genericTypeArgs

            ()

        Some(SerializableType.Basic BasicType.Unit)

let private typeFromFsharpType (genericTypeArgs: Map<string, SerializableType ref>) (typ: FSharpType) =
    let typ = typ.StripAbbreviations()

    let found, serializableType = types.TryGetValue typ

    if found then
        serializableType
    else
        printfn $"Trying to map type: {typ}"

        let sTyp =
            typ
            |> tryMapBasic genericTypeArgs
            |> Option.orElseWith (fun () -> tryMapUom genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapAnonRecord genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapOptional genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapList genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapSet genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapTuple genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapRecord genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapUnion genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapEnum genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapArray genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapMap genericTypeArgs typ)
            |> Option.orElseWith (fun () -> tryMapInterface genericTypeArgs typ)
            |> Option.defaultWith (fun () -> failwith $"Unsupported type {typ}")

        let typRef = ref sTyp
        types.Add(typ, typRef)
        typRef

let collectFrom (entity: FSharpEntity) =
    if not entity.IsFSharpModule && not entity.IsMeasure then
        try
            let typ = entity.AsType().StripAbbreviations()
            let serializableType = typ |> typeFromFsharpType Map.empty

            printfn $"Entity {entity}: %A{serializableType}"
        with ex ->
            eprintfn $"Unsupported entity {entity}: %A{ex}"

    for child in entity.NestedEntities do
        collectFrom child

let getSerializableTypes () = types |> Seq.map _.Value |> Seq.toArray
