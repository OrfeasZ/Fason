module rec Fason.TypeCollector

open System
open System.Collections.Generic
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.UMX

/// A basic "built-in" type.
[<RequireQualifiedAccess>]
type BasicType =
    | Bool
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
    | Decimal
    | String
    | Guid
    | DateTime
    | TimeSpan
    | DateOnly
    | TimeOnly
    | DateTimeOffset
    | Unit

    member this.typeName: string<typeName> =
        match this with
        | Bool -> %"bool"
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
        | Decimal -> %"decimal"
        | String -> %"string"
        | Guid -> %"System.Guid"
        | DateTime -> %"System.DateTime"
        | TimeSpan -> %"System.TimeSpan"
        | DateOnly -> %"System.DateOnly"
        | TimeOnly -> %"System.TimeOnly"
        | DateTimeOffset -> %"System.DateTimeOffset"
        | Unit -> %"unit"

[<Measure>]
type typeName

/// A named field of a record type or union case.
type RecordField =
    { name: string
      fieldType: SerializableType ref
      defaultValue: obj option }

type AnonymousRecordType = { fields: RecordField list }

type RecordType =
    { name: string<typeName>
      typeArgs: SerializableType ref list
      fields: RecordField list }

type UnionCase =
    { name: string
      fields: RecordField list }

type UnionType =
    { name: string<typeName>
      typeArgs: SerializableType ref list
      cases: UnionCase list
      requireQualifiedAccess: bool }

type EnumValue = { name: string; value: obj }

type EnumType =
    { name: string<typeName>
      values: EnumValue list
      valueType: BasicType }

type TupleValue =
    { valueType: SerializableType ref
      defaultValue: obj option }

type TupleType = { values: TupleValue list }

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
let private types = Dictionary<FSharpType * int list, SerializableType ref>()

/// Keys of `types` in registration order, so a failed mapping can discard everything it registered.
let private registered = List<FSharpType * int list>()

let private refIds = Dictionary<SerializableType ref, int>(HashIdentity.Reference)

let private idOf (r: SerializableType ref) =
    match refIds.TryGetValue r with
    | true, id -> id
    | _ ->
        let id = refIds.Count
        refIds.Add(r, id)
        id

let private basicTypeMap =
    Map
        [ typeof<bool>.FullName, BasicType.Bool
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
          typeof<decimal>.FullName, BasicType.Decimal
          typeof<string>.FullName, BasicType.String
          typeof<Guid>.FullName, BasicType.Guid
          typeof<DateTime>.FullName, BasicType.DateTime
          typeof<TimeSpan>.FullName, BasicType.TimeSpan
          typeof<DateOnly>.FullName, BasicType.DateOnly
          typeof<TimeOnly>.FullName, BasicType.TimeOnly
          typeof<DateTimeOffset>.FullName, BasicType.DateTimeOffset
          typeof<unit>.FullName, BasicType.Unit ]

let rec private entityName (entity: FSharpEntity) =
    let own = PrettyNaming.NormalizeIdentifierBackticks entity.DisplayName

    match entity.DeclaringEntity, entity.Namespace with
    | Some parent, _ -> $"{entityName parent}.{own}"
    | None, Some ns ->
        let ns =
            ns.Split('.')
            |> Array.map PrettyNaming.NormalizeIdentifierBackticks
            |> String.concat "."

        $"{ns}.{own}"
    | None, None -> own

let private typeToTypeName (typ: FSharpType) : string<typeName> =
    if typ.HasTypeDefinition then
        %(entityName typ.TypeDefinition)
    else
        %typ.BasicQualifiedName

let private handleGenericType genericTypeArgs (typ: FSharpType) =
    if not typ.IsGenericParameter then
        typ |> typeFromFsharpType genericTypeArgs
    else
        match genericTypeArgs |> Map.tryFind typ.GenericParameter.Name with
        | None -> failwith $"Could not find generic type argument for {typ}"
        | Some actualType -> actualType

let private mapRecordFields genericTypeArgs (fields: FSharpField seq) =
    // TODO: Handle attributes
    fields
    |> Seq.filter (fun f -> not f.IsStatic)
    |> Seq.map (fun f ->
        { name = f.Name
          fieldType = f.FieldType |> handleGenericType genericTypeArgs
          defaultValue = None })
    |> Seq.toList

/// The generic arguments of the type in declaration order, and the bindings in scope
/// extended with them by parameter name.
let private getGenericTypeArgs genericTypeArgs (typ: FSharpType) =
    let genericArgs = typ.TypeDefinition.GenericArguments
    let genericParams = typ.TypeDefinition.GenericParameters

    if genericArgs.Count <> genericParams.Count then
        failwith
            $"Generic arguments and parameters count mismatch for {typ}. This probably means not all generic arguments were specified, which is not supported."

    let ordered =
        genericParams
        |> Seq.mapi (fun i p -> p.Name, genericArgs[i] |> handleGenericType genericTypeArgs)
        |> Seq.toList

    ordered |> List.map snd, ordered |> List.fold (fun acc (k, v) -> Map.add k v acc) genericTypeArgs

let private mapRecord genericTypeArgs (typ: FSharpType) =
    let typeArgs, genericTypeArgs = typ |> getGenericTypeArgs genericTypeArgs

    SerializableType.Record
        { name = typ |> typeToTypeName
          typeArgs = typeArgs
          fields = typ.TypeDefinition.FSharpFields |> mapRecordFields genericTypeArgs }

let private mapUnion genericTypeArgs (typ: FSharpType) =
    let typeArgs, genericTypeArgs = typ |> getGenericTypeArgs genericTypeArgs

    let requireQualifiedAccess =
        hasAttribute typeof<RequireQualifiedAccessAttribute> typ.TypeDefinition

    // The compiler rejects `Module.Type.Case` in patterns when the case is named like the
    // type, and RequireQualifiedAccess leaves no other qualified form.
    if
        requireQualifiedAccess
        && typ.TypeDefinition.UnionCases
           |> Seq.exists (fun uc -> uc.Name = typ.TypeDefinition.DisplayName)
    then
        failwith
            $"Union {typ} has RequireQualifiedAccess and a case named like the type, which cannot be pattern matched from outside its module"

    SerializableType.Union
        { name = typ |> typeToTypeName
          typeArgs = typeArgs
          cases =
            [ for uc in typ.TypeDefinition.UnionCases ->
                  { name = uc.Name
                    fields = uc.Fields |> mapRecordFields genericTypeArgs } ]
          requireQualifiedAccess = requireQualifiedAccess }

let private mapEnum (typ: FSharpType) =
    // TODO: Handle attributes
    let values =
        [ for f in typ.TypeDefinition.FSharpFields do
              if f.IsStatic && f.LiteralValue.IsSome then
                  { name = f.Name
                    value = f.LiteralValue.Value } ]

    let underlying = values.Head.value.GetType().FullName

    SerializableType.Enum
        { name = typ |> typeToTypeName
          values = values
          valueType =
            match basicTypeMap |> Map.tryFind underlying with
            | Some t -> t
            | None -> failwith $"Enum {typ} uses an unsupported type as its underlying type: {underlying}" }

/// Interfaces are not serialized themselves. Only their members' argument and return types are collected.
let private collectInterfaceMembers genericTypeArgs (typ: FSharpType) =
    // Curried members have the type `a -> (b -> ... -> result)`.
    let rec collect (t: FSharpType) =
        if t.IsFunctionType then
            t.GenericArguments[0] |> typeFromFsharpType genericTypeArgs |> ignore
            collect t.GenericArguments[1]
        else
            t |> typeFromFsharpType genericTypeArgs |> ignore

    for memb in typ.TypeDefinition.MembersFunctionsAndValues do
        collect memb.FullType

let private hasAttribute (attribute: Type) (entity: FSharpEntity) =
    entity.Attributes
    |> Seq.exists (fun a -> a.AttributeType.TryGetFullName() = Some attribute.FullName)

/// Wrappers that stand for their type argument: asynchronous results, and anything marked FasonUnwrap.
let private unwrappedByDefault =
    set
        [ "System.Threading.Tasks.Task`1"
          "System.Threading.Tasks.ValueTask`1"
          "Microsoft.FSharp.Control.FSharpAsync`1" ]

let private isUnwrapped (typ: FSharpType) =
    typ.HasTypeDefinition
    && (hasAttribute typeof<FasonUnwrapAttribute> typ.TypeDefinition
        || (typ.TypeDefinition.TryFullName |> Option.exists unwrappedByDefault.Contains))

let private typeFromFsharpType (genericTypeArgs: Map<string, SerializableType ref>) (typ: FSharpType) =
    let stripped = typ.StripAbbreviations()

    if isUnwrapped typ || isUnwrapped stripped then
        stripped.GenericArguments[0] |> handleGenericType genericTypeArgs
    else

        let typ = stripped
        let key = typ, (genericTypeArgs |> Map.toList |> List.map (snd >> idOf))

        match types.TryGetValue key with
        | true, existing -> existing
        | _ ->
            // Register the ref before mapping so recursive types terminate.
            let typRef = ref (SerializableType.Basic BasicType.Unit)
            let mark = registered.Count
            types.Add(key, typRef)
            registered.Add key

            let arg i =
                typ.GenericArguments[i] |> handleGenericType genericTypeArgs

            let named = typ.HasTypeDefinition
            let definition = if named then Some typ.TypeDefinition else None

            try
                typRef.Value <-
                    match
                        definition
                        |> Option.bind (fun d ->
                            if d.IsArrayType then
                                None
                            else
                                basicTypeMap.TryFind typ.BasicQualifiedName)
                    with
                    | Some basic -> SerializableType.Basic basic
                    | None when typ.GenericArguments.Count > 0 && typ.GenericArguments[0].IsMeasureType ->
                        SerializableType.UnitOfMeasure
                            { baseType = typ.ErasedType |> handleGenericType genericTypeArgs
                              unitOfMeasure = typ.GenericArguments[0] |> typeToTypeName }
                    | None when typ.IsAnonRecordType ->
                        // TODO: Handle attributes
                        SerializableType.AnonymousRecord
                            { fields =
                                [ for i, name in typ.AnonRecordTypeDetails.SortedFieldNames |> Seq.indexed ->
                                      { name = name
                                        fieldType = arg i
                                        defaultValue = None } ] }
                    | None when typ.IsTupleType ->
                        // TODO: Handle attributes
                        SerializableType.Tuple
                            { values =
                                [ for i in 0 .. typ.GenericArguments.Count - 1 ->
                                      { valueType = arg i
                                        defaultValue = None } ] }
                    | None ->
                        match definition with
                        | Some d when d.CompiledName = "FSharpOption`1" -> SerializableType.Optional(arg 0)
                        | Some d when d.CompiledName = "FSharpList`1" -> SerializableType.List(arg 0)
                        | Some d when d.CompiledName = "FSharpSet`1" -> SerializableType.Set(arg 0)
                        | Some d when d.CompiledName = "FSharpMap`2" -> SerializableType.Map(arg 0, arg 1)
                        | Some d when d.IsFSharpRecord -> mapRecord genericTypeArgs typ
                        | Some d when d.IsFSharpUnion -> mapUnion genericTypeArgs typ
                        | Some d when d.IsEnum -> mapEnum typ
                        | Some d when d.IsArrayType -> SerializableType.Array(arg 0)
                        | Some d when d.IsInterface ->
                            collectInterfaceMembers genericTypeArgs typ
                            SerializableType.Basic BasicType.Unit
                        | _ -> failwith $"Unsupported type {typ}"

            with ex ->
                eprintfn $"Skipping {typ}: {ex.Message}"

                for i in mark .. registered.Count - 1 do
                    types.Remove registered[i] |> ignore

                registered.RemoveRange(mark, registered.Count - mark)
                reraise ()

            typRef

let rec private collectAll (entity: FSharpEntity) =
    // Open generic definitions are reached through their instantiations, and unwrapped
    // wrappers only stand for their argument. Neither has a codec of its own.
    let skipped =
        entity.IsFSharpModule
        || entity.IsMeasure
        || entity.GenericParameters.Count > 0
        || hasAttribute typeof<FasonUnwrapAttribute> entity

    if not skipped then
        try
            entity.AsType() |> typeFromFsharpType Map.empty |> ignore
        with _ ->
            ()

    for child in entity.NestedEntities do
        collectAll child

/// Collects the entities carrying the FasonSerializable attribute, together with
/// everything nested in them.
let rec collectFrom (entity: FSharpEntity) =
    if hasAttribute typeof<FasonSerializableAttribute> entity then
        collectAll entity
    else
        for child in entity.NestedEntities do
            collectFrom child

let getSerializableTypes () = types |> Seq.map _.Value |> Seq.toArray

let reset () =
    types.Clear()
    registered.Clear()
    refIds.Clear()
