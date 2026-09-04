namespace Fason

open System
open System.Collections.Generic
open Fason.TypeCollector
open Fabulous.AST
open type Fabulous.AST.Ast
open FSharp.Compiler.Syntax
open FSharp.UMX

/// A radix tree over a set of strings. Given "small", "smaller", "smile", "address",
/// "ad" and "banana" the tree is:
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
type RadixTreeNode =
    { data: string
      isLeaf: bool
      children: RadixTreeNode list }

module RadixTree =
    let empty =
        { data = ""
          isLeaf = false
          children = [] }

    let private commonPrefixLength (a: string) (b: string) =
        let mutable i = 0

        while i < min a.Length b.Length && a[i] = b[i] do
            i <- i + 1

        i

    let rec insert (key: string) (node: RadixTreeNode) =
        if key = "" then
            { node with isLeaf = true }
        else
            match node.children |> List.tryFind (fun c -> commonPrefixLength key c.data > 0) with
            | None ->
                { node with
                    children =
                        { data = key
                          isLeaf = true
                          children = [] }
                        :: node.children }
            | Some child ->
                let prefix = commonPrefixLength key child.data

                let updated =
                    if prefix = child.data.Length then
                        insert (key.Substring prefix) child
                    else
                        // Split the edge at the prefix, so the rest of the old edge becomes a child.
                        { data = child.data.Substring(0, prefix)
                          isLeaf = false
                          children =
                            [ { child with
                                  data = child.data.Substring prefix } ] }
                        |> insert (key.Substring prefix)

                { node with
                    children = updated :: (node.children |> List.filter (fun c -> c <> child)) }

    let fromStrings (strings: string seq) =
        strings |> Seq.fold (fun node s -> insert s node) empty

    /// Sorts children by edge length, then alphabetically.
    let rec sort (node: RadixTreeNode) =
        { node with
            children = node.children |> List.map sort |> List.sortBy (fun n -> n.data.Length, n.data) }

module private Emit =
    type Statement = WidgetBuilder<Fantomas.Core.SyntaxOak.ComputationExpressionStatement>
    type Expression = WidgetBuilder<Fantomas.Core.SyntaxOak.Expr>
    type Pattern = WidgetBuilder<Fantomas.Core.SyntaxOak.Pattern>

    let ident (name: string) : Expression = IdentExpr name

    /// Forward an identifier for F#. Keywords and names with things like spaces get double backticks.
    let codeName (name: string) =
        PrettyNaming.NormalizeIdentifierBackticks name

    let str (value: string) : Expression = ConstantExpr(String value)
    let chr (value: char) : Expression = ConstantExpr(Char value)
    let int (value: int) : Expression = ConstantExpr(Int value)
    let hex (value: uint32) : Expression = ConstantExpr(Constant $"0x{value:X8}u")
    let unit: Expression = UnitExpr()
    let paren (expr: Expression) : Expression = ParenExpr expr
    let tuple (items: Expression list) : Expression = TupleExpr items
    let array (items: Expression list) : Expression = ArrayExpr items
    let list (items: Expression list) : Expression = ListExpr items
    let infix (left: Expression) (op: string) (right: Expression) : Expression = InfixAppExpr(left, op, right)
    let eq (left: Expression) (right: Expression) = infix left "=" right
    let pipe (value: Expression) (fn: Expression) : Expression = PipeRightExpr(value, fn)
    let set (target: Expression) (value: Expression) : Expression = SetExpr(target, value)
    let index (arr: Expression) (i: Expression) : Expression = IndexWithoutDotExpr(arr, i)

    let typeApp (fn: string) (typeArgs: string list) : Expression =
        TypeAppExpr(ident fn, typeArgs |> List.map LongIdent)

    let typed (expr: Expression) (typ: string) : Expression = TypedExpr(expr, ":", LongIdent typ)
    let upcastTo (expr: Expression) (typ: string) = infix expr ":>" (ident typ)
    let downcastTo (expr: Expression) (typ: string) = infix expr ":?>" (ident typ)
    let lambda (parameters: string list) (body: Expression) : Expression = LambdaExpr(parameters, body)

    /// A call of a function or method. Several arguments form a parenthesized tuple, so a
    /// compound single argument must be parenthesized by the caller.
    let call (fn: Expression) (args: Expression list) : Expression =
        match args with
        | [] -> AppExpr(fn, unit)
        | [ single ] -> AppExpr(fn, single)
        | many -> AppExpr(fn, paren (tuple many))

    /// A call of a named function with the arguments applied one by one.
    let curried (fn: string) (args: Expression list) : Expression = AppExpr(ident fn, args)
    let notExpr (expr: Expression) = curried "not" [ paren expr ]
    let boxed (expr: Expression) = curried "box" [ paren expr ]
    let some (expr: Expression) = curried "Some" [ paren expr ]

    let stmt (expr: Expression) : Statement = OtherExpr expr

    let letValue (name: string) (expr: Expression) : Statement =
        LetOrUseExpr(Value(NamedPat name, expr))

    let letMutable (name: string) (expr: Expression) : Statement =
        LetOrUseExpr(Value(NamedPat name, expr) |> _.toMutable())

    let letTuple (names: string list) (expr: Expression) : Statement =
        LetOrUseExpr(Value(TuplePat(names |> List.map NamedPat), expr))

    let block (statements: Statement list) : Expression = CompExprBodyExpr statements
    let ifThen (condition: Expression) (body: Expression) : Expression = IfThenExpr(condition, body)

    let ifElse (condition: Expression) (thenBody: Expression) (elseBody: Expression) : Expression =
        IfThenElseExpr(condition, thenBody, elseBody)

    let whileLoop (condition: Expression) (body: Statement list) : Expression = WhileExpr(condition, block body)

    let forEach (item: string) (source: Expression) (body: Expression) : Expression = ForEachDoExpr(item, source, body)

    let forTo (counter: string) (from: Expression) (upTo: Expression) (body: Expression) : Expression =
        ForToExpr(counter, from, upTo, body)

    let matchOn (scrutinee: Expression) (clauses: (Pattern * Expression) list) : Expression =
        MatchExpr(scrutinee, [ for pattern, body in clauses -> MatchClauseExpr(pattern, body) ])

    let patNamed (name: string) : Pattern = NamedPat name
    let patWild: Pattern = WildPat()
    let patStr (value: string) : Pattern = ConstantPat(String value)
    let patChar (value: char) : Pattern = ConstantPat(Char value)
    let patTuple (items: Pattern list) : Pattern = TuplePat items

    /// `Case` or `Case(p0, p1)`.
    let patCase (name: string) (args: string list) : Pattern =
        match args with
        | [] -> LongIdentPat name
        | args -> LongIdentPat(name, [ ParenPat(TuplePat(args |> List.map NamedPat)) ])

    /// A JSON string literal for a field or case name, used inside larger JSON text.
    let jsonString (value: string) =
        let sb = Text.StringBuilder("\"")

        for c in value do
            match c with
            | '"' -> sb.Append "\\\"" |> ignore
            | '\\' -> sb.Append "\\\\" |> ignore
            | c when c < ' ' -> sb.Append($"\\u{int32 c:X4}") |> ignore
            | c -> sb.Append c |> ignore

        sb.Append('"').ToString()

    let private parameters (parameters: (string * string) list) =
        if parameters.IsEmpty then
            UnitPat()
        else
            ParenPat(TuplePat [ for p, t in parameters -> ParameterPat(p, t) ])

    let staticMember (name: string) (ps: (string * string) list) (returnType: string) (body: Expression) =
        Member(name, parameters ps, body, LongIdent returnType).toStatic ()

    let letFunction (name: string) (ps: (string * string) list) (returnType: string) (body: Expression) =
        Function(name, parameters ps, body, LongIdent returnType)

    let directives (before: string, after: string) (decl: WidgetBuilder<'T>) =
        decl |> _.triviaBefore(Directive before) |> _.triviaAfter(Directive after)

open Emit

/// Output of a streaming serializer: literal JSON text, or a statement. Consecutive
/// literals are merged into one `WritePlain` when the statements are assembled.
type Out =
    | Plain of string
    | Code of Statement

module JsonEncoderCodegen =
    /// When set, the JS codecs use the engine's date functions, which is faster under Hermes.
    let mutable hermes = false

    // ---------------------------------------------------------------------------------
    // Type names and units of measure
    // ---------------------------------------------------------------------------------

    let rec private getTypeName (typ: SerializableType ref) : string<typeName> =
        let name (t: SerializableType ref) = string (getTypeName t)

        let generic (n: string<typeName>) (args: SerializableType ref list) =
            if args.IsEmpty then
                n
            else
                let argNames = args |> List.map name |> String.concat ", "
                %($"{n}<{argNames}>")

        match typ.Value with
        | SerializableType.Basic b -> b.typeName
        | SerializableType.AnonymousRecord r ->
            %("{| "
              + (r.fields
                 |> List.map (fun f -> $"{codeName f.name}: {name f.fieldType}")
                 |> String.concat "; ")
              + " |}")
        | SerializableType.Record r -> generic r.name r.typeArgs
        | SerializableType.Union u -> generic u.name u.typeArgs
        | SerializableType.Enum e -> e.name
        | SerializableType.Tuple t ->
            let parts = t.values |> List.map (fun v -> name v.valueType) |> String.concat " * "
            %($"({parts})")
        | SerializableType.Array a -> %($"{name a} array")
        | SerializableType.List l -> %($"{name l} list")
        | SerializableType.Set s -> %($"Set<{name s}>")
        | SerializableType.Map(k, v) -> %($"Map<{name k}, {name v}>")
        | SerializableType.UnitOfMeasure uom -> %($"{name uom.baseType}<{uom.unitOfMeasure}>")
        | SerializableType.Optional o -> %($"{name o} option")

    /// Qualified name of a union case. When a case shares the type's name, `Type.Other`
    /// resolves `Type` to that case, so the cases go through the enclosing module instead.
    /// RequireQualifiedAccess keeps the case out of the module's scope, and `Type.Case` works.
    let private unionCaseName (typ: UnionType) (caseName: string) =
        let caseName = codeName caseName
        let typeName = string typ.name
        let lastDot = typeName.LastIndexOf '.'
        let ownName = typeName.Substring(lastDot + 1)
        let shadowed = typ.cases |> List.exists (fun c -> codeName c.name = ownName)

        if shadowed && not typ.requireQualifiedAccess then
            if lastDot < 0 then
                caseName
            else
                $"{typeName.Substring(0, lastDot)}.{caseName}"
        else
            $"{typeName}.{caseName}"

    let private stripped =
        Dictionary<SerializableType ref, SerializableType ref>(HashIdentity.Reference)

    /// Recursively removes units of measure. Uom types are erased at runtime, so codecs
    /// are generated for the erased type and values are retyped at the call sites.
    let rec private stripUom (typ: SerializableType ref) : SerializableType ref =
        let stripFields fields =
            fields
            |> List.map (fun f ->
                { f with
                    fieldType = stripUom f.fieldType })

        // Generic records and unions are registered before recursing so self-referential types terminate.
        let memoized build =
            let result = ref typ.Value
            stripped.Add(typ, result)
            result.Value <- build ()
            result

        match stripped.TryGetValue typ with
        | true, s -> s
        | _ ->
            match typ.Value with
            | SerializableType.UnitOfMeasure uom -> stripUom uom.baseType
            | SerializableType.Array a -> ref (SerializableType.Array(stripUom a))
            | SerializableType.List l -> ref (SerializableType.List(stripUom l))
            | SerializableType.Set s -> ref (SerializableType.Set(stripUom s))
            | SerializableType.Optional o -> ref (SerializableType.Optional(stripUom o))
            | SerializableType.Map(k, v) -> ref (SerializableType.Map(stripUom k, stripUom v))
            | SerializableType.Tuple t ->
                ref (
                    SerializableType.Tuple
                        { values =
                            t.values
                            |> List.map (fun v ->
                                { v with
                                    valueType = stripUom v.valueType }) }
                )
            | SerializableType.AnonymousRecord r ->
                ref (SerializableType.AnonymousRecord { fields = stripFields r.fields })
            | SerializableType.Record r when not r.typeArgs.IsEmpty ->
                memoized (fun () ->
                    SerializableType.Record
                        { r with
                            typeArgs = r.typeArgs |> List.map stripUom
                            fields = stripFields r.fields })
            | SerializableType.Union u when not u.typeArgs.IsEmpty ->
                memoized (fun () ->
                    SerializableType.Union
                        { u with
                            typeArgs = u.typeArgs |> List.map stripUom
                            cases = u.cases |> List.map (fun c -> { c with fields = stripFields c.fields }) })
            | _ -> typ

    /// The erased type behind `typ`, with the names of both. They differ only with units of measure.
    let private erased (typ: SerializableType ref) =
        let strippedType = stripUom typ
        strippedType, getTypeName strippedType, getTypeName typ

    let private retype (expr: Expression) (toType: string<typeName>) =
        paren (typed (curried "retype" [ expr ]) (string toType))

    let private mangle (name: string<typeName>) =
        string name
        |> String.map (fun c -> if Char.IsLetterOrDigit c || c = '_' then c else '_')

    let private serializerName name = $"serialize_{mangle name}"
    let private deserializerName name = $"deserialize_{mangle name}"
    let private toJsName name = $"toJs_{mangle name}"
    let private fromJsName name = $"fromJs_{mangle name}"

    /// Maps keyed by strings or Guids are JSON objects, others arrays of pairs, as in Thoth.
    let private hasObjectKeys (keyType: SerializableType ref) =
        match (stripUom keyType).Value with
        | SerializableType.Basic BasicType.String
        | SerializableType.Basic BasicType.Guid -> true
        | _ -> false

    let private reader (name: string) (args: Expression list) = call (ident $"reader.{name}") args
    let private writer (name: string) (args: Expression list) = call (ident $"writer.{name}") args

    let private platform (name: string) (args: Expression list) =
        call (ident $"Fason.Platform.{name}") args

    let private jsValue (name: string) (args: Expression list) =
        call (ident $"Fason.JsValue.{name}") args

    // ---------------------------------------------------------------------------------
    // Streaming serializers
    // ---------------------------------------------------------------------------------

    let private writePlain (s: string) = stmt (writer "WritePlain" [ str s ])

    /// Turns serializer output into statements, merging runs of literal text.
    let private writes (outs: Out list) =
        let flush pending acc =
            if pending = "" then acc else writePlain pending :: acc

        let rec go pending acc outs =
            match outs with
            | Plain s :: rest -> go (pending + s) acc rest
            | Code c :: rest -> go "" (c :: flush pending acc) rest
            | [] -> List.rev (flush pending acc)

        go "" [] outs

    /// Writes `value` of the given type.
    let private serialize (typ: SerializableType ref) (value: Expression) =
        let strippedType, strippedName, name = erased typ

        let value =
            if strippedName = name then
                value
            else
                retype value strippedName

        match strippedType.Value with
        | SerializableType.Basic BasicType.Unit -> Plain "null"
        | SerializableType.Basic _ -> Code(stmt (writer "Write" [ paren value ]))
        | _ -> Code(stmt (call (ident $"Codecs.{serializerName strippedName}") [ value; ident "writer" ]))

    /// The body of a loop over elements: a separator before all but the first.
    let private separated (separator: Expression) (body: Statement list) =
        stmt (ifElse (ident "first") (set (ident "first") (ident "false")) separator)
        :: body

    let private commaSeparated = separated (writer "WritePlain" [ str "," ])

    let private recordSerializer (fields: RecordField list) =
        // None fields are omitted, as Thoth does. Whether a comma is needed is known
        // statically until the first optional field, and again after the next required one.
        // Some true means always, Some false never, and None leaves it to `needsComma` at runtime.
        let mutable commaState = Some false

        writes
            [ Plain "{"

              if fields |> List.exists (fun f -> f.fieldType.Value.IsOptional) then
                  Code(letMutable "needsComma" (ident "false"))

              for field in fields do
                  let name = jsonString field.name + ":"

                  let separator =
                      match commaState with
                      | Some true -> [ Plain "," ]
                      | Some false -> []
                      | None -> [ Code(stmt (ifThen (ident "needsComma") (writer "WritePlain" [ str "," ]))) ]

                  match field.fieldType.Value with
                  | SerializableType.Optional inner ->
                      let written =
                          [ yield! separator
                            Plain name
                            serialize inner (ident "v")
                            if commaState <> Some true then
                                Code(stmt (set (ident "needsComma") (ident "true"))) ]

                      Code(
                          stmt (
                              matchOn
                                  (ident $"value.{codeName field.name}")
                                  [ patCase "Some" [ "v" ], block (writes written); patNamed "None", unit ]
                          )
                      )

                      if commaState <> Some true then
                          commaState <- None
                  | _ ->
                      yield! separator
                      Plain name
                      serialize field.fieldType (ident $"value.{codeName field.name}")
                      commaState <- Some true

              Plain "}" ]
        |> block

    let private unionSerializer (typ: UnionType) =
        matchOn
            (ident "value")
            [ for case in typ.cases do
                  let names = case.fields |> List.mapi (fun i _ -> $"p{i}")

                  let body =
                      if case.fields.IsEmpty then
                          [ Plain(jsonString case.name) ]
                      else
                          [ Plain $"[{jsonString case.name}"
                            for i, field in case.fields |> List.indexed do
                                Plain ","
                                serialize field.fieldType (ident names[i])
                            Plain "]" ]

                  patCase (unionCaseName typ case.name) names, block (writes body) ]

    let private tupleSerializer (typ: TupleType) =
        let names = typ.values |> List.mapi (fun i _ -> $"v{i}")

        block
            [ letTuple names (ident "value")
              yield!
                  writes
                      [ Plain "["
                        for i, value in typ.values |> List.indexed do
                            if i > 0 then
                                Plain ","

                            serialize value.valueType (ident names[i])
                        Plain "]" ] ]

    let private arraySerializer (itemType: SerializableType ref) =
        block
            [ writePlain "["
              letMutable "first" (ident "true")
              stmt (
                  forEach "item" (ident "value") (block (commaSeparated (writes [ serialize itemType (ident "item") ])))
              )
              writePlain "]" ]

    /// Lists are walked by Head/Tail rather than enumerated since the enumerator is an
    /// interface call per item.
    let private listSerializer (itemType: SerializableType ref) =
        block
            [ writePlain "["
              letMutable "first" (ident "true")
              letMutable "rest" (ident "value")
              stmt (
                  whileLoop
                      (notExpr (ident "rest.IsEmpty"))
                      (commaSeparated (
                          writes
                              [ serialize itemType (ident "rest.Head")
                                Code(stmt (set (ident "rest") (ident "rest.Tail"))) ]
                      ))
              )
              writePlain "]" ]

    /// Sets and maps are folded rather than enumerated, with the comma flag as the fold state.
    let private foldSerializer
        (foldFunction: string)
        (lambdaParams: string list)
        (item: Out list)
        (prefix: string)
        (suffix: string)
        =
        let step =
            lambda
                lambdaParams
                (block
                    [ stmt (ifThen (notExpr (ident "first")) (writer "WritePlain" [ str "," ]))
                      yield! writes item
                      stmt (ident "false") ])

        block
            [ writePlain prefix
              stmt (pipe (pipe (ident "value") (curried foldFunction [ paren step; ident "true" ])) (ident "ignore"))
              writePlain suffix ]

    let private mapSerializer (keyType: SerializableType ref) (valueType: SerializableType ref) =
        let key = serialize keyType (ident "k")
        let value = serialize valueType (ident "v")

        if hasObjectKeys keyType then
            foldSerializer "Map.fold" [ "first"; "k"; "v" ] [ key; Plain ":"; value ] "{" "}"
        else
            foldSerializer "Map.fold" [ "first"; "k"; "v" ] [ Plain "["; key; Plain ","; value; Plain "]" ] "[" "]"

    let private typeSerializer (typ: SerializableType ref) =
        let name = getTypeName typ

        let body =
            match typ.Value with
            | SerializableType.Basic _ -> block (writes [ serialize typ (ident "value") ])
            | SerializableType.AnonymousRecord r -> recordSerializer r.fields
            | SerializableType.Record r -> recordSerializer r.fields
            | SerializableType.Union u -> unionSerializer u
            | SerializableType.Enum e ->
                writer "Write" [ paren (curried (string e.valueType.typeName) [ ident "value" ]) ]
            | SerializableType.Tuple t -> tupleSerializer t
            | SerializableType.Array a -> arraySerializer a
            | SerializableType.List l -> listSerializer l
            | SerializableType.Set s ->
                foldSerializer "Set.fold" [ "first"; "item" ] [ serialize s (ident "item") ] "[" "]"
            | SerializableType.Map(k, v) -> mapSerializer k v
            | SerializableType.Optional o ->
                matchOn
                    (ident "value")
                    [ patCase "Some" [ "v" ], block (writes [ serialize o (ident "v") ])
                      patNamed "None", writer "WritePlain" [ str "null" ] ]
            | SerializableType.UnitOfMeasure _ -> failwith "units of measure are stripped before generation"

        staticMember (serializerName name) [ "value", string name; "writer", "Fason.JsonWriter" ] "unit" body

    // ---------------------------------------------------------------------------------
    // Streaming deserializers
    // ---------------------------------------------------------------------------------

    let private expect (c: char) = stmt (reader "Expect" [ chr c ])
    let private readUntil (c: char) = reader "ReadUntil" [ chr c ]

    let private basicReader (b: BasicType) =
        let name =
            match b with
            | BasicType.Bool -> "ReadBool"
            | BasicType.Char -> "ReadChar"
            | BasicType.Int8 -> "ReadInt8"
            | BasicType.Int16 -> "ReadInt16"
            | BasicType.Int32 -> "ReadInt32"
            | BasicType.Int64 -> "ReadInt64"
            | BasicType.UInt8 -> "ReadUInt8"
            | BasicType.UInt16 -> "ReadUInt16"
            | BasicType.UInt32 -> "ReadUInt32"
            | BasicType.UInt64 -> "ReadUInt64"
            | BasicType.Single -> "ReadSingle"
            | BasicType.Double -> "ReadDouble"
            | BasicType.Decimal -> "ReadDecimal"
            | BasicType.String -> "ReadString"
            | BasicType.Guid -> "ReadGuid"
            | BasicType.DateTime -> "ReadDateTime"
            | BasicType.TimeSpan -> "ReadTimeSpan"
            | BasicType.DateOnly -> "ReadDateOnly"
            | BasicType.TimeOnly -> "ReadTimeOnly"
            | BasicType.DateTimeOffset -> "ReadDateTimeOffset"
            | BasicType.Unit -> "ReadNull"

        reader name []

    /// A parenthesized call that reads a value of the given type, retyped back if it has units of measure.
    let private deserialize (typ: SerializableType ref) =
        let strippedType, strippedName, name = erased typ

        let readCall =
            match strippedType.Value with
            | SerializableType.Basic b -> basicReader b
            | _ -> call (ident $"Codecs.{deserializerName strippedName}") [ ident "reader" ]

        if strippedName = name then
            paren readCall
        else
            retype (paren readCall) name

    /// Reads the comma-separated elements between `openChar` and `closeChar`, running `itemBody` for each.
    let private elementLoop (openChar: char, closeChar: char) (itemBody: Statement list) =
        [ expect openChar
          letMutable "first" (ident "true")
          stmt (whileLoop (notExpr (readUntil closeChar)) (separated (reader "Expect" [ chr ',' ]) itemBody)) ]

    let private arrayLoop = elementLoop ('[', ']')

    /// Fields are read into mutable locals and a bitmask tracks which required fields were
    /// seen: a uint32 for up to 32 fields, a BitSet for moer. Field names are matched over a
    /// radix tree: each level is a switch on the next character, then one check of the rest
    /// of the edge.
    let private fieldDeserializer (fields: RecordField list) (typeName: string<typeName>) (isAnonymous: bool) =
        let useMask = fields.Length <= 32
        let fieldIndex = fields |> List.mapi (fun i f -> f.name, i) |> Map.ofList
        let skipField = reader "SkipObjectField" []
        let peek = reader "Peek" []

        let markFound index =
            if useMask then
                set (ident "found") (infix (ident "found") "|||" (hex (1u <<< index)))
            else
                call (ident "found.Set") [ int index ]

        let rec dispatch (children: RadixTreeNode list) (parentText: string) =
            matchOn
                peek
                [ for child in children do
                      patChar child.data[0], parse child parentText
                  patWild, skipField ]

        and parse (node: RadixTreeNode) (parentText: string) =
            let fieldName = parentText + node.data
            let isPlainLeaf = node.isLeaf && node.children.IsEmpty

            // A leaf without children is matched together with its closing quote.
            let edge = if isPlainLeaf then node.data + "\"" else node.data

            let readField () =
                let index = fieldIndex[fieldName]

                block
                    [ if not isPlainLeaf then
                          stmt (reader "Skip" [])
                      expect ':'
                      stmt (set (ident $"f{index}") (deserialize fields[index].fieldType))
                      stmt (markFound index) ]

            let body =
                block
                    [ stmt (reader "SkipCount" [ int edge.Length ])
                      match node.isLeaf, node.children.IsEmpty with
                      | true, true -> stmt (readField ())
                      | true, false ->
                          stmt (ifElse (eq peek (chr '"')) (readField ()) (dispatch node.children fieldName))
                      | false, _ -> stmt (dispatch node.children fieldName) ]

            // The switch checked the first character. The rest is verified here.
            if edge.Length = 1 then
                body
            elif edge.Length = 2 && isPlainLeaf then
                ifElse (eq (reader "PeekAt" [ int 1 ]) (chr '"')) body skipField
            else
                ifElse (reader "StartsWith" [ str edge ]) body skipField

        // Optional and unit fields start out as found, and so do the unused high bits.
        let words = Array.create ((fields.Length + 31) / 32) 0xFFFFFFFFu

        for i, f in fields |> List.indexed do
            if
                not (
                    f.fieldType.Value.IsOptional
                    || f.fieldType.Value = SerializableType.Basic BasicType.Unit
                )
            then
                words[i / 32] <- words[i / 32] &&& ~~~(1u <<< (i % 32))

        let tree = fields |> List.map _.name |> RadixTree.fromStrings |> RadixTree.sort

        let recordFields =
            [ for i, field in fields |> List.indexed -> RecordFieldExpr(codeName field.name, ident $"f{i}") ]

        let missingCheck =
            if useMask then
                infix (ident "found") "<>" (hex 0xFFFFFFFFu)
            else
                call (ident "found.AnyFalse") []

        block
            [ for i, field in fields |> List.indexed do
                  let fieldType = string (getTypeName field.fieldType)

                  LetOrUseExpr(
                      Value(NamedPat $"f{i}", typeApp "Unchecked.defaultof" [ fieldType ], LongIdent fieldType)
                      |> _.toMutable()
                  )

              if useMask then
                  letMutable "found" (hex words[0])
              else
                  letValue "found" (curried "Fason.BitSet" [ array [ for w in words -> hex w ] ])

              yield!
                  elementLoop
                      ('{', '}')
                      [ expect '"'
                        stmt (
                            if fields.IsEmpty then
                                skipField
                            else
                                dispatch tree.children ""
                        ) ]
              stmt (ifThen missingCheck (reader "Fail" [ str $"missing a required field of {typeName}" ]))
              stmt (
                  if isAnonymous then
                      AnonRecordExpr recordFields
                  else
                      RecordExpr recordFields
              ) ]

    let private unionDeserializer (typ: UnionType) =
        // Cases without fields are a plain string, cases with fields an array starting with the case name.
        let caseReader (case: UnionCase) =
            let caseName = unionCaseName typ case.name

            if case.fields.IsEmpty then
                ident caseName
            else
                let names = case.fields |> List.mapi (fun i _ -> $"u{i}")

                block
                    [ stmt (
                          ifThen
                              (notExpr (ident "inArray"))
                              (reader "Fail" [ str $"the tag of {case.name} must be the first element of an array" ])
                      )
                      for i, field in case.fields |> List.indexed do
                          expect ','
                          letValue names[i] (deserialize field.fieldType)
                      stmt (call (ident caseName) (names |> List.map ident)) ]

        let unknownTag =
            reader "Fail" [ paren (infix (infix (str "unknown tag '") "+" (ident "tag")) "+" (str "'")) ]

        block
            [ letValue "inArray" (readUntil '[')
              letValue "tag" (reader "ReadString" [])

              letValue
                  "value"
                  (matchOn
                      (ident "tag")
                      [ for case in typ.cases do
                            patStr case.name, caseReader case
                        patWild, unknownTag ])

              stmt (ifThen (ident "inArray") (reader "Expect" [ chr ']' ]))
              stmt (ident "value") ]

    let private enumDeserializer (typ: EnumType) =
        // Thoth writes numbers. Names are accepted too, except for 64-bit enums, whose
        // numbers are quoted and so cannot be told apart from names.
        let fromNumber =
            call
                (typeApp "LanguagePrimitives.EnumOfValue" [ string typ.valueType.typeName; string typ.name ])
                [ paren (basicReader typ.valueType) ]

        let unknownName =
            reader "Fail" [ paren (infix (infix (str "unknown enum value '") "+" (ident "other")) "+" (str "'")) ]

        match typ.valueType with
        | BasicType.Int64
        | BasicType.UInt64 -> fromNumber
        | _ ->
            block
                [ stmt (reader "SkipWhitespace" [])
                  stmt (
                      ifElse
                          (eq (reader "Peek" []) (chr '"'))
                          (matchOn
                              (reader "ReadString" [])
                              [ for v in typ.values do
                                    patStr v.name, ident $"{typ.name}.{codeName v.name}"
                                patNamed "other", unknownName ])
                          fromNumber
                  ) ]

    let private tupleDeserializer (typ: TupleType) =
        let names = typ.values |> List.mapi (fun i _ -> $"t{i}")

        block
            [ expect '['
              for i, value in typ.values |> List.indexed do
                  if i > 0 then
                      expect ','

                  letValue names[i] (deserialize value.valueType)
              expect ']'
              stmt (tuple (names |> List.map ident)) ]

    let private mapDeserializer (keyType: SerializableType ref) (valueType: SerializableType ref) =
        let add =
            stmt (set (ident "values") (pipe (ident "values") (curried "Map.add" [ ident "k"; ident "v" ])))

        let pairs =
            block (
                arrayLoop
                    [ expect '['
                      letValue "k" (deserialize keyType)
                      expect ','
                      letValue "v" (deserialize valueType)
                      expect ']'
                      add ]
            )

        let objectForm =
            block (
                elementLoop
                    ('{', '}')
                    [ letValue "k" (deserialize keyType)
                      expect ':'
                      letValue "v" (deserialize valueType)
                      add ]
            )

        block
            [ letMutable "values" (ident "Map.empty")
              if hasObjectKeys keyType then
                  // Both the object form and the pair array form are accepted.
                  stmt (reader "SkipWhitespace" [])
                  stmt (ifElse (eq (reader "Peek" []) (chr '{')) objectForm pairs)
              else
                  stmt pairs
              stmt (ident "values") ]

    let private typeDeserializer (typ: SerializableType ref) =
        let name = getTypeName typ

        let collection (initial: Expression) (add: Expression) (result: Expression) =
            block [ letMutable "values" initial; yield! arrayLoop [ stmt add ]; stmt result ]

        let body =
            match typ.Value with
            | SerializableType.Basic b -> basicReader b
            | SerializableType.AnonymousRecord r -> fieldDeserializer r.fields name true
            | SerializableType.Record r -> fieldDeserializer r.fields name false
            | SerializableType.Union u -> unionDeserializer u
            | SerializableType.Enum e -> enumDeserializer e
            | SerializableType.Tuple t -> tupleDeserializer t
            | SerializableType.Array a ->
                collection
                    (call (typeApp "ResizeArray" [ string (getTypeName a) ]) [])
                    (call (ident "values.Add") [ deserialize a ])
                    (call (ident "values.ToArray") [])
            // Lists are built by consing in reverse, which needs no intermediate buffer.
            | SerializableType.List l ->
                collection
                    (paren (typed (list []) $"{getTypeName l} list"))
                    (set (ident "values") (infix (deserialize l) "::" (ident "values")))
                    (curried "List.rev" [ ident "values" ])
            | SerializableType.Set s ->
                collection
                    (paren (typed (ident "Set.empty") $"Set<{getTypeName s}>"))
                    (set (ident "values") (pipe (ident "values") (curried "Set.add" [ deserialize s ])))
                    (ident "values")
            | SerializableType.Map(k, v) -> mapDeserializer k v
            | SerializableType.Optional o ->
                block
                    [ stmt (reader "SkipWhitespace" [])
                      stmt (
                          ifElse
                              (eq (reader "Peek" []) (chr 'n'))
                              (block [ stmt (reader "ReadNull" []); stmt (ident "None") ])
                              (curried "Some" [ deserialize o ])
                      ) ]
            | SerializableType.UnitOfMeasure _ -> failwith "units of measure are stripped before generation"

        staticMember (deserializerName name) [ "reader", "Fason.JsonReader" ] (string name) body

    // ---------------------------------------------------------------------------------
    // JavaScript value codecs (Fable only)
    // ---------------------------------------------------------------------------------

    /// Converts `value` of the given type to a JavaScript value.
    let private toJs (typ: SerializableType ref) (value: Expression) =
        let strippedType, strippedName, name = erased typ

        let value =
            if strippedName = name then
                value
            else
                retype value strippedName

        match strippedType.Value with
        | SerializableType.Basic b ->
            match b with
            | BasicType.Unit -> ident "null"
            | BasicType.Char -> jsValue "ofChar" [ paren value ]
            | BasicType.Int64 -> jsValue "ofInt64" [ paren value ]
            | BasicType.UInt64 -> jsValue "ofUInt64" [ paren value ]
            | BasicType.Guid -> jsValue "ofGuid" [ paren value ]
            | BasicType.DateTime when hermes -> jsValue "ofDateTimeNative" [ paren value ]
            | BasicType.DateTime -> jsValue "ofDateTime" [ paren value ]
            | BasicType.TimeSpan -> jsValue "ofTimeSpan" [ paren value ]
            | BasicType.DateOnly -> jsValue "ofDateOnly" [ paren value ]
            | BasicType.TimeOnly -> jsValue "ofTimeOnly" [ paren value ]
            | BasicType.DateTimeOffset -> jsValue "ofDateTimeOffset" [ paren value ]
            | BasicType.Decimal -> jsValue "ofDecimal" [ paren value ]
            | _ -> boxed value
        | _ -> call (ident $"Codecs.{toJsName strippedName}") [ paren value ]

    /// Converts the JavaScript value `js` to the given type.
    let private fromJs (typ: SerializableType ref) (js: Expression) =
        let strippedType, strippedName, name = erased typ

        let converted =
            match strippedType.Value with
            | SerializableType.Basic b ->
                match b with
                | BasicType.Unit -> paren (curried "ignore" [ paren js ])
                | BasicType.Bool -> jsValue "toBool" [ paren js ]
                | BasicType.String -> jsValue "toString" [ paren js ]
                | BasicType.Char -> jsValue "toChar" [ paren js ]
                | BasicType.Int32 -> jsValue "toInt32" [ paren js ]
                | BasicType.Int64 -> jsValue "toInt64" [ paren js ]
                | BasicType.UInt64 -> jsValue "toUInt64" [ paren js ]
                | BasicType.Double -> jsValue "toFloat" [ paren js ]
                | BasicType.Single -> curried "single" [ paren (jsValue "toFloat" [ paren js ]) ]
                | BasicType.Guid -> jsValue "toGuid" [ paren js ]
                | BasicType.DateTime when hermes -> jsValue "toDateTimeNative" [ paren js ]
                | BasicType.DateTime -> jsValue "toDateTime" [ paren js ]
                | BasicType.TimeSpan -> jsValue "toTimeSpan" [ paren js ]
                | BasicType.DateOnly -> jsValue "toDateOnly" [ paren js ]
                | BasicType.TimeOnly -> jsValue "toTimeOnly" [ paren js ]
                | BasicType.DateTimeOffset -> jsValue "toDateTimeOffset" [ paren js ]
                | BasicType.Decimal -> jsValue "toDecimal" [ paren js ]
                | other -> curried (string other.typeName) [ paren (jsValue "toInt32" [ paren js ]) ]
            | _ -> call (ident $"Codecs.{fromJsName strippedName}") [ paren js ]

        if strippedName = name then
            converted
        else
            retype (paren converted) name

    let private at (arr: string) (i: Expression) = platform "at" [ ident arr; i ]
    let private isNullish (value: Expression) = platform "isNullish" [ value ]

    let private isString (value: Expression) =
        eq (platform "typeOf" [ value ]) (str "string")

    let private failWith (expected: string) (value: Expression) =
        curried "Fason.JsValue.fail" [ str expected; paren value ]

    let private jsRecordToJs (fields: RecordField list) =
        block
            [ letValue "o" (platform "newObject" [])
              for field in fields do
                  let setField (converted: Expression) =
                      platform "setField" [ ident "o"; str field.name; converted ]

                  match field.fieldType.Value with
                  | SerializableType.Optional inner ->
                      stmt (
                          matchOn
                              (ident $"value.{codeName field.name}")
                              [ patCase "Some" [ "v" ], setField (toJs inner (ident "v"))
                                patNamed "None", unit ]
                      )
                  | _ -> stmt (setField (toJs field.fieldType (ident $"value.{codeName field.name}")))
              stmt (ident "o") ]

    let private jsRecordFromJs (fields: RecordField list) (typeName: string<typeName>) (isAnonymous: bool) =
        let recordFields =
            [ for i, field in fields |> List.indexed -> RecordFieldExpr(codeName field.name, ident $"f{i}") ]

        block
            [ letValue "o" (jsValue "toObject" [ ident "value" ])
              for i, field in fields |> List.indexed do
                  let f = $"f{i}"
                  letValue f (platform "getField" [ ident "o"; str field.name ])

                  match field.fieldType.Value with
                  | SerializableType.Basic BasicType.Unit -> letValue f unit
                  | SerializableType.Optional inner ->
                      letValue f (ifElse (isNullish (ident f)) (ident "None") (some (fromJs inner (ident f))))
                  | _ ->
                      let missing =
                          curried "Fason.JsValue.missing" [ str field.name; str (string typeName) ]

                      letValue f (ifElse (isNullish (ident f)) missing (fromJs field.fieldType (ident f)))
              stmt (
                  if isAnonymous then
                      AnonRecordExpr recordFields
                  else
                      RecordExpr recordFields
              ) ]

    let private jsUnionToJs (typ: UnionType) =
        matchOn
            (ident "value")
            [ for case in typ.cases do
                  let names = case.fields |> List.mapi (fun i _ -> $"p{i}")
                  let tag = boxed (str case.name)

                  patCase (unionCaseName typ case.name) names,
                  if case.fields.IsEmpty then
                      tag
                  else
                      boxed (
                          array (
                              tag
                              :: [ for i, f in case.fields |> List.indexed -> toJs f.fieldType (ident names[i]) ]
                          )
                      ) ]

    let private jsUnionFromJs (typ: UnionType) =
        let unknown = patNamed "other", failWith "a known case" (boxed (ident "other"))

        let caseFromArray (case: UnionCase) =
            let caseName = unionCaseName typ case.name

            if case.fields.IsEmpty then
                ident caseName
            else
                match [ for i, f in case.fields |> List.indexed -> fromJs f.fieldType (at "arr" (int (i + 1))) ] with
                | [ single ] -> call (ident caseName) [ paren single ]
                | args -> call (ident caseName) args

        ifElse
            (isString (ident "value"))
            (matchOn
                (call (typeApp "unbox" [ "string" ]) [ ident "value" ])
                [ for case in typ.cases do
                      if case.fields.IsEmpty then
                          patStr case.name, ident (unionCaseName typ case.name)
                  unknown ])
            (block
                [ letValue "arr" (jsValue "toArray" [ ident "value" ])
                  stmt (
                      matchOn
                          (jsValue "toString" [ paren (at "arr" (int 0)) ])
                          [ for case in typ.cases do
                                patStr case.name, caseFromArray case
                            unknown ]
                  ) ])

    let private jsEnum (typ: EnumType) =
        let valueType = string typ.valueType.typeName

        let number =
            match typ.valueType with
            | BasicType.Int64 -> jsValue "ofInt64" [ paren (curried "int64" [ ident "value" ]) ]
            | BasicType.UInt64 -> jsValue "ofUInt64" [ paren (curried "uint64" [ ident "value" ]) ]
            | _ -> boxed (curried valueType [ ident "value" ])

        let fromNumber =
            match typ.valueType with
            | BasicType.Int64 -> jsValue "toInt64" [ ident "value" ]
            | BasicType.UInt64 -> jsValue "toUInt64" [ ident "value" ]
            | _ -> curried valueType [ paren (jsValue "toInt32" [ ident "value" ]) ]

        let fromValue =
            call (typeApp "LanguagePrimitives.EnumOfValue" [ valueType; string typ.name ]) [ paren fromNumber ]

        // 64-bit enums are quoted numbers, which cannot be told apart from names.
        let fromBody =
            match typ.valueType with
            | BasicType.Int64
            | BasicType.UInt64 -> fromValue
            | _ ->
                ifElse
                    (isString (ident "value"))
                    (matchOn
                        (call (typeApp "unbox" [ "string" ]) [ ident "value" ])
                        [ for v in typ.values do
                              patStr v.name, ident $"{typ.name}.{codeName v.name}"
                          patNamed "other", failWith "a known enum name" (boxed (ident "other")) ])
                    fromValue

        number, fromBody

    /// `for i in 0 .. arr.Length - 1 do body`.
    let private forEachIndex (body: Expression) =
        forTo "i" (int 0) (infix (ident "arr.Length") "-" (int 1)) body

    let private jsCodecs (typ: SerializableType ref) =
        let name = getTypeName typ
        let toArray = jsValue "toArray" [ ident "value" ]

        let newList () =
            letValue "arr" (call (typeApp "ResizeArray" [ "obj" ]) [])

        let addItem (item: Expression) = call (ident "arr.Add") [ paren item ]
        let itemAt = at "arr" (ident "i")

        let toBody, fromBody =
            match typ.Value with
            | SerializableType.Basic _ -> toJs typ (ident "value"), fromJs typ (ident "value")
            | SerializableType.Record r -> jsRecordToJs r.fields, jsRecordFromJs r.fields name false
            | SerializableType.AnonymousRecord r -> jsRecordToJs r.fields, jsRecordFromJs r.fields name true
            | SerializableType.Union u -> jsUnionToJs u, jsUnionFromJs u
            | SerializableType.Enum e -> jsEnum e
            | SerializableType.Tuple t ->
                let names = t.values |> List.mapi (fun i _ -> $"v{i}")

                block
                    [ letTuple names (ident "value")
                      stmt (boxed (array [ for i, v in t.values |> List.indexed -> toJs v.valueType (ident names[i]) ])) ],
                block
                    [ letValue "arr" toArray
                      stmt (
                          ifThen
                              (infix (ident "arr.Length") "<>" (int t.values.Length))
                              (failWith $"a tuple of {t.values.Length}" (ident "value"))
                      )
                      stmt (tuple [ for i, v in t.values |> List.indexed -> fromJs v.valueType (at "arr" (int i)) ]) ]
            | SerializableType.Array item ->
                block
                    [ letValue "arr" (call (typeApp "ResizeArray" [ "obj" ]) [ ident "value.Length" ])
                      stmt (forEach "item" (ident "value") (addItem (toJs item (ident "item"))))
                      stmt (boxed (ident "arr")) ],
                block
                    [ letValue "arr" toArray
                      letValue
                          "result"
                          (call (typeApp "Array.zeroCreate" [ string (getTypeName item) ]) [ ident "arr.Length" ])
                      stmt (forEachIndex (set (index (ident "result") (ident "i")) (fromJs item itemAt)))
                      stmt (ident "result") ]
            | SerializableType.List item ->
                // Walking the list by field access avoids the enumerator and the List module calls.
                block
                    [ newList ()
                      letMutable "rest" (ident "value")
                      stmt (
                          whileLoop
                              (notExpr (platform "listIsEmpty" [ ident "rest" ]))
                              [ stmt (addItem (toJs item (paren (platform "listHead" [ ident "rest" ]))))
                                stmt (set (ident "rest") (platform "listTail" [ ident "rest" ])) ]
                      )
                      stmt (boxed (ident "arr")) ],
                block
                    [ letValue "arr" toArray
                      letMutable "result" (paren (typed (list []) $"{getTypeName item} list"))
                      letMutable "i" (infix (ident "arr.Length") "-" (int 1))
                      stmt (
                          whileLoop
                              (infix (ident "i") ">=" (int 0))
                              [ stmt (set (ident "result") (infix (fromJs item itemAt) "::" (ident "result")))
                                stmt (set (ident "i") (infix (ident "i") "-" (int 1))) ]
                      )
                      stmt (ident "result") ]
            | SerializableType.Set item ->
                block
                    [ newList ()
                      stmt (
                          curried
                              "Set.iter"
                              [ paren (lambda [ "item" ] (addItem (toJs item (ident "item"))))
                                ident "value" ]
                      )
                      stmt (boxed (ident "arr")) ],
                block
                    [ letValue "arr" toArray
                      letMutable "result" (paren (typed (ident "Set.empty") $"Set<{getTypeName item}>"))
                      stmt (
                          forEachIndex (
                              set (ident "result") (curried "Set.add" [ paren (fromJs item itemAt); ident "result" ])
                          )
                      )
                      stmt (ident "result") ]
            | SerializableType.Map(k, v) ->
                let emptyMap =
                    paren (typed (ident "Map.empty") $"Map<{getTypeName k}, {getTypeName v}>")

                let addTo (key: Expression) (value: Expression) =
                    set (ident "result") (curried "Map.add" [ paren key; paren value; ident "result" ])

                let pairForm =
                    block
                        [ letValue "arr" toArray
                          stmt (
                              forEachIndex (
                                  block
                                      [ letValue "p" (jsValue "toArray" [ paren itemAt ])
                                        stmt (addTo (fromJs k (at "p" (int 0))) (fromJs v (at "p" (int 1)))) ]
                              )
                          ) ]

                if hasObjectKeys k then
                    let keyText, keyFrom =
                        match (stripUom k).Value with
                        | SerializableType.Basic BasicType.Guid ->
                            curried "string" [ ident "k" ], curried "System.Guid.Parse" [ ident "key" ]
                        | _ -> ident "k", ident "key"

                    block
                        [ letValue "o" (platform "newObject" [])
                          stmt (
                              curried
                                  "Map.iter"
                                  [ paren (
                                        lambda
                                            [ "k"; "v" ]
                                            (platform "setField" [ ident "o"; keyText; toJs v (ident "v") ])
                                    )
                                    ident "value" ]
                          )
                          stmt (ident "o") ],
                    block
                        [ letMutable "result" emptyMap
                          stmt (
                              ifElse
                                  (platform "isArray" [ ident "value" ])
                                  pairForm
                                  (block
                                      [ letValue "o" (jsValue "toObject" [ ident "value" ])
                                        stmt (
                                            forEach
                                                "key"
                                                (platform "objectKeys" [ ident "o" ])
                                                (addTo
                                                    keyFrom
                                                    (fromJs v (platform "getField" [ ident "o"; ident "key" ])))
                                        ) ])
                          )
                          stmt (ident "result") ]
                else
                    block
                        [ newList ()
                          stmt (
                              curried
                                  "Map.iter"
                                  [ paren (
                                        lambda
                                            [ "k"; "v" ]
                                            (addItem (boxed (array [ toJs k (ident "k"); toJs v (ident "v") ])))
                                    )
                                    ident "value" ]
                          )
                          stmt (boxed (ident "arr")) ],
                    block [ letMutable "result" emptyMap; stmt pairForm; stmt (ident "result") ]
            | SerializableType.Optional inner ->
                matchOn
                    (ident "value")
                    [ patCase "Some" [ "v" ], toJs inner (ident "v")
                      patNamed "None", ident "null" ],
                ifElse (isNullish (ident "value")) (ident "None") (some (fromJs inner (ident "value")))
            | SerializableType.UnitOfMeasure _ -> failwith "units of measure are stripped before generation"

        [ staticMember (toJsName name) [ "value", string name ] "obj" toBody
          staticMember (fromJsName name) [ "value", "obj" ] (string name) fromBody ]

    // ---------------------------------------------------------------------------------
    // Registration
    // ---------------------------------------------------------------------------------

    /// `Codecs.Register()`, which registers every codec with `Fason.Json`.
    let private registerMember (typeNames: string<typeName> list) (delegates: string<typeName> -> Expression list) =
        let entries =
            [ for name in typeNames ->
                  stmt (
                      curried
                          "Fason.Json.register"
                          [ typeApp "typeof" [ string name ]
                            paren (
                                upcastTo (call (typeApp "Fason.Codec" [ string name ]) (delegates name)) "Fason.ICodec"
                            ) ]
                  ) ]

        staticMember "Register" [] "unit" (if entries.IsEmpty then unit else block entries)

    let private jsDelegates name =
        [ paren (lambda [ "value" ] (call (ident $"Codecs.{toJsName name}") [ ident "value" ]))
          paren (lambda [ "js" ] (call (ident $"Codecs.{fromJsName name}") [ ident "js" ])) ]

    let private streamDelegates name =
        [ paren (
              lambda
                  [ "value"; "writer" ]
                  (call (ident $"Codecs.{serializerName name}") [ ident "value"; ident "writer" ])
          )
          paren (lambda [ "reader" ] (call (ident $"Codecs.{deserializerName name}") [ ident "reader" ])) ]

    let generate (types: SerializableType ref array, ns: string) =
        // Codecs are generated for the erased types only, since units of measure are
        // erased at runtime and cannot be told apart there.
        let typesWithCodec =
            types
            |> Seq.map stripUom
            |> Seq.distinctBy getTypeName
            |> Seq.sortBy getTypeName
            |> Seq.toList

        // Units of measure on non-numeric types need FSharp.UMX for the type annotations.
        let hasNonNumericUom =
            types
            |> Seq.exists (fun t ->
                match t.Value with
                | SerializableType.UnitOfMeasure { baseType = { contents = SerializableType.Basic(BasicType.String | BasicType.Guid | BasicType.Bool | BasicType.Char | BasicType.DateTime | BasicType.TimeSpan | BasicType.DateOnly | BasicType.TimeOnly | BasicType.DateTimeOffset | BasicType.Unit) } } ->
                    true
                | _ -> false)

        Oak() {
            Namespace(ns) {
                Open("Fason")

                if hasNonNumericUom then
                    Open("FSharp.UMX")

                // The streaming codecs only serve .NET. Fable goes through the JS codecs.
                let typeNames = typesWithCodec |> List.map getTypeName

                let streamMembers =
                    [ for typ in typesWithCodec do
                          typeSerializer typ
                          typeDeserializer typ
                      registerMember typeNames streamDelegates ]

                let jsMembers =
                    [ for typ in typesWithCodec do
                          yield! jsCodecs typ
                      registerMember typeNames jsDelegates ]

                TypeDefn("Codecs") {
                    for m in
                        streamMembers
                        |> List.updateAt 0 (streamMembers.Head |> _.triviaBefore(Directive "#if !FABLE_COMPILER")) do
                        m

                    for m in
                        jsMembers
                        |> List.updateAt 0 (jsMembers.Head |> _.triviaBefore(Directive "#else")) do
                        m
                }
                |> _.triviaAfter(Directive "#endif")
            }
        }
        |> Gen.mkOak
        |> Gen.run
