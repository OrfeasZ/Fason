namespace Fabulous.AST

open Fabulous.AST
open Fabulous.AST.StackAllocatedCollections.StackList
open Fantomas.Core.SyntaxOak
open Fantomas.FCS.Text

module FsiMemberNode =
    let Name = Attributes.defineScalar<string> "Name"

    let Parameters = Attributes.defineScalar<Pattern list> "Parameters"

    let WidgetKey =
        Widgets.register
            "FsiMember"
            (fun widget ->
                let name = Widgets.getScalarValue widget Name
                let parameters = Widgets.getScalarValue widget Parameters
                let isInlined = Widgets.tryGetScalarValue widget BindingNode.IsInlined

                let isStatic =
                    Widgets.tryGetScalarValue widget BindingNode.IsStatic
                    |> ValueOption.defaultValue false

                let returnType =
                    Widgets.tryGetNodeFromWidget widget BindingNode.Return
                    |> ValueOption.defaultValue None

                let accessControl =
                    Widgets.tryGetScalarValue widget BindingNode.Accessibility
                    |> ValueOption.defaultValue AccessControl.Unknown

                let accessControl =
                    match accessControl with
                    | Public -> Some(SingleTextNode.``public``)
                    | Private -> Some(SingleTextNode.``private``)
                    | Internal -> Some(SingleTextNode.``internal``)
                    | Unknown -> None

                let xmlDocs =
                    Widgets.tryGetNodeFromWidget widget BindingNode.XmlDocs
                    |> ValueOption.map Some
                    |> ValueOption.defaultValue None

                let attributes =
                    Widgets.tryGetScalarValue widget BindingNode.MultipleAttributes
                    |> ValueOption.map (fun x -> Some(MultipleAttributeListNode.Create(x)))
                    |> ValueOption.defaultValue None

                let inlineNode =
                    match isInlined with
                    | ValueSome true -> Some(SingleTextNode.``inline``)
                    | ValueSome false -> None
                    | ValueNone -> None

                let typeParams =
                    Widgets.tryGetNodeFromWidget widget BindingNode.TypeParams
                    |> ValueOption.map Some
                    |> ValueOption.defaultValue None

                let multipleTextsNode = [
                    if isStatic then
                        SingleTextNode.``static``
                        SingleTextNode.``member``
                    else
                        SingleTextNode.``member``
                ]

                MemberDefnAbstractSlotNode(
                    xmlDocs,
                    attributes,
                    MultipleTextsNode.Create [ SingleTextNode.Create("member") ],
                    SingleTextNode.Create(name),
                    typeParams,
                    returnType.Value,
                    None,
                    Range.Zero
                )

            (*BindingNode(
                    xmlDocs,
                    attributes,
                    MultipleTextsNode(multipleTextsNode, Range.Zero),
                    false,
                    inlineNode,
                    accessControl,
                    Choice1Of2(
                        IdentListNode(
                            [
                                IdentifierOrDot.Ident(SingleTextNode.Create(name))
                            ],
                            Range.Zero
                        )
                    ),
                    typeParams,
                    parameters,
                    returnType,
                    SingleTextNode.Create "",
                    Expr.Constant(Constant.FromText(SingleTextNode.Create "")),
                    Range.Zero
                )*)
            )

[<AutoOpen>]
module FsiMemberBuilders =
    type Ast with

        /// <summary>
        /// Create a method member definition.
        /// </summary>
        /// <param name="name">The name of the method.</param>
        /// <param name="parameters">The parameters of the method.</param>
        /// <param name="body">The body of the method.</param>
        /// <code language="fsharp">
        /// Oak() {
        ///     AnonymousModule() {
        ///         TypeDefn("Person", UnitPat()) {
        ///             Member(
        ///                "this.Name",
        ///                [ ParenPat(ParameterPat("name", String()))
        ///                  ParenPat(ParameterPat("age", Int())) ],
        ///                ConstantExpr(Int 23)
        ///            )
        ///         }
        ///     }
        /// }
        /// </code>
        static member FsiMember(name: string, parameters: WidgetBuilder<Pattern>) =
            let parameters = parameters |> List.singleton |> List.map Gen.mkOak

            WidgetBuilder<BindingNode>(
                FsiMemberNode.WidgetKey,
                AttributesBundle(StackList.two (FsiMemberNode.Name.WithValue(name), FsiMemberNode.Parameters.WithValue(parameters)), [||], Array.empty)
            )
