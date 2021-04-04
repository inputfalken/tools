module Languages.CSharp

open Common.Casing
open Common.StringJoin
open System
open Common.CaseInsensitiveString


type public LetterRule =
    | Prefix of String
    | Suffix of String
    | ``Prefix and Suffix`` of string * String

type public Settings =
    { RootName: String
      NameSpace: String option
      ClassCasing: Casing
      PropertyCasing: Casing
      LetterRule: LetterRule }

type public TypeInfo =
    { Name: string
      Namespace: string
      Alias: string option
      Nullable: bool }
    member this.Stringified =
        this.Alias
        |> Option.defaultValue ([ this.Namespace; "."; this.Name ] |> joinStrings)

    member this.AsNullable =
        if this.Nullable then
            this
        else
            let nullableConcat x = [ x; "?" ] |> joinStrings

            { Namespace = this.Namespace
              Name = nullableConcat this.Name
              Alias = this.Alias |> Option.map nullableConcat
              Nullable = true }

type public ValueTypePair<'T> =
    { Value: 'T
      Type: TypeInfo }
    member pair.AsNullable =
        { Value = pair.Value
          Type = pair.Type.AsNullable }

and public ValueType =
    | Integer of ValueTypePair<int>
    | Guid of ValueTypePair<Guid>
    | Boolean of ValueTypePair<bool>
    | Datetime of ValueTypePair<DateTime>
    | Decimal of ValueTypePair<decimal>
    | Double of ValueTypePair<double>

    member valueType.AsNullable =
        match valueType with
        | Integer x -> x.AsNullable |> ValueType.Integer
        | Guid x -> x.AsNullable |> ValueType.Guid
        | Boolean x -> x.AsNullable |> ValueType.Boolean
        | Datetime x -> x.AsNullable |> ValueType.Datetime
        | Decimal x -> x.AsNullable |> ValueType.Decimal
        | Double x -> x.AsNullable |> ValueType.Double

    member this.TypeInfo =
        match this with
        | Integer x -> x.Type
        | Guid x -> x.Type
        | Boolean x -> x.Type
        | Datetime x -> x.Type
        | Decimal x -> x.Type
        | Double x -> x.Type


and public ReferenceType =
    | String of ValueTypePair<string>
    | Object of TypeInfo
    member this.TypeInfo =
        match this with
        | String x -> x.Type
        | Object x -> x

and public BaseType =
    | ReferenceType of ReferenceType
    | ValueType of ValueType

    member this.TypeInfo =
        match this with
        | ReferenceType x -> x.TypeInfo
        | ValueType x -> x.TypeInfo

    static member Guid x =
        { Type =
              { Namespace = "System"
                Name = "Guid"
                Alias = option.None
                Nullable = false }
          Value = x }
        |> ValueType.Guid
        |> BaseType.ValueType

    static member Double x =
        { Type =
              { Namespace = "System"
                Name = "Double"
                Alias = option.Some "double"
                Nullable = false }
          Value = x }
        |> ValueType.Double
        |> BaseType.ValueType

    static member Integer x =
        { Type =
              { Namespace = "System"
                Name = "Int32"
                Alias = option.Some "int"
                Nullable = false }
          Value = x }
        |> ValueType.Integer
        |> BaseType.ValueType

    static member Boolean x =
        { Type =
              { Namespace = "System"
                Name = "Boolean"
                Alias = option.Some "bool"
                Nullable = false }
          Value = x }
        |> ValueType.Boolean
        |> BaseType.ValueType

    static member DateTime x =
        { Type =
              { Namespace = "System"
                Name = "DateTime"
                Alias = option.None
                Nullable = false }
          Value = x }
        |> ValueType.Datetime
        |> BaseType.ValueType

    static member Decimal x =
        { Type =
              { Namespace = "System"
                Name = "Decimal"
                Alias = option.Some "decimal"
                Nullable = false }
          Value = x }
        |> ValueType.Decimal
        |> BaseType.ValueType

    static member Object =
        { Name = "Object"
          Namespace = "System"
          Alias = option.Some "object"
          Nullable = false }
        |> ReferenceType.Object
        |> BaseType.ReferenceType

    static member String x =
        { Type =
              { Namespace = "System"
                Name = "String"
                Alias = option.Some "string"
                Nullable = false }
          Value = x }
        |> ReferenceType.String
        |> BaseType.ReferenceType

and public Property = { Name: string; Type: CSType Option }

and public CSType =
    | BaseType of BaseType
    | GeneratedType of Property []
    | ArrayType of CSType

module internal Formatters =
    let keywords =
        [| "abstract"
           "as"
           "base"
           "bool"
           "break"
           "byte"
           "case"
           "catch"
           "char"
           "checked"
           "class"
           "const"
           "continue"
           "decimal"
           "default"
           "delegate"
           "do"
           "double"
           "else"
           "enum"
           "event"
           "explicit"
           "extern"
           "false"
           "finally"
           "fixed"
           "float"
           "for"
           "foreach"
           "goto"
           "if"
           "implicit"
           "in"
           "int"
           "interface"
           "internal"
           "is"
           "lock"
           "long"
           "namespace"
           "new"
           "null"
           "object"
           "operator"
           "out"
           "override"
           "params"
           "private"
           "protected"
           "public"
           "readonly"
           "ref"
           "return"
           "sbyte"
           "sealed"
           "short"
           "sizeof"
           "stackalloc"
           "static"
           "string"
           "struct"
           "switch"
           "this"
           "throw"
           "true"
           "try"
           "typeof"
           "uint"
           "ulong"
           "unchecked"
           "unsafe"
           "ushort"
           "using"
           "using"
           "static"
           "virtual"
           "void"
           "volatile"
           "while" |]
        |> Set

    let resolveName (name: string) =
        if keywords.Contains name then [ "@"; name ] else [ name ]
        |> joinStrings

    let ``class`` name content =
        [ "public class"
          resolveName name
          "{"
          content
          "}" ]
        |> joinStringsWithSpaceSeparation

    let property ``type`` name =
        [ "public"
          ``type``
          resolveName name
          "{ get; set; }" ]
        |> joinStringsWithSpaceSeparation

    let arrayProperty ``type`` name =
        property ([ ``type``; "[]" ] |> joinStrings) name

module public Factory =
    open Formatters
    let public UnresolvedBaseType = BaseType.Object |> CSType.BaseType

    let private getFormatter =
        function
        | ArrayType _ -> arrayProperty
        | _ -> property

    let private applyPrefixSuffix key (settings: Settings) (casing: Casing) =
        match settings.LetterRule with
        | Prefix x -> [ x; key ]
        | Suffix x -> [ key; x ]
        | ``Prefix and Suffix`` (x, y) -> [ x; key; y ]
        |> casing.applyMultiple

    let private createClassName (classSet: CIString Set) property className settings =
        if property |> CI |> classSet.Contains then
            settings.ClassCasing.applyMultiple [ className
                                                 property ]
        else
            property

    let validateName (name: String) =
        if not (Char.IsLetter name.[0])
        then raise (ArgumentException("Member names can only start with letters."))

    let rec private GeneratedType members key (classSet: CIString Set) settings propertyFormatter className =
        let classSet = classSet.Add <| CI className

        let classContent =
            members
            |> Array.map (fun property ->
                let formatter =
                    property.Type
                    |> Option.map getFormatter
                    |> Option.defaultValue propertyFormatter

                match property.Type
                      |> Option.defaultValue UnresolvedBaseType with
                | GeneratedType x ->
                    let uniqueClassName =
                        createClassName classSet property.Name className settings

                    GeneratedType x property.Name classSet settings formatter uniqueClassName
                | ArrayType x ->
                    match x with
                    | GeneratedType x ->
                        let uniqueClassName =
                            createClassName classSet property.Name className settings

                        GeneratedType x property.Name classSet settings formatter uniqueClassName
                    | x -> CSharpFactoryPrivate x property.Name classSet settings formatter
                | x -> CSharpFactoryPrivate x property.Name classSet settings (getFormatter x))
            |> joinStringsWithSpaceSeparation

        let formattedClassName =
            applyPrefixSuffix className settings settings.ClassCasing

        // Ugly side effect, maybe use Result in order in order to be explicit that things could go wrong.
        validateName formattedClassName

        let ``class`` =
            ``class`` formattedClassName classContent

        if classSet.Count = 1 then
            ``class``
        else
            let formattedPropertyName = key |> settings.PropertyCasing.apply

            let property =
                propertyFormatter formattedClassName formattedPropertyName

            [ ``class``; property ]
            |> joinStringsWithSpaceSeparation

    and private CSharpFactoryPrivate ``type`` key classSet settings propertyFormatter =
        match ``type`` with
        | GeneratedType members -> GeneratedType members key classSet settings propertyFormatter key
        | ArrayType ``type`` -> CSharpFactoryPrivate ``type`` key classSet settings arrayProperty
        | BaseType x ->

            let formattedPropertyName =
                if classSet.IsEmpty
                then applyPrefixSuffix key settings settings.PropertyCasing
                else key |> settings.PropertyCasing.apply

            // Ugly side effect, maybe use Result in order in order to be explicit that things could go wrong.
            validateName formattedPropertyName

            propertyFormatter x.TypeInfo.Stringified formattedPropertyName

    let public ToCSharpString ``type`` settings =
        let cSharp =
            CSharpFactoryPrivate ``type`` settings.RootName Set.empty settings (getFormatter ``type``)

        settings.NameSpace
        |> Option.map (fun x ->
            joinStringsWithSpaceSeparation [ "namespace"
                                             x
                                             "{"
                                             cSharp
                                             "}" ])
        |> Option.defaultValue cSharp
