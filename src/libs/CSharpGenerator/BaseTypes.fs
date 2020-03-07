namespace CSharpGenerator.Types

open Common.CaseInsensitiveString
open System
open Common.Casing
open Common.Casing
open Common.StringUtils

module private Formatters =
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
           "while" |] |> Set

    let resolveName (name: string) =
        if keywords.Contains name then [ "@"; name ]
        else [ name ]
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
type Settings = {TypeCasing : Casing ; PropertyCasing : Casing ; Prefix: string ; Suffix: string }

type internal TypeInfo =
    { Name: string
      Namespace: string
      Alias: string option
      Nullable: bool }
    override this.ToString() = this.Alias |> Option.defaultValue ([ this.Namespace; "."; this.Name ] |> joinStrings)
    member this.AsNullable =
        if this.Nullable then
            this
        else
            let nullableConcat x =
                [ x; "?" ] |> joinStrings
            { Namespace = this.Namespace
              Name = nullableConcat this.Name
              Alias = this.Alias |> Option.map nullableConcat
              Nullable = true }

type internal ValueTypePair<'T> =
    { Value: 'T
      Type: TypeInfo }
    member pair.AsNullable =
        { Value = pair.Value
          Type = pair.Type.AsNullable }

and internal ValueType =
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


and internal ReferenceType =
    | String of ValueTypePair<string>
    | Object of TypeInfo
    member this.TypeInfo =
        match this with
        | String x -> x.Type
        | Object x -> x

and internal BaseType =
    | ReferenceType of ReferenceType
    | ValueType of ValueType

    member this.TypeInfo =
        match this with
        | ReferenceType x -> x.TypeInfo
        | ValueType x -> x.TypeInfo

    member this.FormatArray key = this.TypeInfo |> fun x -> Formatters.arrayProperty (x.ToString()) key

    member this.FormatProperty key = this.TypeInfo |> fun x -> Formatters.property (x.ToString()) key

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

type internal GeneratedType =
    { Members: Property [] }

    member this.FormatProperty ``type`` name = Formatters.property ``type`` name

    member this.ClassDeclaration (name: string) (typeSet: CIString Set) (settings : Settings) =
        let set =
            name
            |> CI
            |> typeSet.Add
        this.Members
        |> Seq.map (fun property ->
            let casedPropertyName = settings.PropertyCasing.apply property.Name
            match property.Type |> Option.defaultValue CSType.UnresolvedBaseType with
            | _ when not (Char.IsLetter property.Name.[0]) ->
                raise (System.ArgumentException("Member names can only start with letters."))
            | GeneratedType x when property.Name
                                   |> CI
                                   |> set.Contains ->
                // This will make sure that class names do not collide with their outer members.
                let className = joinStringsWithSpaceSeparation [ name; property.Name ] |> settings.TypeCasing.apply
                [ x.ClassDeclaration className set settings
                  x.FormatProperty
                      ([ settings.Prefix; className; settings.Suffix ]
                       |> joinStringsWithSpaceSeparation
                       |> settings.TypeCasing.apply) casedPropertyName ]
                |> joinStringsWithSpaceSeparation
            | GeneratedType x ->
                [ x.ClassDeclaration property.Name set settings
                  x.FormatProperty
                      ([ settings.Prefix; property.Name; settings.Suffix ]
                       |> joinStringsWithSpaceSeparation
                       |> settings.TypeCasing.apply) (casedPropertyName) ]
                |> joinStringsWithSpaceSeparation
            | ArrayType x ->
                let className =
                    joinStringsWithSpaceSeparation [ name; property.Name ]
                    |> settings.TypeCasing.apply
                    |> Option.Some
                x.FormatArray (casedPropertyName) set className settings
            | BaseType x -> x.FormatProperty(casedPropertyName))
        |> joinStringsWithSpaceSeparation
        |> (fun x ->

        let formattedName =
            if true then
                [ settings.Prefix; name; settings.Suffix ]
                |> joinStringsWithSpaceSeparation
                |> settings.TypeCasing.apply
            else
                name
        Formatters.``class`` (formattedName) x)


and internal Property =
    { Name: string
      Type: CSType Option }

and internal CSType =
    | BaseType of BaseType
    | GeneratedType of GeneratedType
    | ArrayType of CSType
    static member UnresolvedBaseType = BaseType.Object |> CSType.BaseType

    // TODO apply property casing and avoid using option string to pass a potential state
    member this.FormatArray key typeSet typeName settings =
        match this with
        | BaseType x ->
            if not typeSet.IsEmpty then
                x.FormatArray key
            else
                [settings.Prefix;key; settings.Suffix]
                |> joinStringsWithSpaceSeparation
                |> settings.PropertyCasing.apply
                |> x.FormatArray
        | GeneratedType x when key
                               |> CI
                               |> typeSet.Contains
                               && typeName.IsSome ->
            let typeName = typeName.Value

            let arrayProperty =
                Formatters.arrayProperty
                    ([ settings.Prefix; typeName; settings.Suffix ]
                     |> joinStringsWithSpaceSeparation
                     |> settings.TypeCasing.apply) key
            [ x.ClassDeclaration typeName typeSet settings
              arrayProperty ]
            |> joinStringsWithSpaceSeparation
        | GeneratedType x ->
            let classDecleration = x.ClassDeclaration key typeSet settings
            if typeSet.IsEmpty then
                classDecleration
            else
                let arrayProperty =
                    Formatters.arrayProperty
                        ([ settings.Prefix; key; settings.Suffix ]
                         |> joinStringsWithSpaceSeparation
                         |> settings.TypeCasing.apply) key
                [ classDecleration; arrayProperty ] |> joinStringsWithSpaceSeparation
        | ArrayType x -> x.FormatArray key typeSet Option.None settings
