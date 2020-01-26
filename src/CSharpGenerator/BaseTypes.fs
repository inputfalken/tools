namespace CSharpGenerator.Types

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
        if (name.ToLowerInvariant >> keywords.Contains)() then [ "@"; name ]
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

type internal TypeInfo =
    { Name: string
      Namespace: string
      Alias: string option
      Nullable: bool }
    member this.ToString() = this.Alias |> Option.defaultValue ([ this.Namespace; "."; this.Name ] |> joinStrings)
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

type internal BaseType =
    | ReferenceType of TypeInfo
    | ValueType of TypeInfo

    member private this.TypeInfo =
        match this with
        | ReferenceType x -> x
        | ValueType x -> x

    member this.FormatArray key = this.TypeInfo |> fun x -> Formatters.arrayProperty (x.ToString()) key

    member this.FormatProperty key = this.TypeInfo |> fun x -> Formatters.property (x.ToString()) key

    static member Guid =
        { Namespace = "System"
          Name = "Guid"
          Alias = option.None
          Nullable = false }
        |> ValueType

    static member Double =
        { Namespace = "System"
          Name = "Double"
          Alias = option.Some "double"
          Nullable = false }
        |> ValueType

    static member Boolean =
        { Namespace = "System"
          Name = "Boolean"
          Alias = option.Some "bool"
          Nullable = false }
        |> ValueType

    static member DateTime =
        { Namespace = "System"
          Name = "DateTime"
          Alias = option.None
          Nullable = false }
        |> ValueType

    static member Decimal =
        { Namespace = "System"
          Name = "Decimal"
          Alias = Option.Some "decimal"
          Nullable = false }
        |> ValueType

    static member Object =
        { Name = "Object"
          Namespace = "System"
          Alias = option.Some "object"
          Nullable = false }
        |> ReferenceType

    static member String =
        { Namespace = "System"
          Name = "String"
          Alias = option.Some "string"
          Nullable = false }
        |> ReferenceType

type internal GeneratedType =
    { Members: Property []
      NamePrefix: string
      NameSuffix: string
      Casing: Casing }
    member this.FormatProperty ``type`` name = Formatters.property ``type`` name
    member this.ClassDeclaration name =
        let name = [ this.NamePrefix; name; this.NameSuffix ] |> joinStrings
        this.Members
        |> Seq.map (fun property ->
            match property.Type |> Option.defaultValue CSType.UnresolvedBaseType with
            | GeneratedType x ->
                [ x.ClassDeclaration property.Name
                  x.FormatProperty ([ x.NamePrefix; property.Name; x.NameSuffix ] |> joinStrings) property.Name ]
                |> joinStringsWithSpaceSeparation
            | ArrayType x -> x.FormatArray property.Name
            | BaseType x -> x.FormatProperty property.Name)
        |> joinStringsWithSpaceSeparation
        |> (fun x -> Formatters.``class`` (this.Casing.apply name) x)

and internal Property =
    { Name: string
      Type: CSType Option }

and internal CSType =
    | BaseType of BaseType
    | GeneratedType of GeneratedType
    | ArrayType of CSType
    static member UnresolvedBaseType = BaseType.Object |> CSType.BaseType
    member this.FormatArray key =
        match this with
        | BaseType x -> x.FormatArray key
        | GeneratedType x ->
            [ x.ClassDeclaration key
              Formatters.arrayProperty ([ x.NamePrefix; key; x.NameSuffix ] |> joinStrings) key ]
            |> joinStringsWithSpaceSeparation
        | ArrayType x -> x.FormatArray key
