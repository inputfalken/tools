namespace CSharpGenerator.Types

open Common.CaseInsensitiveString
open System
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

module internal CSharpFactory =

    let private classProperty ``type`` name = Formatters.property ``type`` name
    let private baseTypeArray (this: BaseType) key =
        this.TypeInfo |> fun x -> Formatters.arrayProperty (x.ToString()) key
    let internal UnresolvedBaseType = BaseType.Object |> CSType.BaseType


    let rec private CSharpClass members name (typeSet: CIString Set) settings =
        let typeSet = typeSet.Add <| CI name
        members
        |> Seq.map (fun property ->
            let casedPropertyName = settings.PropertyCasing.apply property.Name
            match property.Type |> Option.defaultValue UnresolvedBaseType with
            | _ when not (Char.IsLetter property.Name.[0]) ->
                raise (System.ArgumentException("Member names can only start with letters."))
            | GeneratedType x ->
                // This will make sure that class names do not collide with their outer members.
                let className =
                    if property.Name
                       |> CI
                       |> typeSet.Contains
                    then joinStringsWithSpaceSeparation [ name; property.Name ] |> settings.TypeCasing.apply
                    else property.Name

                let ``class`` = CSharpClass x className typeSet settings

                let property =
                    [ settings.Prefix; className; settings.Suffix ]
                    |> joinStringsWithSpaceSeparation
                    |> settings.TypeCasing.apply
                    |> classProperty
                    <| casedPropertyName

                [ ``class``; property ] |> joinStringsWithSpaceSeparation
            | ArrayType x ->
                let className =
                    joinStringsWithSpaceSeparation [ name; property.Name ]
                    |> settings.TypeCasing.apply
                    |> Option.Some

                CSharpArray x casedPropertyName typeSet className settings
            | x -> CSharpFactoryPrivate x casedPropertyName typeSet settings)
        |> joinStringsWithSpaceSeparation
        |> (fun x ->

        let name =
            [ settings.Prefix; name; settings.Suffix ]
            |> joinStringsWithSpaceSeparation
            |> settings.TypeCasing.apply

        Formatters.``class`` name x)

    and private CSharpArray ``type`` key typeSet typeName settings =
        match ``type`` with
        | GeneratedType x when key
                               |> CI
                               |> typeSet.Contains
                               && typeName.IsSome ->
            let typeName = typeName.Value

            let ``class`` = CSharpClass x typeName typeSet settings

            let property =
                [ settings.Prefix; typeName; settings.Suffix ]
                |> joinStringsWithSpaceSeparation
                |> settings.TypeCasing.apply
                |> Formatters.arrayProperty
                <| key

            [ ``class``; property ] |> joinStringsWithSpaceSeparation
        | GeneratedType x ->
            let ``class`` = CSharpClass x key typeSet settings
            if typeSet.IsEmpty then
                ``class``
            else
                let property =
                    [ settings.Prefix; key; settings.Suffix ]
                    |> joinStringsWithSpaceSeparation
                    |> settings.TypeCasing.apply
                    |> Formatters.arrayProperty
                    <| key

                [ ``class``; property ] |> joinStringsWithSpaceSeparation
        | ArrayType x -> CSharpFactoryPrivate x key typeSet settings
        | BaseType x ->
            if not typeSet.IsEmpty then
                baseTypeArray x key
            else
                [ settings.Prefix; key; settings.Suffix ]
                |> joinStringsWithSpaceSeparation
                |> settings.PropertyCasing.apply
                |> baseTypeArray x

    and private CSharpFactoryPrivate ``type`` key typeSet settings =
        match ``type`` with
        | GeneratedType x -> CSharpClass x key typeSet settings
        | ArrayType x -> CSharpArray x key typeSet Option.None settings
        | BaseType x ->
            let casedKey =
                if typeSet.IsEmpty then [ settings.Prefix; key; settings.Suffix ] |> joinStringsWithSpaceSeparation
                else key

            let casedKey = casedKey |> settings.PropertyCasing.apply
            let property = Formatters.property (x.TypeInfo.ToString()) casedKey
            property

    let internal CSharpFactory ``type`` root settings =
        CSharpFactoryPrivate ``type`` root Set.empty settings
