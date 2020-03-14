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
    let internal UnresolvedBaseType = BaseType.Object |> CSType.BaseType

    let private getFormatter =
        function
        | ArrayType _ -> Formatters.arrayProperty
        | _ -> Formatters.property
        
    let validateName (name: String) = if not (Char.IsLetter name.[0]) then raise (System.ArgumentException("Member names can only start with letters."))

    let rec private GeneratedType members key (typeSet: CIString Set) settings propertyFormatter className =
        let className = className |> Option.defaultValue key
        let typeSet = typeSet.Add <| CI className
        
        let classContent =
            members
            |> Array.map (fun property ->
                let className =
                    if property.Name
                       |> CI
                       |> typeSet.Contains
                    then joinStringsWithSpaceSeparation [ className; property.Name ] |> settings.TypeCasing.apply
                    else property.Name
                    |> Option.Some
                match property.Type |> Option.defaultValue UnresolvedBaseType with
                | GeneratedType x ->
                    GeneratedType x property.Name typeSet settings propertyFormatter className
                | ArrayType x ->
                    let formatter = Formatters.arrayProperty
                    match x with
                    | GeneratedType x -> GeneratedType x property.Name typeSet settings formatter className
                    | x -> CSharpFactoryPrivate x property.Name typeSet settings formatter
                | x -> CSharpFactoryPrivate x property.Name typeSet settings (getFormatter x))
            |> joinStringsWithSpaceSeparation

        let formattedClassName =
            [ settings.Prefix; className; settings.Suffix ]
            |> joinStringsWithSpaceSeparation
            |> settings.TypeCasing.apply
            
        // Ugly side effect, maybe use Result in order in order to be explicit that things could go wrong.
        validateName formattedClassName

        let ``class`` = Formatters.``class`` formattedClassName classContent
        if typeSet.Count = 1 then
            ``class``
        else
            let formattedPropertyName = key |> settings.PropertyCasing.apply
            let property = propertyFormatter formattedClassName formattedPropertyName
            let res = [ ``class``; property ] |> joinStringsWithSpaceSeparation
            res

    and private CSharpFactoryPrivate ``type`` key typeSet settings propertyFormatter =
        match ``type`` with
        | GeneratedType members -> GeneratedType members key typeSet settings propertyFormatter Option.None
        | ArrayType ``type`` -> CSharpFactoryPrivate ``type`` key typeSet settings Formatters.arrayProperty
        | BaseType x ->
            let formattedPropertyName =
                if typeSet.IsEmpty then [ settings.Prefix; key; settings.Suffix ] |> joinStringsWithSpaceSeparation
                else key
                |> settings.PropertyCasing.apply
            validateName formattedPropertyName
            propertyFormatter x.TypeInfo.Stringified formattedPropertyName

    let internal CSharpFactory ``type`` root settings =
        CSharpFactoryPrivate ``type`` root Set.empty settings (getFormatter ``type``)
