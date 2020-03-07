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
    let private baseTypeArray (this: BaseType) key = this.TypeInfo |> fun x -> Formatters.arrayProperty (x.ToString()) key
    let internal CSharpProperty (this: BaseType) key = this.TypeInfo |> fun x -> Formatters.property (x.ToString()) key
    let internal UnresolvedBaseType = BaseType.Object |> CSType.BaseType
    let rec CSharpClass members name (typeSet: CIString Set) settings =
        let set =
            name
            |> CI
            |> typeSet.Add
        members
        |> Seq.map (fun property ->
            let casedPropertyName = settings.PropertyCasing.apply property.Name
            match property.Type |> Option.defaultValue UnresolvedBaseType with
            | _ when not (Char.IsLetter property.Name.[0]) ->
                raise (System.ArgumentException("Member names can only start with letters."))
            | GeneratedType x when property.Name
                                   |> CI
                                   |> set.Contains ->
                // This will make sure that class names do not collide with their outer members.
                let className = joinStringsWithSpaceSeparation [ name; property.Name ] |> settings.TypeCasing.apply
                [ CSharpClass x className set settings
                  classProperty
                      ([ settings.Prefix; className; settings.Suffix ]
                       |> joinStringsWithSpaceSeparation
                       |> settings.TypeCasing.apply) casedPropertyName ]
                |> joinStringsWithSpaceSeparation
            | GeneratedType x ->
                [ CSharpClass x property.Name set settings
                  classProperty
                      ([ settings.Prefix; property.Name; settings.Suffix ]
                       |> joinStringsWithSpaceSeparation
                       |> settings.TypeCasing.apply) (casedPropertyName) ]
                |> joinStringsWithSpaceSeparation
            | ArrayType x ->
                let className =
                    joinStringsWithSpaceSeparation [ name; property.Name ]
                    |> settings.TypeCasing.apply
                    |> Option.Some
                
                CSharpArray x (casedPropertyName) set className settings
            | BaseType x -> CSharpProperty x casedPropertyName)
        |> joinStringsWithSpaceSeparation
        |> (fun x ->

        let formattedName =
            if true then
                [ settings.Prefix; name; settings.Suffix ]
                |> joinStringsWithSpaceSeparation
                |> settings.TypeCasing.apply
            else
                name
        Formatters.``class`` formattedName x)
    and CSharpArray ``type`` key (typeSet: CIString Set ) (typeName: string Option) settings =
        match ``type`` with
        | BaseType x ->
            if not typeSet.IsEmpty then
                baseTypeArray x key
            else
                [ settings.Prefix; key; settings.Suffix ]
                |> joinStringsWithSpaceSeparation
                |> settings.PropertyCasing.apply
                |> baseTypeArray x 
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
            [ CSharpClass x typeName typeSet settings
              arrayProperty ]
            |> joinStringsWithSpaceSeparation
        | GeneratedType x ->
            let classDecleration = CSharpClass x key typeSet settings
            if typeSet.IsEmpty then
                classDecleration
            else
                let arrayProperty =
                    Formatters.arrayProperty
                        ([ settings.Prefix; key; settings.Suffix ]
                         |> joinStringsWithSpaceSeparation
                         |> settings.TypeCasing.apply) key
                [ classDecleration; arrayProperty ] |> joinStringsWithSpaceSeparation
        | ArrayType x -> CSharpArray x key typeSet Option.None settings

