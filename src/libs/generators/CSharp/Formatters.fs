namespace CSharp.Formatters
    open Common.StringUtils
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

