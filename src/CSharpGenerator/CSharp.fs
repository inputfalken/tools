namespace TemplateFactory
open JsonParser
open System
open System

module private stringValidators =
    let valueExists input =
       input
       |> Option.Some
       |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))

module private Formatters =
    let ``class`` name content =
        sprintf "public class %s { %s }" name content

    let ``namespace`` name content =
        sprintf "namespace %s { %s }" name content

    let property ``type`` name : string =
        sprintf "public %s %s { get; set; }" ``type`` name

    let arrayProperty ``type`` name =
        property (sprintf "%s[]" ``type``) name

type Settings() =
        member val Casing = "" with get, set
        member val NameSpace = "" with get, set
        member val ClassPrefix = "" with get, set
        member val ClassSuffix = "" with get, set
        member val RootObjectName = "" with get, set

type CSharp =
    static member CreateFile(input : string) =
        CSharp.CreateFile(input, new Settings())

    static member CreateFile(input : string, settings : Settings) =
        let data = (input, settings.Casing |> CasingRule.fromString |> Option.defaultValue CasingRule.Pascal) ||> Json.parse
        let classPrefix = settings.ClassPrefix
                          |> stringValidators.valueExists
                          |> Option.defaultValue String.Empty
        let classSuffix = settings.ClassSuffix
                          |> stringValidators.valueExists
                          |> Option.defaultValue "Model"
        let rootObject = settings.RootObjectName
                         |> stringValidators.valueExists
                         |> Option.defaultValue "Root"
                         |> (fun x -> sprintf "%s%s%s" classPrefix x classSuffix)


        let rec stringifyValue value : string =
            match value with
            | DateTime x -> "System.DateTime"
            | Decimal x -> "decimal"
            | String x -> "string"
            | Boolean x -> "bool"
            | Guid x -> "System.Guid"
            | Double x -> "double"
            | Null -> String.Empty
            | Array x -> stringifyArray rootObject x
            | Object x -> x |> stringifyObject

        and stringifyArray (key : string) (value : Value seq) =
            value
            |> Seq.fold (fun x y ->
                let (x, _) = x
                match y with
                | Object x ->
                     x
                     |> stringifyObject
                     |> Formatters.``class`` key
                     |> (fun x -> (x, true))
                | _ ->
                    let value = y |> stringifyValue
                    if x = String.Empty then value
                    else if value = x then value
                    else "object"
                    |> (fun x -> (x, false))

            ) (String.Empty, false)
            |> (fun (x, y) -> if y then x else Formatters.arrayProperty (if x = String.Empty then "object" else x) key)

        and stringifyObject (properties : Property seq)  : string =
            properties
            |> Seq.mapi (fun x y -> (x, y))
            |> Seq.fold (fun acc (index, (key, value)) ->
                let className = classPrefix + key + classSuffix
                let stringifiedValue = stringifyValue value
                let space = (if index <> 0 then " " else String.Empty)
                let result = match value with
                             | Object x -> Formatters.``class`` className stringifiedValue |> (fun x -> sprintf "%s%s" space x)
                             | Array x -> stringifyArray key x |> (fun x -> sprintf "%s%s" space x)
                             | _ -> Formatters.property stringifiedValue key |> (fun x -> sprintf "%s%s" space x)
                acc + result
            ) String.Empty
            
        let namespaceFormatter = settings.NameSpace
                                 |> stringValidators.valueExists
                                 |> Option.map (fun x -> Formatters.``namespace`` x)
                                 |> Option.defaultValue (sprintf "%s")

        let error = """
            JSON is built on two structures:
            1: A collection of name/value pairs
            2: An ordered list of values.
        """
        match data with
        | Array x -> x |> stringifyArray rootObject
        | Object x -> x |> stringifyObject |> Formatters.``class`` rootObject
        | _ -> raise (new ArgumentException(error))
        |> namespaceFormatter
