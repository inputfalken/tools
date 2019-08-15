namespace TemplateFactory
open Formatters
open JsonParser
open System

module private stringValidators =
    let valueExists input =
       input
       |> Option.Some
       |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))

type CSharp =
    static member CreateFile(input: string) =
        CSharp.CreateFile(input, new Settings())

    static member CreateFile(input: string, settings: Settings) =
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

        let rec stringifyValue value: string =
            match value with
            | DateTime x -> "System.DateTime"
            | Decimal x -> "decimal"
            | String x -> "string"
            | Boolean x -> "bool"
            | Guid x -> "System.Guid"
            | Double x -> "double"
            | Null -> String.Empty
            | _ -> raise (new Exception("Array or object can never be resolved from value."))

        and stringifyArray (value: Value seq) (key: string): string =
            if Seq.isEmpty value then "object" |> (fun x -> Formatters.arrayProperty x key)
            else
                 value
                 |> Seq.map (fun x ->
                     match x with
                     | Object x -> stringifyObject x rootObject
                     | x -> stringifyValue x |> (fun x -> (x, option.None))
                 )
                 |> Seq.reduce (fun x y -> if x = y then y else ("object", option.None))
                 |> (fun (x, y) -> if y.IsSome then x + " " + (Formatters.arrayProperty y.Value key) else Formatters.arrayProperty x key)

        and stringifyObject (properties: Property seq) (key: string): string * string option =
            let key = classPrefix + key + classSuffix
            properties
            |> Seq.mapi (fun index property ->
                let space = (if index <> 0 then " " else String.Empty)
                match property.Value with
                | Object x -> stringifyObject x property.Key |> (fun (x, y) -> x)
                | Array x ->
                    let stringifiedValue = stringifyArray x property.Key
                    sprintf "%s%s" space stringifiedValue
                | x ->
                    let stringifiedValue = stringifyValue x
                    let formatted = Formatters.property stringifiedValue property.Key |> (fun x -> sprintf "%s%s" space x)
                    formatted
            )
            |> Seq.reduce (fun acc curr -> acc + curr)
            |> (fun x -> (Formatters.``class`` key x, option.Some key))

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
        | Array x -> stringifyArray x "Items"
        | Object x -> x |> stringifyObject <| rootObject |> (fun (x, y) -> x)
        | _ -> raise (new ArgumentException(error))
        |> namespaceFormatter
