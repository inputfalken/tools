namespace TemplateFactory
open JsonParser
open System

module private stringValidators =
    let valueExists input =
       input
       |> Option.Some
       |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))
       
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
            if Seq.isEmpty value then Formatters.arrayProperty "object" key
            else
                value
                |> Seq.map (fun x ->
                    match x with
                    | Object x -> x |> stringifyObject |> (fun x -> Formatters.``class`` key x)
                    | x -> x |> stringifyValue |> (fun x -> Formatters.arrayProperty x key)
                )
                |> Seq.reduce (fun x y -> if y = x then y else "object" |> (fun x -> Formatters.arrayProperty x key))


        and stringifyObject (properties : Property seq) : string =
            properties
            |> Seq.mapi (fun index property ->
                let space = (if index <> 0 then " " else String.Empty)
                match property.Value with
                | Object x ->
                    let className = classPrefix + property.Key + classSuffix
                    let stringifiedValue = stringifyObject x
                    Formatters.``class`` className stringifiedValue |> (fun x -> sprintf "%s%s" space x)
                | Array x ->
                    let stringifiedValue = stringifyArray property.Key x
                    sprintf "%s%s" space stringifiedValue
                | x ->
                    let stringifiedValue = stringifyValue x
                    Formatters.property stringifiedValue property.Key |> (fun x -> sprintf "%s%s" space x)
            )
            |> Seq.reduce (fun acc curr -> acc + curr)

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
