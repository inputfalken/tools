namespace TemplateFactory
open FSharp.Data
open System
open FSharp.Data.Runtime


module public CSharp =
    type public Key = string
    type public NameSpace = string
    type public Value =
                | Double of double
                | Decimal of decimal
                | String of string
                | DateTime of DateTime
                | Boolean of Boolean
                | Array of Value seq
                | Guid of Guid
                | Null
                | Object of Property seq
    and public Property = Key * Value
    type public File = {
        NameSpace : NameSpace
        Data : Value
    }

    let private stringParser =
         function
            | TryParse.Date x -> Value.DateTime x
            | TryParse.Guid x -> Value.Guid x
            | x -> Value.String x

    // JSON is built on two structures:
    // 1: A collection of name/value pairs
    // 2: An ordered list of values.
    let rec private map (value : JsonValue) =
         match value with
         | JsonValue.Number x -> Value.Decimal(x)
         | JsonValue.Float x -> Value.Decimal(decimal x)
         | JsonValue.String x -> stringParser x
         | JsonValue.Boolean x -> Value.Boolean(x)
         | JsonValue.Null -> Value.Null
         | JsonValue.Array x -> x |> Seq.map (fun x -> map x) |> Value.Array
         | JsonValue.Record x -> x |> Seq.map (fun x -> x ||> propertyMap) |> Value.Object
    and private propertyMap (key : string) (value : JsonValue) = (key, map value)

    let public parseJson input =
        {
            NameSpace = "TODO"
            Data = input |> JsonValue.Parse |> map
        }

    let public CreateFile(file : File) : string =
        let rec stringifyObject property : string =
            let (key, value) = property
            let typeKey = key |> NameUtils.nicePascalName
            let className = typeKey + "Model"
            let stringifiedValue = stringifyValue value

            match value with
            | Value.Object x -> sprintf "public class %s { %s }" className stringifiedValue
            | _ -> sprintf "public %s %s { get; set; }" stringifiedValue typeKey

        and stringifyValue value : string =
            match value with
            | DateTime x -> "System.DateTime"
            | Decimal x -> "decimal"
            | String x -> "string"
            | Boolean x -> "bool"
            | Guid x -> "System.Guid"
            | Double x -> "double"
            | Null -> String.Empty
            | Array x -> x |> Seq.map stringifyValue |> Seq.reduce (fun x y -> x + y)
            | Object x -> x |> Seq.fold (fun acc x -> stringifyObject x) String.Empty

        sprintf "namespace %s {%s}" file.NameSpace (stringifyValue (file.Data))
