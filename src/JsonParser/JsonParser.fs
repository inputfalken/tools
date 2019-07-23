namespace JsonParser
open FSharp.Data
open FSharp.Data.Runtime.NameUtils

module public Json =
    let private stringParser =
         function
            | TryParse.Date x -> DateTime x
            | TryParse.Guid x -> Guid x
            | x -> Value.String x

    // JSON is built on two structures:
    // 1: A collection of name/value pairs
    // 2: An ordered list of values.
    let rec private map (value : JsonValue) casing =
         match value with
         | JsonValue.Number x -> Decimal(x)
         | JsonValue.Float x -> Decimal(decimal x)
         | JsonValue.String x -> stringParser x
         | JsonValue.Boolean x -> Boolean(x)
         | JsonValue.Null -> Null
         | JsonValue.Array x -> x |> Seq.map (fun x -> map x casing) |> Array
         | JsonValue.Record x -> x |> Seq.map (fun x -> x ||> propertyMap <| casing) |> Object
    and private propertyMap (key : string) (value : JsonValue) casing =
        key
        |> match casing with
           | Pascal ->  nicePascalName
           | Camel -> niceCamelName
        |> (fun x -> (x, map value casing))
    
    let parse (input: string) (casing : CasingRule) =
         input |> JsonValue.Parse |> (fun x -> map x casing)  



