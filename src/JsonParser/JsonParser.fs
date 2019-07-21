namespace JsonParser
open FSharp.Data

module public Json =
    let private stringParser =
         function
            | TryParse.Date x -> DateTime x
            | TryParse.Guid x -> Guid x
            | x -> Value.String x

    // JSON is built on two structures:
    // 1: A collection of name/value pairs
    // 2: An ordered list of values.
    let rec private map (value : JsonValue) =
         match value with
         | JsonValue.Number x -> Decimal(x)
         | JsonValue.Float x -> Decimal(decimal x)
         | JsonValue.String x -> stringParser x
         | JsonValue.Boolean x -> Boolean(x)
         | JsonValue.Null -> Null
         | JsonValue.Array x -> x |> Seq.map (fun x -> map x) |> Array
         | JsonValue.Record x -> x |> Seq.map (fun x -> x ||> propertyMap) |> Object
    and private propertyMap (key : string) (value : JsonValue) = (key |> FSharp.Data.Runtime.NameUtils.nicePascalName, map value)

    // this will always produce pascal case currently.
    let parse input nameSpace = input |> JsonValue.Parse |> map



