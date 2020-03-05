namespace JsonParser

open Common.Casing
open FSharp.Data

module public Json =
    let private stringParser =
        function
        | TryParse.Date x -> DateTime x
        | TryParse.Guid x -> Guid x
        | x -> Value.String x

    let rec private map value =
        match value with
        | JsonValue.Number x -> Decimal(x)
        | JsonValue.Float x -> Decimal(decimal x)
        | JsonValue.String x -> stringParser x
        | JsonValue.Boolean x -> Boolean(x)
        | JsonValue.Null -> Null
        | JsonValue.Array x ->
            x
            |> Array.map map
            |> Array
        | JsonValue.Record x ->
            x
            |> Array.map (fun (x, y) -> propertyMap x y )
            |> Object

    and private propertyMap key value  =
        { Key =  key
          Value = map value }

    let parse input =
        input
        |> JsonValue.Parse
        |> (fun x -> map x )
