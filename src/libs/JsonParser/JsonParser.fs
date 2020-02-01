namespace JsonParser

open Common.Casing
open FSharp.Data

module public Json =
    let private stringParser =
        function
        | TryParse.Date x -> DateTime x
        | TryParse.Guid x -> Guid x
        | x -> Value.String x

    let rec private map value casing =
        match value with
        | JsonValue.Number x -> Decimal(x)
        | JsonValue.Float x -> Decimal(decimal x)
        | JsonValue.String x -> stringParser x
        | JsonValue.Boolean x -> Boolean(x)
        | JsonValue.Null -> Null
        | JsonValue.Array x ->
            x
            |> Array.map (fun x -> map x casing)
            |> Array
        | JsonValue.Record x ->
            x
            |> Array.map (fun (x, y) -> propertyMap x y casing)
            |> Object

    and private propertyMap key value (casing: Casing) =
        { Key = casing.apply key
          Value = map value casing }

    let parse input casing =
        input
        |> JsonValue.Parse
        |> (fun x -> map x casing)
