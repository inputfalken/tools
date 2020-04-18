namespace JsonParser

open FSharp.Data
open FSharp.Data.Runtime

module public Json =
    let private stringParser =
        function
        | TryParse.Date x -> DateTime x
        | TryParse.Guid x -> Guid x
        | x -> Value.String x

    // TODO use the JsonConversions type in order to translate into custom types.
    // link: https://github.com/fsharp/FSharp.Data/blob/master/src/Json/JsonConversions.fs
    let rec private map value =
        match value with
        | JsonValue.Number x ->
            // We only get decimals here, so we need to check if the number is a integer before mapping to int.
            if x % 1m = 0m then Int <| int x else Decimal(x)
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
            |> Array.map (fun (x, y) ->
                { Key = x
                  Value = map y })
            |> Object

    let public parse input =
        input
        |> JsonValue.Parse
        |> (fun x -> map x)
