namespace JsonParser

open System.Globalization
open FSharp.Data
open FSharp.Data.Runtime

module public Json =
    let private culture = CultureInfo.InvariantCulture

    let rec private map (value: JsonValue) =
        match value with
        | JsonValue.Record x ->
            x
            |> Array.map (fun (x, y) ->
                { Key = x
                  Value = map y })
            |> Value.Object
        | JsonValue.Array x ->
            x
            |> Array.map map
            |> Value.Array
        | x ->
            
            JsonConversions.AsInteger culture x
            |> Option.map Value.Int
            |> Option.orElseWith (fun () -> JsonConversions.AsGuid x |> Option.map Value.Guid)
            |> Option.orElseWith (fun () -> JsonConversions.AsDateTime culture x |> Option.map Value.DateTime)
            |> Option.defaultWith (fun () ->
                match x with
                | JsonValue.String x -> Value.String x
                | JsonValue.Boolean x -> Value.Boolean x
                | JsonValue.Null -> Value.Null
                | JsonValue.Number x -> Value.Decimal x
                | JsonValue.Record _
                | JsonValue.Array _
                | JsonValue.Float _
                | JsonValue.Number _ -> failwith "Should never happen.")


    let public parse input =
        input
        |> JsonValue.Parse
        |> map
