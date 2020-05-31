namespace JsonParser

open System
open System.Globalization
open FSharp.Data
open FSharp.Data.Runtime

module public Json =
    let private culture = CultureInfo.InvariantCulture
    let private emptyArray = Array.empty<String>
    let rec private map (value: JsonValue) =
        JsonConversions.AsInteger culture value
        |> Option.map Value.Int
        |> Option.orElseWith (fun () -> JsonConversions.AsDecimal culture value |> Option.map Value.Decimal)
        |> Option.orElseWith (fun () -> JsonConversions.AsFloat emptyArray true culture value |> Option.map Decimal |> Option.map Value.Decimal)
        |> Option.orElseWith (fun () -> JsonConversions.AsGuid value |> Option.map Value.Guid)
        |> Option.orElseWith (fun () -> JsonConversions.AsDateTime culture value |> Option.map Value.DateTime)
        |> Option.orElseWith (fun () -> JsonConversions.AsBoolean value |> Option.map Value.Boolean)
        |> Option.defaultWith (fun () ->
            match value with
            | JsonValue.String x -> Value.String x
            | JsonValue.Null -> Value.Null
            | JsonValue.Array x ->
                x
                |> Array.map map
                |> Value.Array
            | JsonValue.Record x ->
                x
                |> Array.map (fun (x, y) ->
                    { Key = x
                      Value = map y })
                |> Value.Object)

    let public parse input =
        input
        |> JsonValue.Parse
        |> map
