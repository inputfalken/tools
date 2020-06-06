namespace JsonParser

open System.Globalization
open FSharp.Data
open FSharp.Data.Runtime
open System

type public Value =
    | Int of int
    | Double of double
    | Decimal of decimal
    | String of string
    | DateTime of DateTime
    | Boolean of Boolean
    | Array of Value []
    | Guid of Guid
    | Null
    | Object of Record []

and public Record =
    { Key: String
      Value: Value }

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
            match x with
            | JsonValue.Boolean x -> Value.Boolean x
            | JsonValue.Null -> Value.Null
            | x ->
                let option =
                    JsonConversions.AsInteger culture x
                    |> Option.map Value.Int
                    |> Option.orElseWith (fun () -> JsonConversions.AsGuid x |> Option.map Value.Guid)
                    |> Option.orElseWith
                        (fun () -> JsonConversions.AsDateTime culture x |> Option.map Value.DateTime)
                match option with
                | Some x -> x
                | _ ->
                    match x with
                    | JsonValue.String x -> Value.String x
                    | JsonValue.Number x -> Value.Decimal x
                    | _ -> failwith "Should never happen."


    let public parse input =
        input
        |> JsonValue.Parse
        |> map
