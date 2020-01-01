module public Common.StringValidator

let public valueExists input =
    input
    |> Option.Some
    |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))
    |> Option.map (fun x -> x.Trim())
