namespace Common

module public StringValidator =
    let public valueExists input =
        input
        |> Option.Some
        |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))
        |> Option.map (fun x -> x.Trim())

module public StringJoin =
    let joinStrings strings: string =
        strings |> String.concat System.String.Empty
        
    let joinStringsWithSpaceSeparation strings: string =
        strings |> String.concat " "
