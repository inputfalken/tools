namespace JsonParser

module internal TryParse =
    let private tryParseWith (tryParseFunc: string -> (bool * _)) =
        tryParseFunc
        >> function
        | true, v -> Some v
        | false, _ -> None

    let (|Date|_|) = tryParseWith System.DateTime.TryParse
    let (|Guid|_|) = tryParseWith System.Guid.TryParse
    let (|Int|_|) = tryParseWith System.Int32.TryParse
    let (|Double|_|) = tryParseWith System.Double.TryParse
