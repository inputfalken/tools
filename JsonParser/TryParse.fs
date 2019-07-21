namespace JsonParser
module internal TryParse =
    // convenient, functional TryParse wrappers returning option<'a>
    let private tryParseWith (tryParseFunc : string -> (bool * _)) = tryParseFunc >> function
        | true, v -> Some v
        | false, _ -> None

    let private parseDate = tryParseWith System.DateTime.TryParse
    let private parseGuid = tryParseWith System.Guid.TryParse
    let private parseInt = tryParseWith System.Int32.TryParse
    let private parseSingle = tryParseWith System.Single.TryParse
    let private parseDouble = tryParseWith System.Double.TryParse
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|) = parseDate
    let (|Guid|_|) = parseGuid
    let (|Int|_|) = parseInt
    let (|Single|_|) = parseSingle
    let (|Double|_|) = parseDouble

