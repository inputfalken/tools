module CSharpParser

open FParsec

let test p str =
    match run p str with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let digitOrLetter<'a> : Parser<char, unit> = letter <|> digit

let tableNameParser<'a> : Parser<{| Schema: string option
                                    Table: string |}, unit> =
    let digitOrLetters = many1Chars digitOrLetter

    let withSchema =
        digitOrLetters .>> pchar '.' .>>. digitOrLetters
        |>> (fun (x, y) -> {| Schema = Option.Some x; Table = y |})

    let withoutSchema =
        digitOrLetters .>> (notFollowedBy <| pchar '.')
        |>> (fun x -> {| Schema = Option.None; Table = x |})

    attempt withSchema <|> attempt withoutSchema

let dropTableParser<'a> =
    spaces >>. pstringCI "DROP" .>> spaces
    >>. pstringCI "TABLE"
    >>. spaces1
    >>. tableNameParser

let createTableParser<'a> =
    spaces
    >>. pstringCI "CREATE"
    >>. spaces1
    >>. pstringCI "TABLE"
    >>. spaces1
    >>. tableNameParser

let tableCreationColumnParser<'a> =
    let columnParser =
        many1Chars digitOrLetter .>> spaces1
        .>>. many1Chars digitOrLetter
        |>> (fun (x, y) -> {| Column = x; DataType = y |})

    pchar '(' >>. sepBy1 columnParser (pchar ',')
    .>> pchar ')'

let testing s =
    let parser =
        dropTableParser <|> createTableParser
        |>> (fun x ->
            sprintf
                "You tried to create or delete table %s%s"
                x.Table
                (x.Schema
                 |> Option.map (sprintf " on schema %s.")
                 |> Option.defaultValue "."))

    parser |> test <| s

testing "CREATE TABLE dbo.Foo"
testing "CREATE TABLE Foo"
testing "CREATE TABLE dbo."
