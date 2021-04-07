module CSharpParser

open System.ComponentModel.DataAnnotations
open FParsec

let test p str =
    match run p str with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let digitOrLetter: Parser<char, unit> = letter <|> digit

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

let createTableParser<'a> =
    spaces
    >>. pstringCI "CREATE"
    >>. spaces1
    >>. pstringCI "TABLE"
    >>. spaces1
    >>. tableNameParser

type TableCreationDataType =
    | DateTime2 of {| Presicion: int option |}
    | Int

let dateTime2Parser<'a> =
    let withPrecision =
        pstringCI "DATETIME2"
        >>. spaces
        >>. (pchar '(' >>. pint32 .>> pchar ')')
        |>> (fun x -> TableCreationDataType.DateTime2 {| Presicion = Some x |})

    let withoutPrecision =
        pstringCI "DATETIME2" .>> spaces
        |>> (fun _ -> TableCreationDataType.DateTime2 {| Presicion = Option.None |})

    attempt withPrecision <|> attempt withoutPrecision

let intParser<'a> =
    pstringCI "INT"
    |>> (fun _ -> TableCreationDataType.Int)

let tableCreationColumnParser<'a> =
    let columnParser =
        spaces >>. many1Chars digitOrLetter .>> spaces1
        .>>. (dateTime2Parser <|> intParser)
        |>> (fun (x, y) -> {| Name = x; DataType = y |})
        .>> spaces

    pchar '(' >>. sepBy1 columnParser (pchar ',')
    .>> pchar ')'

// TODO Create table SQL -> CSharp class with properties from the tables columns.
// TODO then use SELECT .. FROM {table} to generate class with properties names pre written but all have objects as their type.

let testing s =
    let parser =
        createTableParser
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
