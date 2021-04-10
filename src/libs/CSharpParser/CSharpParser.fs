module CSharpParser

open FParsec

let test p str =
    match run p str with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let digitOrLetter : Parser<char, unit> = letter <|> digit

let betweenParentheses parser = pchar '(' >>. parser .>> pchar ')'

let tableNameParser<'a> : Parser<{| Schema: string option
                                    Table: string |}, unit> =
    let digitOrLetters = many1Chars digitOrLetter

    let withSchema =
        digitOrLetters .>> pchar '.' .>>. digitOrLetters
        |>> (fun (x, y) -> {| Schema = Option.Some x; Table = y |})

    let withoutSchema =
        digitOrLetters .>> (notFollowedBy <| pchar '.')
        |>> (fun x -> {| Schema = Option.None; Table = x |})

    attempt withSchema <|> withoutSchema

let createTableParser<'a> =
    spaces
    >>. pstringCI "CREATE"
    >>. spaces1
    >>. pstringCI "TABLE"
    >>. spaces1
    >>. tableNameParser

type CharSize =
    | Max
    | Value of int

let charSizeParser<'a> : Parser<CharSize, 'a> =
    pstringCI "MAX" |>> (fun _ -> CharSize.Max)
    <|> (pint32 |>> CharSize.Value)

type TableCreationDataType =
    | DateTime2 of {| Presicion: int option |}
    | DateTime
    | Int
    | Date
    | UniqueIdentifier
    | Nvarchar of CharSize Option
    | Varchar of CharSize Option
    | Char of CharSize Option
    | NChar of CharSize Option

let tableCreationDataTypeParser<'a> x y : Parser<TableCreationDataType, 'a> = pstringCI x |>> (fun _ -> y)

let dateTime2Parser<'a> : Parser<TableCreationDataType, 'a> =
    let withPrecision =
        pstringCI "DATETIME2"
        >>. spaces
        >>. betweenParentheses pint32
        |>> (fun x -> TableCreationDataType.DateTime2 {| Presicion = Some x |})

    let withoutPrecision =
        pstringCI "DATETIME2" .>> spaces
        |>> (fun _ -> TableCreationDataType.DateTime2 {| Presicion = Option.None |})

    attempt withPrecision <|> withoutPrecision

let tableCreationTextParser<'a> keyword abc : Parser<TableCreationDataType, 'a> =
    let typeParser = pstringCI keyword

    let withParam =
        typeParser >>. betweenParentheses charSizeParser
        |>> (fun x -> abc <| Some x)

    let withoutParam = typeParser |>> (fun _ -> abc None)

    attempt withParam <|> withoutParam

let tableCreationColumnParser<'a> =
    let columnParser =
        let tablecreationDataTypeParser =
            choice
                [
                  // NOTE order matters
                  dateTime2Parser
                  tableCreationDataTypeParser "INT" TableCreationDataType.Int
                  tableCreationDataTypeParser "DATETIME" TableCreationDataType.DateTime
                  tableCreationDataTypeParser "DATE" TableCreationDataType.Date
                  tableCreationDataTypeParser "UNIQUEIDENTIFIER" TableCreationDataType.UniqueIdentifier
                  tableCreationTextParser "NVARCHAR" TableCreationDataType.Nvarchar
                  tableCreationTextParser "VARCHAR" TableCreationDataType.Varchar
                  tableCreationTextParser "NCHAR" TableCreationDataType.NChar
                  tableCreationTextParser "CHAR" TableCreationDataType.Char ]

        spaces >>. many1Chars digitOrLetter .>> spaces1
        .>>. tablecreationDataTypeParser
        |>> (fun (x, y) -> {| Name = x; DataType = y |})
        .>> spaces

    // TODO validate uniqueness of column names
    sepBy1 columnParser (pchar ',')
    |> betweenParentheses

tableCreationColumnParser |> test
<| "(id int, timestamp datetime, timestampi datetime2)"

// TODO Create table SQL -> CSharp class with properties from the tables columns.
// TODO then use SELECT .. FROM {table} to generate class with properties names pre written but all have objects as their type.
