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

type TimePrecision = { Precision: int }

type CharSize =
    | Max
    | Value of int

type TableCreationDataType =
    | Char of CharSize Option
    | Date
    | DateTime
    | DateTime2 of TimePrecision option
    | DateTimeOffset of TimePrecision option
    | Int
    | NChar of CharSize Option
    | Nvarchar of CharSize Option
    | SmallDateTime
    | Time of TimePrecision option
    | UniqueIdentifier
    | Varchar of CharSize Option

let tableCreationDataTypeParser<'a> x y : Parser<TableCreationDataType, 'a> = pstringCI x |>> (fun _ -> y)

let tableCreationTimePrecisionParser<'a> x y : Parser<TableCreationDataType, 'a> =
    let precisionParser = betweenParentheses pint32
    let keyWordParser = pstringCI x

    let withPrecision =
        keyWordParser >>. spaces >>. precisionParser
        |>> (fun x -> { Precision = x } |> Some |> y)

    let withoutPrecision =
        keyWordParser .>> spaces |>> (fun _ -> None |> y)

    attempt withPrecision <|> withoutPrecision

let tableCreationTextParser<'a> x y : Parser<TableCreationDataType, 'a> =
    let charSizeParser : Parser<CharSize, 'a> =
        pstringCI "MAX" |>> (fun _ -> CharSize.Max)
        <|> (pint32 |>> CharSize.Value)

    let keyWordParser = pstringCI x

    let withParam =
        keyWordParser
        >>. betweenParentheses charSizeParser
        |>> (fun x -> Some x |> y)

    let withoutParam = keyWordParser |>> (fun _ -> None |> y)

    attempt withParam <|> withoutParam

let tableCreationColumnParser<'a> =
    let columnParser =
        let dataTypes =
            seq {
                tableCreationTimePrecisionParser "DATETIME2" TableCreationDataType.DateTime2
                tableCreationTimePrecisionParser "DATETIMEOFFSET" TableCreationDataType.DateTimeOffset
                tableCreationTimePrecisionParser "TIME" TableCreationDataType.Time
                tableCreationDataTypeParser "INT" TableCreationDataType.Int
                tableCreationDataTypeParser "DATETIME" TableCreationDataType.DateTime
                tableCreationDataTypeParser "DATE" TableCreationDataType.Date
                tableCreationDataTypeParser "UNIQUEIDENTIFIER" TableCreationDataType.UniqueIdentifier
                tableCreationDataTypeParser "SMALLDATETIME" TableCreationDataType.SmallDateTime
                tableCreationTextParser "NVARCHAR" TableCreationDataType.Nvarchar
                tableCreationTextParser "VARCHAR" TableCreationDataType.Varchar
                tableCreationTextParser "NCHAR" TableCreationDataType.NChar
                tableCreationTextParser "CHAR" TableCreationDataType.Char
            }
        let dataTypeDeclarations = choice dataTypes
        spaces >>. many1Chars digitOrLetter .>> spaces1
        .>>. dataTypeDeclarations
        |>> (fun (x, y) -> {| Name = x; DataType = y |})
        .>> spaces

    // TODO validate uniqueness of column names
    sepBy1 columnParser (pchar ',')
    |> betweenParentheses

let sample = @"
CREATE TABLE Persons (
    PersonID int,
    LastName varchar(255),
    FirstName varchar(255),
    Address varchar(255),
    City varchar(255)
)
"

let watch = System.Diagnostics.Stopwatch.StartNew()

createTableParser .>> spaces
.>>. tableCreationColumnParser
|> test
<| sample

printfn $"{watch.Elapsed}"

// TODO Create table SQL -> CSharp class with properties from the tables columns.
// TODO then use SELECT .. FROM {table} to generate class with properties names pre written but all have objects as their type.
