﻿#if INTERACTIVE
#I @"bin\Debug\net5.0"
#r "Languages"
#r "nuget: FParsec"
#else
module TableCreationSQLParser
#endif

open FParsec

type TimePrecisionArgument = { Precision: int }

type CharSizeArgument =
    | Max
    | Value of int

type Identity = { Seed: int; Increment: int }
type DecimalArguments = { Precision: int; Scale: int }

type TableCreationDataType =
    | Decimal of DecimalArguments option
    | Char of CharSizeArgument Option
    | Date
    | Bit
    | DateTime
    | DateTime2 of TimePrecisionArgument option
    | DateTimeOffset of TimePrecisionArgument option
    | Int of Identity option
    | NChar of CharSizeArgument Option
    | Nvarchar of CharSizeArgument Option
    | SmallDateTime
    | Time of TimePrecisionArgument option
    | UniqueIdentifier
    | Varchar of CharSizeArgument Option

type SQLColumn =
    { DataType: TableCreationDataType
      Name: string }

type SQLParseResult =
    { Columns: SQLColumn list
      TableName: string
      Schema: string option }

let test p str =
    match run p str with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let private digitOrLetter : Parser<char, unit> = letter <|> digit
let betweenParentheses parser = pchar '(' >>. parser .>> pchar ')'

let private tableNameParser<'a> : Parser<{| Schema: string option
                                            Table: string |}, unit> =
    let digitOrLetters = many1Chars digitOrLetter

    let withSchema =
        digitOrLetters .>>? pchar '.' .>>. digitOrLetters
        |>> (fun (x, y) -> {| Schema = Option.Some x; Table = y |})

    let withoutSchema =
        digitOrLetters .>> (notFollowedBy <| pchar '.')
        |>> (fun x -> {| Schema = Option.None; Table = x |})

    withSchema <|> withoutSchema

let private createTableParser<'a> =
    spaces
    >>. pstringCI "CREATE"
    >>. spaces1
    >>. pstringCI "TABLE"
    >>. spaces1
    >>. tableNameParser

let tableCreationDataTypeParser<'a> x y : Parser<TableCreationDataType, 'a> = pstringCI x |>> (fun _ -> y)

let private tableCreationTimePrecisionParser<'a> x y : Parser<TableCreationDataType, 'a> =
    let precisionParser = betweenParentheses pint32
    let keyWordParser = pstringCI x

    let withPrecision =
        keyWordParser >>? spaces >>? precisionParser
        |>> (fun x -> { Precision = x } |> Some |> y)

    let withoutPrecision =
        keyWordParser .>>? spaces |>> (fun _ -> None |> y)

    withPrecision <|> withoutPrecision

let private tableCreationTextParser<'a> x y : Parser<TableCreationDataType, 'a> =
    let charSizeParser : Parser<CharSizeArgument, 'a> =
        pstringCI "MAX"
        |>> (fun _ -> CharSizeArgument.Max)
        <|> (pint32 |>> CharSizeArgument.Value)

    let keyWordParser = pstringCI x

    let withParam =
        keyWordParser
        >>? spaces
        >>? betweenParentheses charSizeParser
        |>> (fun x -> Some x |> y)

    let withoutParam = keyWordParser |>> (fun _ -> None |> y)

    withParam <|> withoutParam

let private tableCreationIntParser<'a> : Parser<TableCreationDataType, 'a> =
    let parameterExtraction =
        spaces >>. pint32
        .>> spaces
        .>> pchar ','
        .>> spaces
        .>>. pint32
        .>> spaces
        |>> (fun (x, y) -> { Seed = x; Increment = y })

    let withParam = betweenParentheses parameterExtraction

    let int =
        tableCreationDataTypeParser "INT" (TableCreationDataType.Int None)

    let identityWithoutArgument =
        int .>>? spaces1
        .>>. pstringCI "IDENTITY"
        .>>. spaces
        .>> notFollowedByL
                (pchar '(')
                "Invalid `IDENTITY` syntax; Allowed syntax: `IDENTITY` or `IDENTITY ({int}, {int})`"
        |>> (fun _ -> Option.None)
        |>> TableCreationDataType.Int

    let identityWithArgument =
        int >>? spaces1
        >>. pstringCI "IDENTITY"
        >>. spaces
        >>? withParam
        |>> Some
        |>> TableCreationDataType.Int

    choice
    <| seq {
        identityWithArgument
        identityWithoutArgument
        int
       }
let tableCreationDecimalParser<'a> : Parser<TableCreationDataType, 'a> =
    let parameterExtraction =
        spaces >>. pint32
        .>> spaces
        .>> pchar ','
        .>> spaces
        .>>. pint32
        .>> spaces
        |>> (fun (x, y) -> { Precision = x; Scale = y })

    let withParam = betweenParentheses parameterExtraction
    let decimal = tableCreationDataTypeParser "DECIMAL" <| TableCreationDataType.Decimal None
    
    decimal .>> spaces >>? withParam  |>> (fun x -> TableCreationDataType.Decimal (Some x) ) <|>  decimal

let private tableCreationColumnParser<'a> =
    let columnParser =
        let dataTypes =
            seq {
                tableCreationTimePrecisionParser "DATETIME2" TableCreationDataType.DateTime2
                tableCreationTimePrecisionParser "DATETIMEOFFSET" TableCreationDataType.DateTimeOffset
                tableCreationTimePrecisionParser "TIME" TableCreationDataType.Time
                tableCreationIntParser
                tableCreationDecimalParser
                tableCreationDataTypeParser "BIT" TableCreationDataType.Bit
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
        |>> (fun (x, y) -> { Name = x; DataType = y })
        .>> spaces

    // TODO throw error instead of  distinct the names
    sepBy1 columnParser (pchar ',')
    |> betweenParentheses
    |>> List.distinctBy (fun x -> x.Name.ToLower())

let public parse string =
    let parser =
        createTableParser .>> spaces
        .>>. tableCreationColumnParser
        |>> (fun (x, y) ->
            { Columns = y
              TableName = x.Table
              Schema = x.Schema })

    match run parser string with
    | Success (res, x, y) -> res |> Result.Ok
    | Failure (msg, x, y) -> msg |> Result.Error

// TODO Create table SQL -> CSharp class with properties from the tables columns.
// TODO then use SELECT .. FROM {table} to generate class with properties names pre written but all have objects as their type.
