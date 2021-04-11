#if INTERACTIVE
#I @"bin\Debug\net5.0"
#r "Languages"
#r "nuget: FParsec"
#else
module CSharpParser
#endif

open FParsec

type TimePrecisionArgument = { Precision: int }

type CharSizeArgument =
    | Max
    | Value of int

type Identity =
    { Argument: {| Seed: int; Increment: int |} option }

type TableCreationDataType =
    | Char of CharSizeArgument Option
    | Date
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
        digitOrLetters .>>? pchar '.' .>>. digitOrLetters
        |>> (fun (x, y) -> {| Schema = Option.Some x; Table = y |})

    let withoutSchema =
        digitOrLetters .>> (notFollowedBy <| pchar '.')
        |>> (fun x -> {| Schema = Option.None; Table = x |})

    withSchema <|> withoutSchema

let createTableParser<'a> =
    spaces
    >>. pstringCI "CREATE"
    >>. spaces1
    >>. pstringCI "TABLE"
    >>. spaces1
    >>. tableNameParser

let tableCreationDataTypeParser<'a> x y : Parser<TableCreationDataType, 'a> = pstringCI x |>> (fun _ -> y)

let tableCreationTimePrecisionParser<'a> x y : Parser<TableCreationDataType, 'a> =
    let precisionParser = betweenParentheses pint32
    let keyWordParser = pstringCI x

    let withPrecision =
        keyWordParser >>? spaces >>? precisionParser
        |>> (fun x -> { Precision = x } |> Some |> y)

    let withoutPrecision =
        keyWordParser .>>? spaces |>> (fun _ -> None |> y)

    withPrecision <|> withoutPrecision

let tableCreationTextParser<'a> x y : Parser<TableCreationDataType, 'a> =
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

let tableCreationIntParser<'a> : Parser<TableCreationDataType, 'a> =
    let parameterExtraction =
        spaces >>. pint32
        .>> spaces
        .>> pchar ','
        .>> spaces
        .>>. pint32
        .>> spaces
        |>> (fun (x, y) -> {| Seed = x; Increment = y |})

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
        |>> (fun _ -> { Argument = option.None })
        |>> Some
        |>> TableCreationDataType.Int

    let identityWithArgument =
        int >>? spaces1
        >>. pstringCI "IDENTITY"
        >>. spaces
        >>? withParam
        |>> Some
        |>> (fun x -> { Argument = x })
        |>> Some
        |>> TableCreationDataType.Int

    choice
    <| seq {
        identityWithArgument
        identityWithoutArgument
        int
       }

let tableCreationColumnParser<'a> =
    let columnParser =
        let dataTypes =
            seq {
                tableCreationTimePrecisionParser "DATETIME2" TableCreationDataType.DateTime2
                tableCreationTimePrecisionParser "DATETIMEOFFSET" TableCreationDataType.DateTimeOffset
                tableCreationTimePrecisionParser "TIME" TableCreationDataType.Time
                tableCreationIntParser
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

    // TODO throw error instead of  distinct the names
    sepBy1 columnParser (pchar ',')
    |> betweenParentheses
    |>> List.distinctBy (fun x -> x.Name.ToLower())

let sample = @"
CREATE TABLE Persons (
    Id int IDENTITY (1,1),
    Length int,
    LastName varchar(255),
    FirstName varchar(255),
    Address varchar(255),
    City varchar(255)
)
"


createTableParser .>> spaces
.>>. tableCreationColumnParser
|> test
<| sample


// TODO Create table SQL -> CSharp class with properties from the tables columns.
// TODO then use SELECT .. FROM {table} to generate class with properties names pre written but all have objects as their type.
