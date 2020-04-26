module TemplateFactory.SQL.SQL

open Common.CaseInsensitiveString
open Common.StringJoin
open System
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

type GenerationType =
    | UserDefinedTableType
    | None

type Settings =
    { GenerationType: GenerationType }

// TODO create proper type with support for nullable value types.
let private string = CIString.CI "string"
let private int = CIString.CI "int"
let private int32 = CIString.CI "int32"
let private double = CIString.CI "double"
let private float = CIString.CI "float"
let private bool = CIString.CI "bool"
let private boolean = CIString.CI "boolean"
let private guid = CIString.CI "guid"
let private datetTime = CIString.CI "datetime"
let private intNullable = CIString.CI "int?"
let private int32Nullable = CIString.CI "int32?"
let private doubleNullable = CIString.CI "double?"
let private floatNullable = CIString.CI "float?"
let private boolNullable = CIString.CI "bool?"
let private booleanNullable = CIString.CI "boolean?"
let private guidNullable = CIString.CI "guid?"
let private datetTimeNullable = CIString.CI "datetime?"

type NVarCharArgument =
    | Max
    // We get type safety but negative numbers and 0 could still be passed which is not allowed.
    | Number of int

type SqlDataType =
    | DateTime
    | Int
    | Float
    | Bit
    | UniqueIdentifier
    | Nvarchar of NVarCharArgument

    static member NvarcharMax = NVarCharArgument.Max |> Nvarchar
    static member NvarcharNumber number = NVarCharArgument.Number number |> Nvarchar
    static member toSqlType(str: string): SqlDataType =
        let ciString = str.Replace("System.", System.String.Empty, StringComparison.OrdinalIgnoreCase) |> CIString.CI
        match ciString with
        | x when x.Equals int || x.Equals int32 || x.Equals intNullable || x.Equals int32Nullable -> SqlDataType.Int
        | x when x.Equals bool || x.Equals boolean || x.Equals boolNullable || x.Equals booleanNullable ->
            SqlDataType.Bit
        | x when x.Equals guid || x.Equals guidNullable -> SqlDataType.UniqueIdentifier
        | x when x.Equals datetTime || x.Equals datetTimeNullable -> SqlDataType.DateTime
        | x when x.Equals float || x.Equals double || x.Equals floatNullable || x.Equals doubleNullable ->
            SqlDataType.Float
        | x when x.Equals string -> SqlDataType.Nvarchar Max
        | _ -> raise (NotImplementedException(sprintf "Type '%s' is not supported." str))

    override x.ToString() =
        match x with
        | DateTime -> "datetime"
        | Int -> "int"
        | Bit -> "bit"
        | UniqueIdentifier -> "uniqueidentifier"
        | Float -> "float"
        | Nvarchar x ->
            match x with
            | Max -> "nvarchar(max)"
            | Number x -> sprintf "nvarchar(%d)" x

type Parameter =
    { Type: SqlDataType
      Name: string }

type UserDefined =
    { Parameters: Parameter list
      Name: string }

type ProcedureParameter =
    | DataType of Parameter
    | UserDefinedTableType of UserDefined

let rec formatProcedure name (arg: ProcedureParameter list): string =

    let userDefinedTypes =
        arg
        |> List.choose (fun x ->
            match x with
            | UserDefinedTableType x -> option.Some x
            | DataType _ -> option.None)

    let stringifiedParams =
        arg
        |> List.map (fun x ->
            match x with
            | DataType x ->
                let name =
                    match x.Name with
                    | x when x.[0] = '@' -> x
                    | x -> joinStrings [ "@"; x ]
                [ name
                  x.Type.ToString() ]
                |> joinStringsWithSpaceSeparation
            | UserDefinedTableType userDefined ->

                let param =
                    joinStringsWithSpaceSeparation
                        [ (joinStrings [ "@"; userDefined.Name ])
                          userDefined.Name
                          "READONLY" ]

                param)

    let userDefinedTypeProvided = not userDefinedTypes.IsEmpty

    let dropProcedure =
        if userDefinedTypeProvided
        then sprintf "DROP PROCEDURE IF EXISTS %s\nGO\n\n" name
        else System.String.Empty

    let userDefinedCreate =
        (userDefinedTypes
         |> List.map (fun x ->
             sprintf "CREATE TYPE %s AS TABLE (%s)\nGO" x.Name
                 (x.Parameters
                  |> List.map (fun x -> sprintf "%s %s" x.Name (x.Type.ToString()))
                  |> joinStringsWithCommaSpaceSeparation))
         |> String.concat "\n\n")
        + if userDefinedTypeProvided then "\n\n" else System.String.Empty

    let userDefinedDrop =
        (userDefinedTypes
         |> List.map (fun x ->
             sprintf "IF type_id('%s') IS NOT NULL DROP TYPE %s\nGO" x.Name x.Name)
         |> String.concat "\n\n")
        + if userDefinedTypeProvided then "\n\n" else System.String.Empty

    let createOrAlterProcedure =
        sprintf "CREATE OR ALTER PROCEDURE %s (%s) AS\nBEGIN\n\nEND" name

    dropProcedure + userDefinedDrop + userDefinedCreate + (stringifiedParams
                                                           |> joinStringsWithCommaSpaceSeparation
                                                           |> createOrAlterProcedure)

let parseClass (input: string) =
    let res = CSharpSyntaxTree.ParseText(input) |> CSharpExtensions.GetCompilationUnitRoot

    try
        res.DescendantNodes()
        |> Enumerable.OfType<ClassDeclarationSyntax>
        |> Seq.exactlyOne
    with
    | :? ArgumentException as x when x.Message =
                                         "The input sequence contains more than one element. (Parameter 'source')" ->
        raise (NotSupportedException("Passing multiple classes is not supported."))
    | :? ArgumentException as x when x.Message = "The input sequence was empty. (Parameter 'source')" ->
        raise (ArgumentException("You must supply a CSharp class."))
    | x -> raise (x)


let generateStoredProcedureFromCSharp (cSharp: string) (settings: Settings): string =
    let ``class`` = parseClass cSharp

    let parameters =
        ``class``.Members
        |> Enumerable.OfType<PropertyDeclarationSyntax>
        |> Seq.map (fun x ->
            { Type = x.Type.ToString() |> SqlDataType.toSqlType
              Name = x.Identifier.Text })
        |> Seq.toList

    match settings.GenerationType with
    | GenerationType.UserDefinedTableType ->
        let f =
            { Parameters = parameters
              Name = sprintf "%sUserDefinedTableType" ``class``.Identifier.Text }
        [ f ] |> List.map ProcedureParameter.UserDefinedTableType
    | GenerationType.None -> parameters |> List.map ProcedureParameter.DataType
    |> formatProcedure ``class``.Identifier.Text
