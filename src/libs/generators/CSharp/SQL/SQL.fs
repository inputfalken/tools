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

let private int = CIString.CI "int"
let private int32 = CIString.CI "int32"
let private double = CIString.CI "double"
let private float = CIString.CI "float"
let private bool = CIString.CI "bool"
let private boolean = CIString.CI "boolean"
let private guid = CIString.CI "guid"
let private string = CIString.CI "string"
let private datetTime = CIString.CI "datetime"

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

    static member toSqlType str: SqlDataType =
        let str = str |> CIString.CI
        match str with
        | x when x.Equals int || x.Equals int32 -> SqlDataType.Int
        | x when x.Equals bool || x.Equals boolean -> SqlDataType.Bit
        | x when x.Equals guid -> SqlDataType.UniqueIdentifier
        | x when x.Equals datetTime -> SqlDataType.DateTime
        | x when x.Equals string -> SqlDataType.Nvarchar Max
        | x when x.Equals float || x.Equals double -> SqlDataType.Float
        | x -> raise (NotImplementedException(sprintf "Type %s has not been implemented yet" x.String))

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
type UserDefined = {Parameters: Parameter list; Name: string}

type ProcedureParameter =
    | Parameters of Parameter list
    | UserDefinedTableType of UserDefined
    | ProcedureParameter of ProcedureParameter

let rec formatProcedure name (parameter: ProcedureParameter): string =
    let join (parameters: Parameter list) =
        parameters
        |> List.map (fun x ->
            let name =
                match x.Name with
                | x when x.[0] = '@' -> x
                | x -> joinStrings [ "@"; x ]  
            [ name
              x.Type.ToString() ]
            |> joinStringsWithSpaceSeparation)
        |> joinStringsWithCommaSpaceSeparation

    let procedure = sprintf "CREATE OR ALTER PROCEDURE %s (%s) AS\nBEGIN\n\nEND" name
    let userDefinedType =
        sprintf
            "DROP PROCEDURE IF EXISTS %s\nGO\n\nIF type_id('%s') IS NOT NULL DROP TYPE %s\nGO\n\nCREATE TYPE %s AS TABLE (%s)\nGO\n\n" name
    match parameter with
    | Parameters parameters ->
        procedure <| join parameters
    | UserDefinedTableType x ->
        let param = joinStringsWithSpaceSeparation [(joinStrings ["@"; x.Name]) ; x.Name ] 
        userDefinedType x.Name x.Name x.Name (join x.Parameters) + procedure param
    | ProcedureParameter x -> formatProcedure name x

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
        ProcedureParameter.UserDefinedTableType
            { Parameters = parameters;
               Name = sprintf "%sUserDefinedTableType" ``class``.Identifier.Text } 
    | GenerationType.None -> parameters |> ProcedureParameter.Parameters
    |> formatProcedure ``class``.Identifier.Text
