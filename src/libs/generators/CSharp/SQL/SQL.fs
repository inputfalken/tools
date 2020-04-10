module TemplateFactory.SQL.SQL


open Common.CaseInsensitiveString
open Common.StringJoin
open System
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

let int = CIString.CI "int"
let int32 = CIString.CI "int32"
let bool = CIString.CI "bool"
let boolean = CIString.CI "boolean"
let guid = CIString.CI "guid"
let string = CIString.CI "string"
let datetTime = CIString.CI "datetime"

type NVarCharArgument =
    | Max
    // We get type safety but negative numbers and 0 could still be passed which is not allowed.
    | Number of int

type SqlDataType =
    | DateTime
    | Int
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
        | x -> raise (NotImplementedException(sprintf "Type %s has not been implemented yet" x.String))

    override x.ToString() =
        match x with
        | DateTime -> "datetime"
        | Int -> "int"
        | Bit -> "bit"
        | UniqueIdentifier -> "uniqueidentifier"
        | Nvarchar x ->
            match x with
            | Max -> "nvarchar(max)"
            | Number x -> sprintf "nvarchar(%d)" x

type ProcedureParameter =
    { Type: SqlDataType
      Name: string }

let formatProcedure name (parameters: ProcedureParameter seq) =
    let joinedParamemters =
        parameters
        |> Seq.map (fun x ->
            let name =
                match x.Name with
                | x when x.[0] = '@' -> x
                | x -> [ "@"; x ] |> joinStrings
            [ name
              x.Type.ToString() ]
            |> joinStringsWithSpaceSeparation)
        |> joinStringsWithCommaSpaceSeparation
    sprintf "CREATE OR ALTER PROCEDURE %s (%s) AS\nBEGIN\n\nEND" <| name <| joinedParamemters

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
         raise(ArgumentException("You must supply a CSharp class.")) 
    | x -> raise (x)

let generateStoredProcedureFromCSharp (cSharp: string) =
    let ``class`` = parseClass cSharp

    let procedure =
        ``class``.Members
        |> Enumerable.OfType<PropertyDeclarationSyntax>
        |> Seq.map (fun x ->
            { Type = x.Type.ToString() |> SqlDataType.toSqlType
              Name = x.Identifier.Text })
        |> formatProcedure ``class``.Identifier.Text

    procedure
