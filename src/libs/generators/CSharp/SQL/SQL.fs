module TemplateFactory.SQL.SQL

open System
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Sql.Index
open Common.CaseInsensitiveString

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

let toSqlType (str: string): SqlDataType =
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
            { Type = x.Type.ToString() |> toSqlType
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
