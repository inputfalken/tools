module TemplateFactory.SQL.SQL

open System
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Sql.Generator
open Sql.Types
open Common.CaseInsensitiveString

type GenerationType =
    | UserDefinedTableType
    | None

type Settings = { GenerationType: GenerationType }
// TODO create proper type with support for nullable value types.
let private string = CIString.CI "string"
let private int = CIString.CI "int"
let private int32 = CIString.CI "int32"
let private double = CIString.CI "double"
let private float = CIString.CI "float"
let private bool = CIString.CI "bool"
let private boolean = CIString.CI "boolean"
let private guid = CIString.CI "guid"
let private dateTime = CIString.CI "datetime"
let private intNullable = CIString.CI "int?"
let private int32Nullable = CIString.CI "int32?"
let private doubleNullable = CIString.CI "double?"
let private floatNullable = CIString.CI "float?"
let private boolNullable = CIString.CI "bool?"
let private booleanNullable = CIString.CI "boolean?"
let private guidNullable = CIString.CI "guid?"
let private dateTimeNullable = CIString.CI "datetime?"

let toSqlType (str: string): Sql.Types.DataType =
    let ciString = str.Replace("System.", System.String.Empty, StringComparison.OrdinalIgnoreCase) |> CIString.CI
    match ciString with
    | x when x.Equals int || x.Equals int32 || x.Equals intNullable || x.Equals int32Nullable -> Sql.Types.Int
    | x when x.Equals bool || x.Equals boolean || x.Equals boolNullable || x.Equals booleanNullable ->
        Sql.Types.Bit
    | x when x.Equals guid || x.Equals guidNullable -> Sql.Types.UniqueIdentifier
    | x when x.Equals dateTime || x.Equals dateTimeNullable -> Sql.Types.DateTime
    | x when x.Equals float || x.Equals double || x.Equals floatNullable || x.Equals doubleNullable ->
        Sql.Types.Float
    | x when x.Equals string -> Sql.Types.Nvarchar Sql.Types.Max
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
