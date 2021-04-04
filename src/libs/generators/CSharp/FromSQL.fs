module CSharp.FromSQL

open System
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Sql.Generator
open Languages.SQL
open Common

type GenerationType =
    | UserDefinedTableType
    | None

type Settings = { GenerationType: GenerationType }
// TODO create proper type with support for nullable value types.
let private string = CI "string"
let private int = CI "int"
let private int32 = CI "int32"
let private double = CI "double"
let private float = CI "float"
let private bool = CI "bool"
let private boolean = CI "boolean"
let private guid = CI "guid"
let private dateTime = CI "datetime"
let private intNullable = CI "int?"
let private int32Nullable = CI "int32?"
let private doubleNullable = CI "double?"
let private floatNullable = CI "float?"
let private boolNullable = CI "bool?"
let private booleanNullable = CI "boolean?"
let private guidNullable = CI "guid?"
let private dateTimeNullable = CI "datetime?"

let toSqlType (str: string): DataType =
    let ciString = str.Replace("System.", String.Empty, StringComparison.OrdinalIgnoreCase) |> CI
    match ciString with
    | x when x.Equals int || x.Equals int32 || x.Equals intNullable || x.Equals int32Nullable -> Int
    | x when x.Equals bool || x.Equals boolean || x.Equals boolNullable || x.Equals booleanNullable ->
        Bit
    | x when x.Equals guid || x.Equals guidNullable -> UniqueIdentifier
    | x when x.Equals dateTime || x.Equals dateTimeNullable -> DateTime
    | x when x.Equals float || x.Equals double || x.Equals floatNullable || x.Equals doubleNullable ->
        Float
    | x when x.Equals string -> Nvarchar Max
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
