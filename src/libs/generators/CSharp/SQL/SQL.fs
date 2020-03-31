module TemplateFactory.SQL.SQL


open Common.StringJoin
open System
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

type ProcedureParameter =
    { Type: String
      Name: string }

// Source: https://github.com/dotnet/roslyn/blob/master/src/Compilers/CSharp/Portable/Syntax/SyntaxKind.cs
// TODO create a proper type for similar to BaseType
let mapBaseTypes (dataType: string): String =
    match dataType with
    | "int" -> "int"
    | "bool" -> "bit"
    | "string" -> "nvarchar(max)"
    | "GUID" -> "uniqueidentifier"
    | "DateTime" -> "datetime"
    | _ -> raise (NotImplementedException("TODO"))


let formatProcedure name (parameters: ProcedureParameter seq) =
    sprintf "CREATE OR ALTER PROCEDURE %s (%s) AS\nBEGIN\n\nEND" <| name
    <| (parameters
        |> Seq.map (fun x ->
            let name =
                match x.Name with
                | x when x.[0] = '@' -> x
                | x -> [ "@"; x ] |> joinStrings
            [ name; x.Type ]
            |> joinStringsWithSpaceSeparation
        )
        |> joinStringsWithCommaSpaceSeparation)

let generateStoredProcedureFromCSharp  (cSharp: string) =
    let res = CSharpSyntaxTree.ParseText(cSharp) |> CSharpExtensions.GetCompilationUnitRoot

    let ``class`` =
        res.DescendantNodes()
        |> Enumerable.OfType<ClassDeclarationSyntax>
        |> Seq.exactlyOne

    let procedure =
        ``class``.Members
        |> Enumerable.OfType<PropertyDeclarationSyntax>
        |> Seq.map (fun x ->
            { Type = mapBaseTypes <| x.Type.ToString()
              Name = x.Identifier.Text })
        |> formatProcedure ``class``.Identifier.Text

    procedure
