module TemplateFactory.SQL.Index


open Common
open System
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

// Source: https://github.com/dotnet/roslyn/blob/master/src/Compilers/CSharp/Portable/Syntax/SyntaxKind.cs
let private mapBaseTypes (property:  PropertyDeclarationSyntax): String =
    match property.Type.ToString()  with
    | "int" -> "int"
    | "bool" -> "bit"
    | "string" -> "nvarchar(max)"
    | "GUID" -> "uniqueidentifier"
    | "DateTime" -> "datetime"
    | _ -> raise (NotImplementedException("TODO"))
    |> sprintf "@%s %s" property.Identifier.Text

let foo (procedureName: string) (cSharp: string) =
    let res = CSharpSyntaxTree.ParseText(cSharp) |> CSharpExtensions.GetCompilationUnitRoot

    let ``class`` =
        res.DescendantNodes()
        |> Enumerable.OfType<ClassDeclarationSyntax>
        |> Seq.exactlyOne

    let properties =
        ``class``.Members
        |> Enumerable.OfType<PropertyDeclarationSyntax>
        |> Seq.map mapBaseTypes
        |> StringJoin.joinStringsWithCommaSpaceSeparation

    let storedProcedure =
        sprintf "CREATE OR ALTER PROCEDURE %s (%s) AS\nBEGIN\n\nEND" procedureName properties

    storedProcedure
