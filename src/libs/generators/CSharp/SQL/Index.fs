module TemplateFactory.SQL.Index


open Common
open System
open System.Linq
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

let private mapBaseTypes (``type``: PredefinedTypeSyntax) =
    match ``type``.Keyword.RawKind with
    | 8309 -> "int"
    | 8304 -> "bit"
    | 8316 -> "nvarchar(max)"
    | _ -> raise (NotImplementedException("TODO"))

let foo (procedureName: string) (cSharp: string) =
    let res = CSharpSyntaxTree.ParseText(cSharp) |> CSharpExtensions.GetCompilationUnitRoot

    let ``class`` =
        res.DescendantNodes()
        |> Enumerable.OfType<ClassDeclarationSyntax>
        |> Seq.exactlyOne

    let properties =
        ``class``.Members
        |> Enumerable.OfType<PropertyDeclarationSyntax>
        |> Seq.map (fun x -> x.Type)
        |> Seq.cast<PredefinedTypeSyntax>
        |> Seq.map mapBaseTypes
        |> StringJoin.joinStringsWithCommaSpaceSeparation

    let storedProcedure =
        sprintf "CREATE OR ALTER PROCEDURE %s (%s) AS \nBEGIN\nEND" procedureName properties

    storedProcedure
