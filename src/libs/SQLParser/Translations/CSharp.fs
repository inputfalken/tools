#if INTERACTIVE
#I @"..\bin\Debug\net5.0"
#r "Languages"
#r "Common"
#r "nuget: FParsec"
#r "nuget: FSharp.Data"
#r "SQLParser"
#else
module CSharpParser.Translations.CSharp
#endif

open System
open Common
open Languages.CSharp
open Languages.CSharp.Factory
open SQLParser

let toCSharp sql =
    let toCSType sql =
        sql.Columns
        |> List.map
            // TODO use object data to create setters with rules.
            (fun x ->
                let f =
                    match x.DataType with
                    | Char _ -> BaseType.String String.Empty
                    | Date -> BaseType.DateTime DateTime.MinValue
                    | Bit -> BaseType.Boolean false
                    | DateTime -> BaseType.DateTime DateTime.MinValue
                    | DateTime2 _ -> BaseType.DateTime DateTime.MinValue
                    | DateTimeOffset _ -> BaseType.DateTime DateTime.MinValue
                    | Int _ -> BaseType.Integer 0
                    | NChar _ -> BaseType.String String.Empty
                    | Nvarchar _ -> BaseType.String String.Empty
                    | SmallDateTime -> BaseType.DateTime DateTime.MinValue
                    | Time _ -> BaseType.DateTime DateTime.MinValue
                    | UniqueIdentifier -> BaseType.Guid System.Guid.Empty
                    | Varchar _ -> BaseType.String String.Empty
                    |> CSType.BaseType

                { Name = x.Name; Type = Option.Some f })
        |> List.toArray
        |> CSType.GeneratedType
        |> (fun x ->
            {| CSType = x
               ClassName = sql.TableName
               Schema = sql.Schema |})


    parse sql
    |> Result.map toCSType
    |> Result.map
        (fun x ->
            ToCSharpString
                x.CSType
                { RootName = x.ClassName
                  NameSpace = Option.None
                  ClassCasing = Casing.None
                  PropertyCasing = Casing.Pascal
                  LetterRule = LetterRule.Prefix (x.Schema |> Option.map (fun x -> $"{x}_") |> Option.defaultValue String.Empty) })
