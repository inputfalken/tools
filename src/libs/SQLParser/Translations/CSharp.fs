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
open TableCreationSQLParser

let toCSharp sql =
    let toCSType sql =
        sql.Columns
        |> List.map
            // TODO use object data to create setters with rules.
            (fun x ->
                let translation =
                    match x.DataType with
                    | Char _ -> BaseType.String String.Empty option.None
                    | Date -> BaseType.DateTime DateTime.MinValue option.None
                    | Bit -> BaseType.Boolean false
                    | DateTime -> BaseType.DateTime DateTime.MinValue  option.None
                    | DateTime2 _ -> BaseType.DateTime DateTime.MinValue option.None
                    | DateTimeOffset _ -> BaseType.DateTime DateTime.MinValue option.None
                    | Int _ -> BaseType.Integer 0 option.None
                    | Decimal _ -> BaseType.Decimal 0m option.None
                    | NChar _ -> BaseType.String String.Empty option.None
                    | Nvarchar x -> BaseType.String String.Empty (x |> Option.bind (fun x -> match x with
                                                                                             | CharSizeArgument.Max -> option.None
                                                                                             | CharSizeArgument.Value x -> option.Some "TEST"))
                    | SmallDateTime -> BaseType.DateTime DateTime.MinValue option.None
                    | Time _ -> BaseType.DateTime DateTime.MinValue option.None
                    | UniqueIdentifier -> BaseType.Guid System.Guid.Empty option.None
                    | Varchar _ -> BaseType.String String.Empty option.None
                    |> CSType.BaseType

                { Name = x.Name; Type = Option.Some translation })
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
