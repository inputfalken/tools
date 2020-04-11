namespace Generator

open CSHarp.JSON
open Common
open Common.Casing
open Lemonad.ErrorHandling
open StringValidator
open TemplateFactory.SQL
open TemplateFactory.SQL.SQL

module Config =
    let transformCSharpSettings (settings: CSharpSettings) =
        let letterRule =
            match valueExists settings.ClassPrefix, valueExists settings.ClassSuffix with
            | Some prefix, Some suffix -> (prefix, suffix) |> LetterRule.``Prefix and Suffix``
            | Some prefix, Option.None -> prefix |> LetterRule.Prefix
            | Option.None, Option.Some suffix -> suffix |> LetterRule.Suffix
            | Option.None, Option.None -> "model" |> LetterRule.Suffix: LetterRule

        let csharpSettings =
            { RootName =
                  settings.RootObjectName
                  |> valueExists
                  |> Option.defaultValue "root"
              NameSpace = settings.NameSpace |> valueExists
              LetterRule = letterRule
              PropertyCasing =
                  settings.PropertyCasing
                  |> Casing.fromString
                  |> Option.defaultValue Casing.Pascal
              ClassCasing =
                  settings.ClassCasing
                  |> Casing.fromString
                  |> Option.defaultValue Casing.Pascal }

        csharpSettings

    let transformSqlSettings (settings: SqlSettings) =
        match settings with
        | x when x.GenerateUserDefinedTable = true -> { GenerationType = SQL.GenerationType.UserDefinedTableType }
        | _ -> { GenerationType = SQL.GenerationType.None }


type public Factory =

    static member public CSharpFromJson input = Factory.ConfiguredCSharpFromJson <| input <| CSharpSettings()
    static member public ConfiguredCSharpFromJson (input: System.String) (settings: CSharpSettings): IResult<System.String, exn> =
        CSharp.generateFromJson <| input <| Config.transformCSharpSettings settings

    static member public ConfiguredStoredProcedureFromCsharp cSharp settings =
        try
            SQL.generateStoredProcedureFromCSharp cSharp settings |> Lemonad.ErrorHandling.Result.Value
        with ex -> Lemonad.ErrorHandling.Result.Error ex

    static member public StoredProcedureFromCsharp cSharp =
        Factory.ConfiguredStoredProcedureFromCsharp cSharp (Config.transformSqlSettings (SqlSettings()))
