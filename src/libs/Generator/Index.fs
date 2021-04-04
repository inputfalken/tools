namespace Generator

open System
open Common.StringValidator
open Common
open Generator
open Languages.CSharp
open Languages.SQL
open Lemonad.ErrorHandling
open Sql.Generator
open Sql.FromCSharp
open CSharp.FromJSON

module Config =
    let transformCSharpSettings (settings: CSharpSettings) =
        let letterRule =
            match valueExists settings.ClassPrefix, valueExists settings.ClassSuffix with
            | Some prefix, Some suffix ->
                (prefix, suffix)
                |> LetterRule.``Prefix and Suffix``
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
                  |> Option.defaultValue Pascal
              ClassCasing =
                  settings.ClassCasing
                  |> Casing.fromString
                  |> Option.defaultValue Pascal }

        csharpSettings

    let transformSqlSettings (settings: SqlSettingsApiModel) =
        match settings with
        | x when x.GenerateUserDefinedTable = true -> { GenerationType = GenerationType.UserDefinedTableType }
        | _ -> { GenerationType = GenerationType.None }


type public Factory =

    static member public CSharpFromJson input =
        Factory.ConfiguredCSharpFromJson
        <| input
        <| CSharpSettings()

    static member public ConfiguredCSharpFromJson (input: String) (settings: CSharpSettings)
                                                  : IResult<String, exn> =
        generate
        <| input
        <| Config.transformCSharpSettings settings

    static member StoredProcedure(arg: SqlProcedureApiModel) =

        let mapSqlDataType (x: SqlProcedureDataTypeParameterApiModel) =
            let sqlDataType =
                match x.DataType with
                | SqlDataApiEnum.Int -> Int
                | SqlDataApiEnum.Bit -> Bit
                | SqlDataApiEnum.UniqueIdentifier -> UniqueIdentifier
                | SqlDataApiEnum.Float -> Float
                | SqlDataApiEnum.NVarchar -> CharArgument.Max |> Nvarchar
                | SqlDataApiEnum.DateTime -> DateTime
                | SqlDataApiEnum.DateTime2 ->
                    DateTime2Argument.Default
                    |> DateTime2
                | SqlDataApiEnum.Varchar -> CharArgument.Max |> Varchar
                | _ -> raise (NotImplementedException(x.ToString()))

            match valueExists x.Name with
            | Some x -> { Name = x; Type = sqlDataType }
            | Option.None -> raise (ArgumentException("Invalid name"))

        match arg with
        | x when (valueExists x.Name).IsNone -> raise (ArgumentException("Invalid name"))
        | x when x.DataTypes = null && x.UserDefinedTypes = null -> formatProcedure x.Name []
        | x when x.DataTypes <> null && x.UserDefinedTypes <> null ->
            let dataTypes =
                x.DataTypes
                |> Seq.map (mapSqlDataType)
                |> Seq.map ProcedureParameter.DataType

            let userDefinedTypes =
                x.UserDefinedTypes
                |> Seq.map (fun x ->
                    { Name = x.Name
                      Parameters =
                          (x.DataTypes
                           |> Seq.map mapSqlDataType
                           |> Seq.toList) })
                |> Seq.map ProcedureParameter.UserDefinedTableType

            let parameters =
                seq {
                    dataTypes
                    userDefinedTypes
                }
                |> Seq.concat
                |> Seq.toList

            formatProcedure x.Name parameters
        | x when x.DataTypes <> null ->
            let dataTypes =
                x.DataTypes
                |> Seq.map (mapSqlDataType)
                |> Seq.map ProcedureParameter.DataType
                |> Seq.toList

            formatProcedure x.Name dataTypes
        | x when x.UserDefinedTypes <> null ->
            let userDefinedTypes =
                x.UserDefinedTypes
                |> Seq.map (fun x ->
                    { Name = x.Name
                      Parameters =
                          (x.DataTypes
                           |> Seq.map mapSqlDataType
                           |> Seq.toList) })
                |> Seq.map ProcedureParameter.UserDefinedTableType
                |> Seq.toList

            formatProcedure x.Name userDefinedTypes
        | _ -> raise (ArgumentException())



    static member public ConfiguredStoredProcedureFromCsharp cSharp settings =
        try
            generateStoredProcedureFromCSharp cSharp (Config.transformSqlSettings (settings))
            |> Result.Value
        with ex -> Result.Error ex

    static member public StoredProcedureFromCsharp cSharp =
        Factory.ConfiguredStoredProcedureFromCsharp cSharp (SqlSettingsApiModel())
