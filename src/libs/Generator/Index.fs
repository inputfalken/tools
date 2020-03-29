namespace Generator

open CSHarp.JSON
open Common
open Common.Casing
open Lemonad.ErrorHandling
open StringValidator

module Config =
    let transformCSharpSettings (settings: CSharpSettings) =
        let letterRule =
            match valueExists settings.ClassPrefix, valueExists settings.ClassSuffix with
            | Some prefix, Some suffix -> (prefix, suffix) |> LetterRule.``Prefix and Suffix``
            | Some prefix, Option.None -> prefix |> LetterRule.Prefix
            | Option.None, Option.Some suffix -> suffix |> LetterRule.Suffix
            | Option.None, Option.None -> "model" |> LetterRule.Suffix  : LetterRule

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

type public Factory =

    static member public CSharp input = Factory.ConfiguredCSharp <| input <| CSharpSettings()
    static member public ConfiguredCSharp (input: System.String) (settings: CSharpSettings): IResult<System.String, exn> =
        CSharp.generateFromJson <| input <| Config.transformCSharpSettings settings
