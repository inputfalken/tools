namespace Generator

open CSharp.CSharp
open CSharp.Types
open Common
open Common.Casing
open Lemonad.ErrorHandling
open StringValidator

module Config =
    let transformCSharpSettings (settings: CSharpSettings) =
        let (classPrefix, classSuffix) =
            match valueExists settings.ClassPrefix, valueExists settings.ClassSuffix with
            | Some prefix, Some suffix -> (prefix, suffix)
            | Some prefix, Option.None -> (prefix, System.String.Empty)
            | Option.None, Option.Some suffix -> (System.String.Empty, suffix)
            | Option.None, Option.None -> (System.String.Empty, "model")

        let csharpSettings =
            { RootName =
                  settings.RootObjectName
                  |> valueExists
                  |> Option.defaultValue "root"
              NameSpace = settings.NameSpace |> valueExists
              ClassPrefix = classPrefix
              ClassSuffix = classSuffix
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
        generateCSharpFromJson <| input <| Config.transformCSharpSettings settings
