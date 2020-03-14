namespace Generator

open CSharp
open Common
open StringValidator
open Common.Casing

type public Factory =
    static member public CSharp input = Factory.CSharp(input, CSharpSettings())
    static member public CSharp(input, (settings : CSharpSettings)) =
        let (classPrefix, classSuffix) =
            match valueExists settings.ClassPrefix, valueExists settings.ClassSuffix with
            | Some prefix, Some suffix -> (prefix, suffix)
            | Some prefix, Option.None -> (prefix, System.String.Empty)
            | Option.None, Option.Some suffix -> (System.String.Empty, suffix)
            | Option.None, Option.None -> (System.String.Empty, "model")

        let root = settings.RootObjectName
                |> valueExists
                |> Option.defaultValue "root"
                
        let csharpSettings =
            { Prefix = classPrefix
              Suffix = classSuffix
              PropertyCasing =
                  settings.PropertyCasing
                  |> Casing.fromString
                  |> Option.defaultValue Casing.Pascal
              TypeCasing =
                  settings.TypeCasing
                  |> Casing.fromString
                  |> Option.defaultValue Casing.Pascal } : Types.Settings
            
        CSharp.CreateFile(input, csharpSettings, root, settings.NameSpace)
