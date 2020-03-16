namespace Generator

open CSharp.CSharp
open CSharp.Types
open Common
open Common.Casing
open StringValidator

type public Factory =
    static member public CSharp input = Factory.CSharp(input, CSharpSettings())
    static member public CSharp (input, settings) =
        
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
            { ClassPrefix = classPrefix
              ClassSuffix = classSuffix
              PropertyCasing =
                  settings.PropertyCasing
                  |> Casing.fromString
                  |> Option.defaultValue Casing.Pascal
              ClassCasing =
                  settings.ClassCasing
                  |> Casing.fromString
                  |> Option.defaultValue Casing.Pascal } 
            
        generateCSharpFromJson input csharpSettings root settings.NameSpace
