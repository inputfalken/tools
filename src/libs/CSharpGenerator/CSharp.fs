namespace CSharpGenerator

open System
open JsonParser
open CSharpGenerator.Types
open CSharpGenerator.Arguments
open Common
open Common.Casing
open Common.StringValidator

type CSharp =
    static member public CreateFile input = CSharp.CreateFile(input, Settings())
    static member public CreateFile(input, settings) =

        let casing =
            settings.Casing
            |> Casing.fromString
            |> Option.defaultValue Casing.Pascal

        let defaultValues =
            {| Root = "root"
               Model = "model" |}

        let rootObject =
            settings.RootObjectName
            |> valueExists
            |> Option.defaultValue defaultValues.Root

        let (classPrefix, classSuffix, rootObject) =
            match valueExists settings.ClassPrefix, valueExists settings.ClassSuffix with
            | Some prefix, Some suffix ->
                match casing with
                | Camel -> (prefix, Pascal.apply suffix, Pascal.apply rootObject)
                | x -> (x.apply prefix, x.apply suffix, x.apply rootObject)
            | Some prefix, Option.None -> (casing.apply prefix, System.String.Empty, casing.apply rootObject)
            | Option.None, Option.Some suffix ->
                match casing with
                | None -> (System.String.Empty, suffix, casing.apply rootObject)
                | _ -> (System.String.Empty, Pascal.apply suffix, casing.apply rootObject)
            | Option.None, Option.None ->
                match casing with
                | None -> (System.String.Empty, defaultValues.Model, casing.apply rootObject)
                | _ -> (System.String.Empty, Pascal.apply defaultValues.Model, casing.apply rootObject)

        let tryConvertToNullableValueType current =
            match current with
            | BaseType x ->
                match x with
                | ValueType x ->
                    x.AsNullable
                    |> BaseType.ValueType
                    |> CSType.BaseType
                | _ -> current
            | _ -> current

        let tryConvertToNullableValueTypeProperty current =
            { Name = current.Name
              Type = Option.map tryConvertToNullableValueType current.Type }

        let rec createBaseType previous current =
            match previous, current with
            | BaseType previous, BaseType current ->
                match previous, current with
                | BaseType.ValueType previous, BaseType.ValueType current ->
                    match previous, current with
                    | previous, current when previous.TypeInfo = current.TypeInfo ->
                        current
                        |> BaseType.ValueType
                        |> CSType.BaseType
                    | previous, current when current.TypeInfo = previous.TypeInfo.AsNullable ->
                        current
                        |> BaseType.ValueType
                        |> CSType.BaseType
                    | previous, current when previous.TypeInfo = current.TypeInfo.AsNullable ->
                        previous
                        |> BaseType.ValueType
                        |> CSType.BaseType
                    | _ -> CSType.UnresolvedBaseType
                | _ -> CSType.UnresolvedBaseType
            | ArrayType previous, ArrayType current -> createBaseType previous current |> CSType.ArrayType
            | _ -> CSType.UnresolvedBaseType

        let createProperty previous current =
            match previous, current with
            | previous, current when previous = current ->
                previous
            | previous, current when previous.Type.IsNone || previous.Type.Value = CSType.UnresolvedBaseType ->
                tryConvertToNullableValueTypeProperty current
            | previous, current when current.Type.IsNone || current.Type.Value = CSType.UnresolvedBaseType ->
                tryConvertToNullableValueTypeProperty previous
            | previous, current ->
                { Name = previous.Name
                  Type = Option.map2 createBaseType previous.Type current.Type }

        let analyzeValues previous current (parent: CSType Option []) =
            match previous, current with
            | GeneratedType previous, GeneratedType current ->
                let members =
                    Array.concat [ previous.Members; current.Members ]
                    |> Array.groupBy (fun x -> x.Name)
                    |> Array.map (fun (_, grouping) ->
                        match Array.reduce createProperty grouping with
                        | property when grouping.Length = parent.Length -> property
                        | property -> tryConvertToNullableValueTypeProperty property)

                { Members = members
                  NamePrefix = classPrefix
                  NameSuffix = classSuffix
                  Casing = casing }
                |> CSType.GeneratedType
            | previous, current -> createBaseType previous current

        let rec baseType =
            function
            | DateTime x ->
                BaseType.DateTime x
                |> CSType.BaseType
                |> Option.Some
            | JsonParser.Decimal x ->
                BaseType.Decimal x
                |> CSType.BaseType
                |> Option.Some
            | JsonParser.String x ->
                BaseType.String x
                |> CSType.BaseType
                |> Option.Some
            | JsonParser.Boolean x ->
                BaseType.Boolean  x
                |> CSType.BaseType
                |> Option.Some
            | JsonParser.Guid x ->
                BaseType.Guid x
                |> CSType.BaseType
                |> Option.Some
            | JsonParser.Double x ->
                BaseType.Double x
                |> CSType.BaseType
                |> Option.Some
            | JsonParser.Object x ->
                x
                |> generatedType
                |> Option.Some
            | Array x ->
                x
                |> arrayType
                |> Option.Some
            | Null -> Option.None

        and arrayType values =
            match values with
            | values when values.Length = 0 -> CSType.UnresolvedBaseType
            | values ->
                let baseTypes =
                    values
                    |> Array.map baseType

                // TODO create a equals which only checks types and do not care about values.
                baseTypes
                |> Array.reduce (fun previous current ->
                    match previous, current with
                    | previous, current when previous = current -> current
                    | (Some previous, Some current) ->
                        analyzeValues previous current baseTypes |> Option.Some
                    | previous, current ->
                        current
                        |> Option.orElse previous
                        |> Option.map tryConvertToNullableValueType)
                |> Option.defaultValue CSType.UnresolvedBaseType
            |> CSType.ArrayType

        and generatedType records =
            match records with
            | records when records.Length = 0 -> CSType.UnresolvedBaseType
            | records ->
                records
                |> Array.map (fun x ->
                    { Name = x.Key
                      Type = baseType x.Value })
                |> (fun x ->
                { Members = x
                  NameSuffix = classSuffix
                  NamePrefix = classPrefix
                  Casing = casing })
                |> CSType.GeneratedType

        try
            let cSharp =
                Json.parse input casing
                |> baseType
                |> Option.defaultValue CSType.UnresolvedBaseType
                |> function
                | GeneratedType x -> x.ClassDeclaration
                | ArrayType x -> x.FormatArray
                | BaseType x -> x.FormatProperty
                <| rootObject

            settings.NameSpace
            |> valueExists
            |> Option.map (fun x -> StringUtils.joinStringsWithSpaceSeparation [ "namespace"; x; "{"; cSharp; "}" ])
            |> Option.defaultValue cSharp
            |> Lemonad.ErrorHandling.Result.Value

        with ex -> ex |> Lemonad.ErrorHandling.Result.Error
