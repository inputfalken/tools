﻿namespace CSharpGenerator

open JsonParser
open CSharpGenerator.Types
open CSharpGenerator.Arguments
open Common.Casing
open Common.StringValidator
open System

type CSharp =
    static member CreateFile input = CSharp.CreateFile(input, Settings())
    static member CreateFile(input, settings) =

        let casing =
            settings.Casing
            |> Casing.fromString
            |> Option.defaultValue Casing.Pascal

        let defaultValues =
            match casing with
            | Pascal ->
                {| Root = "Root"
                   Model = "Model" |}
            | Camel ->
                {| Root = "root"
                   Model = "Model" |}
            | None ->
                {| Root = "root"
                   Model = "model" |}

        let rootObject =
            settings.RootObjectName
            |> valueExists
            |> Option.defaultValue defaultValues.Root

        let (classPrefix, classSuffix) =
            (settings.ClassPrefix
             |> valueExists
             |> Option.map (fun x -> casing.apply x)
             |> Option.defaultValue String.Empty,
             settings.ClassSuffix
             |> valueExists
             |> Option.map (fun x -> casing.apply x)
             |> Option.defaultValue defaultValues.Model)

        let tryConvertToNullableValueType current =
            match current.Type |> Option.defaultValue CSType.UnresolvedBaseType with
            | BaseType x ->
                match x with
                | ValueType x ->
                    { Name = current.Name
                      Type =
                          if x.Nullable then x
                          else x.AsNullable
                          |> BaseType.ValueType
                          |> CSType.BaseType
                          |> Option.Some }
                | _ -> current
            | _ -> current

        let rec createBaseType previous current =
            match previous, current with
            | BaseType previous, BaseType current ->
                match previous, current with
                | BaseType.ValueType previous, BaseType.ValueType current ->
                    match previous, current with
                    | previous, current when previous = current ->
                        current
                        |> BaseType.ValueType
                        |> CSType.BaseType
                    | previous, current when current = previous.AsNullable ->
                        current
                        |> BaseType.ValueType
                        |> CSType.BaseType
                    | previous, current when previous = current.AsNullable ->
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
                tryConvertToNullableValueType current
            | previous, current when current.Type.IsNone || current.Type.Value = CSType.UnresolvedBaseType ->
                tryConvertToNullableValueType previous
            | previous, current ->
                { Name = previous.Name
                  Type = createBaseType previous.Type.Value current.Type.Value |> Option.Some }

        let analyzeValues previous current (parent: CSType Option []) =
            match previous, current with
            | GeneratedType previous, GeneratedType current ->
                let members =
                    Array.concat [ previous.Members; current.Members ]
                    |> Array.groupBy (fun x -> x.Name)
                    |> Array.map (fun (_, grouping) ->
                        match Array.reduce createProperty grouping with
                        | property when grouping.Length = parent.Length -> property
                        | property -> tryConvertToNullableValueType property)

                { Members = members
                  NamePrefix = classPrefix
                  NameSuffix = classSuffix
                  Casing = casing }
                |> CSType.GeneratedType
            | previous, current -> createBaseType previous current

        let rec baseType value =
            match value with
            | DateTime _ ->
                BaseType.DateTime
                |> CSType.BaseType
                |> Option.Some
            | Decimal _ ->
                BaseType.Decimal
                |> CSType.BaseType
                |> Option.Some
            | String _ ->
                BaseType.String
                |> CSType.BaseType
                |> Option.Some
            | Boolean _ ->
                BaseType.Boolean
                |> CSType.BaseType
                |> Option.Some
            | Guid _ ->
                BaseType.Guid
                |> CSType.BaseType
                |> Option.Some
            | Double _ ->
                BaseType.Double
                |> CSType.BaseType
                |> Option.Some
            | Object x ->
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
                    |> Array.distinct

                baseTypes
                |> Array.reduce (fun previous current ->
                    match previous, current with
                    | previous, current when previous = current -> current
                    | (Some previous, Some current) ->
                        analyzeValues previous current baseTypes |> Option.Some
                    | previous, current ->
                        match current |> Option.defaultWith (fun () -> previous.Value) with
                        | CSType.BaseType x ->
                            match x with
                            | BaseType.ValueType x ->
                                x.AsNullable
                                |> BaseType.ValueType
                                |> CSType.BaseType
                                |> Option.Some
                            | _ -> option.None
                        | x -> x |> Option.Some)
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

        let namespaceFormatter =
            settings.NameSpace
            |> valueExists
            |> Option.map (fun x -> sprintf "namespace %s { %s }" x)

        (input, casing)
        ||> Json.parse
        |> baseType
        |> Option.map (fun csType ->
            match csType with
            | GeneratedType x -> x.ClassDeclaration rootObject
            | ArrayType x -> x.FormatArray rootObject
            | BaseType x -> x.FormatProperty rootObject
            |> (fun csharp ->
            namespaceFormatter
            |> Option.map (fun formatter -> formatter csharp)
            |> Option.defaultValue csharp))
        |> Option.defaultValue String.Empty
