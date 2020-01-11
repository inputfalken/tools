namespace CSharpGenerator

open JsonParser
open CSharpGenerator.Types
open CSharpGenerator.Arguments
open Common
open Common.Casing
open Common.StringValidator

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
            match valueExists settings.ClassPrefix, valueExists settings.ClassSuffix with
            | Some prefix, Some suffix -> (casing.apply prefix, casing.apply suffix)
            | Some prefix, Option.None -> (casing.apply prefix, System.String.Empty)
            | Option.None, Option.Some suffix -> (System.String.Empty, casing.apply suffix)
            | Option.None, Option.None -> (System.String.Empty, defaultValues.Model)

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
