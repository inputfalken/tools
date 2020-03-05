namespace CSharpGenerator

open JsonParser
open CSharpGenerator.Types
open CSharpGenerator.Arguments
open Common
open Common.Casing
open Common.StringValidator

type CSharp =
    static member public CreateFile input = CSharp.CreateFile(input, Settings())
    static member public CreateFile(input, settings) =

        let propertyCasing =
            settings.PropertyCasing
            |> Casing.fromString
            |> Option.defaultValue Casing.Pascal

        let typeCasing =
            settings.TypeCasing
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
                match typeCasing with
                | Camel -> (prefix, Pascal.apply suffix, Pascal.apply rootObject)
                | x -> (x.apply prefix, x.apply suffix, x.apply rootObject)
            | Some prefix, Option.None -> (typeCasing.apply prefix, System.String.Empty, typeCasing.apply rootObject)
            | Option.None, Option.Some suffix ->
                match typeCasing with
                | None -> (System.String.Empty, suffix, typeCasing.apply rootObject)
                | _ -> (System.String.Empty, Pascal.apply suffix, typeCasing.apply rootObject)
            | Option.None, Option.None ->
                match typeCasing with
                | None -> (System.String.Empty, defaultValues.Model, typeCasing.apply rootObject)
                | _ -> (System.String.Empty, Pascal.apply defaultValues.Model, typeCasing.apply rootObject)

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

        let rec createBaseType previous current parentLength =
            match previous, current with
            | BaseType previous, BaseType current ->
                match previous, current with
                | previous, current when previous.TypeInfo = current.TypeInfo -> current |> CSType.BaseType
                | BaseType.ValueType previous, BaseType.ValueType current ->
                    match previous, current with
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
            | ArrayType previous, ArrayType current ->
                createBaseType previous current parentLength |> CSType.ArrayType
            | GeneratedType previous, GeneratedType current ->
                let members =
                    Array.concat [ previous.Members; current.Members ]
                    |> Array.groupBy (fun x -> x.Name.ToLowerInvariant())
                    |> Array.map (fun (_, grouping) ->
                        match Array.reduce (fun x y -> createProperty x y parentLength) grouping with
                        | property when grouping.Length = parentLength -> property
                        | property -> tryConvertToNullableValueTypeProperty property)

                { Members = members
                  NamePrefix = classPrefix
                  NameSuffix = classSuffix
                  PropertyCasing = propertyCasing
                  TypeCasing = typeCasing }
                |> CSType.GeneratedType
            | previous, GeneratedType current when previous = CSType.UnresolvedBaseType ->
                current |> CSType.GeneratedType
            | GeneratedType previous, current when current = CSType.UnresolvedBaseType ->
                previous |> CSType.GeneratedType
            | _ -> CSType.UnresolvedBaseType

        and createProperty previous current parentLength =
            match previous, current with
            | previous, current when previous = current ->
                previous
            | previous, current when previous.Type.IsNone || previous.Type.Value = CSType.UnresolvedBaseType ->
                tryConvertToNullableValueTypeProperty current
            | previous, current when current.Type.IsNone || current.Type.Value = CSType.UnresolvedBaseType ->
                tryConvertToNullableValueTypeProperty previous
            | previous, current ->
                { Name = previous.Name
                  Type = Option.map2 (fun x y -> createBaseType x y parentLength) previous.Type current.Type }

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
                BaseType.Boolean x
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
            | JsonParser.Object records ->
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
                      PropertyCasing = propertyCasing
                      TypeCasing = typeCasing
                      })
                    |> CSType.GeneratedType
                |> Option.Some
            | Array values ->
                match values with
                | values when values.Length = 0 -> CSType.UnresolvedBaseType
                | values ->
                    let baseTypes = values |> Array.map baseType
                    baseTypes
                    |> Array.reduce (fun previous current ->
                        match previous, current with
                        | previous, current when previous = current -> current
                        | (Some previous, Some current) ->
                            createBaseType previous current baseTypes.Length |> Option.Some
                        | previous, current ->
                            current
                            |> Option.orElse previous
                            |> Option.map tryConvertToNullableValueType)
                    |> Option.defaultValue CSType.UnresolvedBaseType
                |> CSType.ArrayType
                |> Option.Some
            | Null -> Option.None

        try
            let cSharp =
                Json.parse input 
                |> baseType
                |> Option.defaultValue CSType.UnresolvedBaseType
                |> function
                | GeneratedType x -> x.ClassDeclaration
                | ArrayType x -> fun y -> x.FormatArray y true
                // TODO apply property casing
                | BaseType x -> x.FormatProperty
                <| rootObject

            settings.NameSpace
            |> valueExists
            |> Option.map (fun x -> StringUtils.joinStringsWithSpaceSeparation [ "namespace"; x; "{"; cSharp; "}" ])
            |> Option.defaultValue cSharp
            |> Lemonad.ErrorHandling.Result.Value

        with ex -> ex |> Lemonad.ErrorHandling.Result.Error
