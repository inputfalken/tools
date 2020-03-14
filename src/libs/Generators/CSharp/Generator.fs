namespace CSharp

open JsonParser
open CSharp.Types
open Common
open Common.CaseInsensitiveString
open Common.StringValidator
open CSharp.Factory.CSharpFactory

type CSharp =
    static member public CreateFile(input, (settings: Types.Settings), (root: System.String), (nameSpace: System.String)) =

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
                    | _ -> UnresolvedBaseType
                | _ -> UnresolvedBaseType
            | ArrayType previous, ArrayType current ->
                createBaseType previous current parentLength |> CSType.ArrayType
            | GeneratedType previous, GeneratedType current ->
                let members =
                    Array.concat [ previous; current ]
                    |> Array.groupBy (fun x -> x.Name |> CI)
                    |> Array.map (fun (_, grouping) ->
                        match Array.reduce (fun x y -> createProperty x y parentLength) grouping with
                        | property when grouping.Length = parentLength -> property
                        | property -> tryConvertToNullableValueTypeProperty property)
                members |> CSType.GeneratedType
            | previous, GeneratedType current when previous = UnresolvedBaseType ->
                current |> CSType.GeneratedType
            | GeneratedType previous, current when current = UnresolvedBaseType ->
                previous |> CSType.GeneratedType
            | _ -> UnresolvedBaseType

        and createProperty previous current parentLength =
            match previous, current with
            | previous, current when previous = current ->
                previous
            | previous, current when previous.Type.IsNone || previous.Type.Value = UnresolvedBaseType ->
                tryConvertToNullableValueTypeProperty current
            | previous, current when current.Type.IsNone || current.Type.Value = UnresolvedBaseType ->
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
                | records when records.Length = 0 -> UnresolvedBaseType
                | records ->
                    records
                    |> Array.map (fun x ->
                        { Name = x.Key
                          Type = baseType x.Value })
                    |> CSType.GeneratedType
                |> Option.Some
            | Array values ->
                match values with
                | values when values.Length = 0 -> UnresolvedBaseType
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
                    |> Option.defaultValue UnresolvedBaseType
                |> CSType.ArrayType
                |> Option.Some
            | Null -> Option.None



        try
            let cSharp =
                Json.parse input
                |> baseType
                |> Option.defaultValue UnresolvedBaseType
                |> CSharpFactory
                <| root
                <| settings

            nameSpace
            |> valueExists
            |> Option.map (fun x -> StringUtils.joinStringsWithSpaceSeparation [ "namespace"; x; "{"; cSharp; "}" ])
            |> Option.defaultValue cSharp
            |> Lemonad.ErrorHandling.Result.Value

        with ex -> ex |> Lemonad.ErrorHandling.Result.Error
