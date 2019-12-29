namespace CSharpGenerator

open JsonParser
open CSharpGenerator.Types
open CSharpGenerator.Arguments
open System

module private stringValidators =
    let valueExists input =
        input
        |> Option.Some
        |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))
        |> Option.map (fun x -> x.Trim())

type CSharp =
    static member CreateFile(input: string) = CSharp.CreateFile(input, Settings())
    static member private UnresolvedBaseType = BaseType.Object |> CSType.BaseType
    static member CreateFile(input: string, settings: Settings) =
        let rootObject =
            settings.RootObjectName
            |> stringValidators.valueExists
            |> Option.defaultValue "Root"

        let (classPrefix, classSuffix) =
            (settings.ClassPrefix
             |> stringValidators.valueExists
             |> Option.defaultValue String.Empty,
             settings.ClassSuffix
             |> stringValidators.valueExists
             |> Option.defaultValue "Model")

        let tryConvertToNullableValueType current =
            match current.Type with
            | BaseType x ->
                match x with
                | ValueType x ->
                    { Name = current.Name
                      Type =
                          x.AsNullable
                          |> BaseType.ValueType
                          |> CSType.BaseType }
                | _ -> current
            | _ -> current

        let analyzeValues (previous: CSType) (current: CSType) =
            match current with
            | GeneratedType current ->
                match previous with
                | GeneratedType previous ->
                    List.map2 (fun previous current ->
                        match (previous: Property), (current: Property) with
                        | previous, current when previous = current -> previous
                        | previous, current when previous.Name = current.Name ->
                            match previous, current with
                            | previous, current when previous.Type = CSharp.UnresolvedBaseType ->
                                tryConvertToNullableValueType current
                            | previous, current when current.Type = CSharp.UnresolvedBaseType ->
                                tryConvertToNullableValueType previous
                            | previous, _ ->
                                { Name = previous.Name
                                  Type = CSharp.UnresolvedBaseType |> ArrayType }
                        | _ -> raise (Exception("Could not generate unresolved type when keys differ.")))
                        previous.Members current.Members
                    |> (fun x ->
                    { Members = x
                      NamePrefix = classPrefix
                      NameSuffix = classSuffix })
                    |> Option.Some
                | _ -> Option.None
                |> Option.map CSType.GeneratedType
            | BaseType current ->
                match previous with
                | BaseType previous ->
                    match current with
                    | BaseType.ValueType current ->
                        match previous with
                        | BaseType.ValueType previous ->
                            if current = previous then current |> Option.Some
                            else if current = previous.AsNullable then current |> Option.Some
                            else if previous = current.AsNullable then previous |> Option.Some
                            else Option.None
                        | _ -> Option.None
                    | _ -> Option.None
                | _ -> Option.None
                |> Option.map BaseType.ValueType
                |> Option.map CSType.BaseType
            | _ -> Option.None

        let rec baseType value: CSType Option =
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

        and arrayType (value: Value seq): CSType =
            if Seq.isEmpty value then
                CSharp.UnresolvedBaseType
            else
                let result =
                    value
                    |> Seq.map baseType
                    |> Seq.reduce (fun previous current ->
                        match previous, current with
                        | previous, current when previous = current -> current
                        | (Some previous, Some current) ->
                            analyzeValues previous current
                            |> Option.defaultValue CSharp.UnresolvedBaseType
                            |> Option.Some
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

                result |> Option.defaultValue CSharp.UnresolvedBaseType
            |> CSType.ArrayType

        and generatedType (records: Record seq): CSType =
            records
            |> Seq.map (fun x ->
                { Name = x.Key
                  Type =
                      x.Value
                      |> baseType
                      |> Option.defaultValue CSharp.UnresolvedBaseType })
            |> Seq.toList
            |> (fun x ->
            { Members = x
              NameSuffix = classSuffix
              NamePrefix = classPrefix })
            |> (fun x ->
            if x.Members.IsEmpty then CSharp.UnresolvedBaseType
            else CSType.GeneratedType x)

        let namespaceFormatter =
            settings.NameSpace
            |> stringValidators.valueExists
            |> Option.map (fun x -> sprintf "namespace %s { %s }" x)
            |> Option.defaultValue (sprintf "%s")

        (input,
         settings.Casing
         |> CasingRule.fromString
         |> Option.defaultValue CasingRule.Pascal)
        ||> Json.parse
        |> baseType
        |> Option.map (fun x ->
            match x with
            | GeneratedType x -> x.ClassDeclaration rootObject
            | ArrayType x -> x.FormatArray rootObject
            | BaseType x -> x.FormatProperty rootObject
            |> namespaceFormatter)
        |> Option.defaultValue String.Empty
