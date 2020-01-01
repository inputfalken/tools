namespace CSharpGenerator

open JsonParser
open CSharpGenerator.Types
open CSharpGenerator.Arguments
open Common
open System

module private stringValidators =
    let valueExists input =
        input
        |> Option.Some
        |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))
        |> Option.map (fun x -> x.Trim())

type CSharp =
    static member CreateFile input = CSharp.CreateFile(input, Settings())
    static member private UnresolvedBaseType = BaseType.Object |> CSType.BaseType
    static member CreateFile(input, settings) =
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

        let casing =
            settings.Casing
            |> Casing.fromString
            |> Option.defaultValue Casing.Pascal

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

        let createProperties previous current =
            match previous, current with
            | previous, current when previous = current ->
                [| previous |]
            | previous, current when previous.Name = current.Name ->
                match previous, current with
                | previous, current when previous.Type = CSharp.UnresolvedBaseType ->
                    [| tryConvertToNullableValueType current |]
                | previous, current when current.Type = CSharp.UnresolvedBaseType ->
                    [| tryConvertToNullableValueType previous |]
                | previous, _ ->
                    [| { Name = previous.Name
                         Type = CSharp.UnresolvedBaseType |> ArrayType } |]
            | _ -> [| previous; current |]

        let generatedType members =
            { Members = members |> Array.distinct
              NamePrefix = classPrefix
              NameSuffix = classSuffix
              Casing = casing }
            |> CSType.GeneratedType
            |> Option.Some

        let resolveInbalancedProperties biggerType lesserType =
            let fillOut: Property Option [] =
                [| 0 .. (biggerType.Members.Length - lesserType.Members.Length - 1) |]
                |> Array.map (fun _ -> Option.None)
            let values: Property Option [] = (lesserType.Members |> Array.map Option.Some)

            Array.map2
                (fun previous current -> createProperties previous (current |> Option.defaultValue previous))
                biggerType.Members (Array.concat [| values; fillOut |])
            |> Array.collect (fun x -> x)
            |> generatedType

        let analyzeValues previous current =
            match previous, current with
            | GeneratedType previous, GeneratedType current when previous.Members.Length = current.Members.Length ->
                Array.map2 createProperties previous.Members current.Members
                |> Array.collect (fun x -> x)
                |> generatedType
            | GeneratedType previous, GeneratedType current when previous.Members.Length < current.Members.Length ->
                resolveInbalancedProperties current previous
            | GeneratedType previous, GeneratedType current when previous.Members.Length > current.Members.Length ->
                resolveInbalancedProperties previous current
            | BaseType previous, BaseType current ->
                match previous, current with
                | BaseType.ValueType previous, BaseType.ValueType current ->
                    match previous, current with
                    | previous, current when previous = current -> current |> Option.Some
                    | previous, current when current = previous.AsNullable -> current |> Option.Some
                    | previous, current when previous = current.AsNullable -> previous |> Option.Some
                    | _ -> Option.None
                | _ -> Option.None
                |> Option.map BaseType.ValueType
                |> Option.map CSType.BaseType
            | _ -> Option.None

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
            if values.Length = 0 then
                CSharp.UnresolvedBaseType
            else
                values
                |> Array.map baseType
                |> Array.distinct
                |> Array.reduce (fun previous current ->
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
                |> Option.defaultValue CSharp.UnresolvedBaseType
            |> CSType.ArrayType

        and generatedType records =
            records
            |> Array.map (fun x ->
                { Name = x.Key
                  Type =
                      x.Value
                      |> baseType
                      |> Option.defaultValue CSharp.UnresolvedBaseType })
            |> (fun x ->
            { Members = x
              NameSuffix = classSuffix
              NamePrefix = classPrefix
              Casing = casing })
            |> (fun x ->
            if x.Members.Length = 0 then CSharp.UnresolvedBaseType
            else CSType.GeneratedType x)

        let namespaceFormatter =
            settings.NameSpace
            |> stringValidators.valueExists
            |> Option.map (fun x -> sprintf "namespace %s { %s }" x)
            |> Option.defaultValue (sprintf "%s")

        (input, casing)
        ||> Json.parse
        |> baseType
        |> Option.map (fun x ->
            match x with
            | GeneratedType x -> x.ClassDeclaration rootObject
            | ArrayType x -> x.FormatArray rootObject
            | BaseType x -> x.FormatProperty rootObject
            |> namespaceFormatter)
        |> Option.defaultValue String.Empty
