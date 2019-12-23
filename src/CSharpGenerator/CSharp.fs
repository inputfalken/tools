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

    static member CreateFile(input: string, settings: Settings) =

        let rootObject =
            settings.RootObjectName
            |> stringValidators.valueExists
            |> Option.defaultValue "Root"

        let classPrefixExists = settings.ClassPrefix |> stringValidators.valueExists
        let classSuffixExists = settings.ClassSuffix |> stringValidators.valueExists

        let (classPrefix, classSuffix) =
            if classSuffixExists.IsNone && classPrefixExists.IsNone then (String.Empty, "Model")
            else
                (classPrefixExists |> Option.defaultValue String.Empty,
                 classSuffixExists |> Option.defaultValue String.Empty)

        let unresolvedBaseType = BaseType.Object |> CSType.BaseType

        let analyzeValues (current: CSType) (previous: CSType) =
            match current with
            | GeneratedType current ->
                match previous with
                | GeneratedType x2 ->
                    List.map2 (fun current previous ->
                        if current = previous then
                            current
                        else
                            let hasSameName = current.Name = current.Name
                            if current.Type = unresolvedBaseType && hasSameName then
                                match previous.Type with
                                | BaseType x ->
                                    match x with
                                    | ValueType x ->
                                        { Name = previous.Name
                                          Type =
                                              x.AsNullable
                                              |> BaseType.ValueType
                                              |> CSType.BaseType }
                                    | _ -> previous
                                | _ -> previous
                            else if previous.Type = unresolvedBaseType && hasSameName then
                                current
                            else if hasSameName then
                                { Name = current.Name
                                  Type = unresolvedBaseType |> ArrType }
                            else
                                raise (Exception("Could not generate unresolved type when keys differ.")))
                        current.Members x2.Members
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
                            else if current.AsNullable = previous.AsNullable then previous |> Option.Some
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
                |> stringifyArray
                |> Option.Some
            | Null -> Option.None

        and stringifyArray (value: Value seq): CSType =
            if Seq.isEmpty value then
                unresolvedBaseType
            else
                let result =
                    value
                    |> Seq.map baseType
                    |> Seq.reduce (fun previous current ->
                        if current = previous then
                            current
                        else if current.IsSome && previous.IsSome then
                            analyzeValues previous.Value current.Value
                        else
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

                result |> Option.defaultValue unresolvedBaseType
            |> CSType.ArrType

        and generatedType (records: Record seq): CSType =
            records
            |> Seq.map (fun x ->
                { Name = x.Key
                  Type =
                      x.Value
                      |> baseType
                      |> Option.defaultValue unresolvedBaseType })
            |> Seq.toList
            |> (fun x ->
            { Members = x
              NameSuffix = classSuffix
              NamePrefix = classPrefix })
            |> (fun x ->
            if x.Members.IsEmpty then unresolvedBaseType
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
            | ArrType x -> x.FormatArray rootObject
            | BaseType x -> x.FormatProperty rootObject
            |> namespaceFormatter)
        |> Option.defaultValue String.Empty
