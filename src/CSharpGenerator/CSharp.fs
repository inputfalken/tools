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
    static member CreateFile(input: string) =
        CSharp.CreateFile(input, new Settings())

    static member CreateFile(input: string, settings: Settings) =

        let rootObject = settings.RootObjectName
                         |> stringValidators.valueExists
                         |> Option.defaultValue "Root"

        let classPrefixExists = settings.ClassPrefix |> stringValidators.valueExists
        let classSuffixExists = settings.ClassSuffix |> stringValidators.valueExists

        let (classPrefix, classSuffix) = if classSuffixExists.IsNone && classPrefixExists.IsNone then (String.Empty, "Model")
                                         else (classPrefixExists |> Option.defaultValue String.Empty, classSuffixExists |> Option.defaultValue String.Empty)

        let unresolvedBaseType = ReferenceType.Object
                                 |> BaseType.ReferenceType
                                 |> CSType.BaseType

        let analyzeValues (left: CSType) (right: CSType) =
             match left with
             | GeneratedType x1 ->
                 match right with
                 | GeneratedType x2 ->
                     List.map2 (fun left right ->
                         if left = right then left
                         else
                             let hasSameName = left.Name = left.Name
                             if left.Type = unresolvedBaseType && hasSameName then right
                             else if right.Type = unresolvedBaseType && hasSameName then left
                             else
                                 if hasSameName then
                                     { Name = left.Name; Type = unresolvedBaseType |> ArrType }
                                 else
                                     raise (new Exception("Could not generate unresolved type when keys differ."))
                     ) x1.Members x2.Members
                     |> (fun x -> { Members = x; NamePrefix = classPrefix; NameSuffix = classSuffix })
                     |> Option.Some
                 | _ -> Option.None
             | _ -> Option.None
             |> Option.map CSType.GeneratedType

        let rec baseType value: CSType Option =
            match value with
            | DateTime _ -> ValueType.DateTime |> BaseType.ValueType |> CSType.BaseType |> Option.Some
            | Decimal _ -> ValueType.Decimal |> BaseType.ValueType |> CSType.BaseType |> Option.Some
            | String _ -> ReferenceType.String |> BaseType.ReferenceType |> CSType.BaseType |> Option.Some
            | Boolean _ -> ValueType.Boolean |> BaseType.ValueType |> CSType.BaseType |> Option.Some
            | Guid _ -> ValueType.Guid |> BaseType.ValueType |> CSType.BaseType |> Option.Some
            | Double _ -> ValueType.Double |> BaseType.ValueType |> CSType.BaseType |> Option.Some
            | Object x -> generatedType x |> Option.Some
            | Array x -> stringifyArray x |> Option.Some
            | Null -> Option.None

        and stringifyArray (value: Value seq): CSType =
            if Seq.isEmpty value then unresolvedBaseType
            else
                 value
                 |> Seq.map baseType
                 |> Seq.reduce (fun x y ->
                     if x = y then y
                     else if x.IsSome && y.IsSome then analyzeValues x.Value y.Value
                     else if x.IsNone && y.IsNone then option.None
                     else
                         match x |> Option.defaultWith (fun () -> y.Value) with
                         | CSType.BaseType x ->
                             match x with
                             | BaseType.ValueType x -> option.None // TODO make nullable valuetype here.
                             | _ -> option.None
                         | _ -> option.None
                 )
                 |> Option.defaultValue unresolvedBaseType
            |> CSType.ArrType

        and generatedType (records: Record seq): CSType =
            records
            |> Seq.map (fun x -> {
                    Name = x.Key;
                    Type = x.Value
                           |> baseType
                           |> Option.defaultValue unresolvedBaseType
                }
            )
            |> Seq.toList
            |> (fun x -> { Members = x; NameSuffix = classSuffix; NamePrefix = classPrefix })
            |> (fun x -> if x.Members.IsEmpty then unresolvedBaseType else CSType.GeneratedType x)

        let namespaceFormatter = settings.NameSpace
                                 |> stringValidators.valueExists
                                 |> Option.map (fun x -> sprintf "namespace %s { %s }" x)
                                 |> Option.defaultValue (sprintf "%s")

        let data = (input, settings.Casing |> CasingRule.fromString |> Option.defaultValue CasingRule.Pascal) ||> Json.parse
        baseType data
        |> Option.map (fun x ->
            match x with
            | GeneratedType x -> x.ClassDeclaration
            | ArrType x -> x.FormatArray
            | BaseType x -> match x with
                            | ReferenceType x -> x.Info.FormatProperty
                            | ValueType x -> x.Info.FormatProperty
            <| rootObject
            |> namespaceFormatter
        )
        |> Option.defaultValue String.Empty
