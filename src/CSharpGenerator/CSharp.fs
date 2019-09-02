namespace CSTypeTemp

namespace TemplateFactory
open JsonParser
open CSTypeTemp
open System

module private stringValidators =
    let valueExists input =
       input
       |> Option.Some
       |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))

type CSharp =
    static member CreateFile(input: string) =
        CSharp.CreateFile(input, new Settings())

    static member CreateFile(input: string, settings: Settings) =
        let classPrefix = settings.ClassPrefix
                          |> stringValidators.valueExists
                          |> Option.defaultValue String.Empty
        let classSuffix = settings.ClassSuffix
                          |> stringValidators.valueExists
                          |> Option.defaultValue "Model"
        let rootObject = settings.RootObjectName
                         |> stringValidators.valueExists
                         |> Option.defaultValue "Root"

        let unresolvedBaseType = BaseType.Object |> BaseType

        let rec baseType value: CSType =
            match value with
            | DateTime _ -> BaseType.DateTime |> BaseType
            | Decimal _ -> BaseType.Decimal |> BaseType
            | String _ -> BaseType.String |> BaseType
            | Boolean _ -> BaseType.Boolean |> BaseType
            | Guid _ -> BaseType.Guid |> BaseType
            | Double _ -> BaseType.Double |> BaseType
            | Object x -> generatedType x 
            | Array x -> stringifyArray x
            | x -> raise (new Exception("Unhandled value" + sprintf "'%A'." x))

        and stringifyArray (value: Value seq): CSType =
            if Seq.isEmpty value then unresolvedBaseType |> CSType.ArrType
            else
                 value
                 |> Seq.map baseType
                 |> Seq.reduce (fun x y ->
                     let comparison = match x with
                                       | GeneratedType x1 ->
                                           match y with
                                           | GeneratedType x2 ->
                                               Seq.map2 (fun elem1 elem2 ->
                                                   if elem1 = elem2 then elem1
                                                   else
                                                       let (key, ``type``) = elem1
                                                       match ``type`` with
                                                       | CSType.ArrType _ -> unresolvedBaseType |> ArrType
                                                       | _ -> unresolvedBaseType
                                                       |> (fun x -> (key, x))
                                               ) x1.Members x2.Members
                                               |> (fun x -> {Members =x ; NamePrefix = classPrefix ; NameSuffix = classSuffix})
                                               |> Option.Some
                                           | _ -> Option.None
                                       | _ -> Option.None
                     if comparison.IsSome then comparison.Value |> CSType.GeneratedType
                     else if x = y then y
                     else unresolvedBaseType
                     )
                 |> CSType.ArrType

        and generatedType (properties: Property seq) : CSType =
            properties
            |> Seq.map (fun x -> (x.Key, baseType x.Value))
            |> (fun x -> { Members = x; NameSuffix = classSuffix; NamePrefix = classPrefix })
            |> CSType.GeneratedType

        let namespaceFormatter = settings.NameSpace
                                 |> stringValidators.valueExists
                                 |> Option.map (fun x -> sprintf "namespace %s { %s }" x)
                                 |> Option.defaultValue (sprintf "%s")

        let data = (input, settings.Casing |> CasingRule.fromString |> Option.defaultValue CasingRule.Pascal) ||> Json.parse
        match baseType data with
        | GeneratedType x -> x.ClassDeclaration 
        | ArrType x -> x.FormatArray 
        | BaseType x -> x.FormatProperty
        <| rootObject
        |> namespaceFormatter
