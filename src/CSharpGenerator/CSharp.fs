namespace TemplateFactory
open JsonParser
open CSTypeTemp
open System
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

        let rec baseType value: BaseType =
            match value with
            | DateTime _ -> BaseType.DateTime
            | Decimal _ -> BaseType.Decimal
            | String _ -> BaseType.String
            | Boolean _ -> BaseType.Boolean
            | Guid _ -> BaseType.Guid
            | Double _ -> BaseType.Double
            | _ -> raise (new Exception("Array or object can never be resolved from value."))

        and stringifyArray (value: Value seq): CSType =
            if Seq.isEmpty value then unresolvedBaseType
            else
                 value
                 |> Seq.map (fun x ->
                     match x with
                     | Object x -> generatedType x rootObject |> GeneratedType
                     | x -> baseType x |> BaseType
                 )
                 |> Seq.reduce (fun x y -> if x = y then y else unresolvedBaseType)

        and generatedType (properties: Property seq) (key: string): GeneratedType =
            let key = classPrefix + key + classSuffix
            properties
            |> Seq.map (fun property ->
                match property.Value with
                | Object x ->
                    let generatedType = (generatedType x property.Key)
                    generatedType.FormatClass
                    + " "
                    + generatedType.FormatProperty generatedType.Name
                | Array x ->
                    let ``type`` = stringifyArray x
                    let str = match ``type`` with
                              | GeneratedType x -> x.FormatClass
                              | _ -> String.Empty
                              + " "
                              + ``type``.FormatArray property.Key
                    str
                | x ->
                    let baseType = baseType x
                    let str = baseType.FormatProperty(property.Key)
                    str
            )
            |> Seq.reduce (fun acc curr -> acc + " " + curr)
            |> (fun x -> { Name = key; Members = x })

        let namespaceFormatter = settings.NameSpace
                                 |> stringValidators.valueExists
                                 |> Option.map (fun x -> sprintf "namespace %s { %s }" x)
                                 |> Option.defaultValue (sprintf "%s")

        let error = """
            JSON is built on two structures:
            1: A collection of name/value pairs
            2: An ordered list of values.
        """
        let data = (input, settings.Casing |> CasingRule.fromString |> Option.defaultValue CasingRule.Pascal) ||> Json.parse
        match data with
        | Array x ->
            let ``type`` = stringifyArray x
            match ``type`` with
            | GeneratedType x -> x.FormatClass + " "
            | _ -> String.Empty
            + (``type``).FormatArray "Items"
        | Object x -> (x |> generatedType <| rootObject).FormatClass
        | _ -> raise (new ArgumentException(error))
        |> namespaceFormatter
