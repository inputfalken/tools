namespace TemplateFactory
open JsonParser
open System

module private stringValidators =
    let valueExists input =
        let optional = input
                       |> Option.Some
                       |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))
        optional

type Settings() =
        member val Casing = "" with get, set
        member val NameSpace = "" with get, set
        member val ClassPrefix = "" with get, set
        member val ClassSuffix = "" with get, set
        member val RootObjectName = "" with get, set

type CSharp =
    static member CreateFile(input : string) =
        CSharp.CreateFile(input, new Settings())

    static member CreateFile(input : string, settings : Settings) =
        let data = (input, settings.Casing |> CasingRule.fromString |> Option.defaultValue CasingRule.Pascal) ||> Json.parse
        let classPrefix = settings.ClassPrefix
                          |> stringValidators.valueExists
                          |> Option.defaultValue String.Empty
        let classSuffix = settings.ClassSuffix
                          |> stringValidators.valueExists
                          |> Option.defaultValue "Model"
        let rootObject = settings.RootObjectName
                         |> stringValidators.valueExists
                         |> Option.defaultValue "Root"
                         |> (fun x -> sprintf "%s%s%s" classPrefix x classSuffix)


        let rec stringifyValue value : string =
            match value with
            | DateTime x -> "System.DateTime"
            | Decimal x -> "decimal"
            | String x -> "string"
            | Boolean x -> "bool"
            | Guid x -> "System.Guid"
            | Double x -> "double"
            | Null -> String.Empty
            | Array x -> stringifyArray x
            | Object x -> x |> Seq.mapi (fun x y -> (x, y)) |> Seq.fold (fun acc (index, x) -> acc + stringifyObject x (index <> 0)) String.Empty

        and stringifyArray (value : Value seq) =
            value
            |> Seq.take 1
            |> Seq.toList
            |> Option.Some
            |> Option.filter (fun x -> not x.IsEmpty)
            |> Option.map (fun x -> x.[0])
            |> Option.map stringifyValue
            |> Option.defaultValue "object"
            |> (fun x -> sprintf "public %s[] %s { get; set; }" x rootObject )

        and stringifyObject (property : Property) (useSpace : bool) : string =
            let (key, value) = property
            let className = classPrefix + key + classSuffix
            let stringifiedValue = stringifyValue value
            let space = (if useSpace then "" else String.Empty)

            match value with
            | Object x -> sprintf "%spublic class %s { %s }" space className stringifiedValue
            | Array x -> ""
            | _ -> sprintf "%spublic %s %s { get; set; }" space stringifiedValue key

        let namespaceFormatter = settings.NameSpace
                                 |> stringValidators.valueExists
                                 |> Option.map (fun x -> sprintf "namespace %s { %s }" x)
                                 |> Option.defaultValue (sprintf "%s")


        let classFormatter = sprintf "public class %s { %s }" rootObject 

        data |> (stringifyValue >> classFormatter >> namespaceFormatter)
