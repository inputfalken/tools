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

        let casing = match settings.Casing |> CasingRule.fromString with
                     | Option.Some x -> x
                     | Option.None -> Pascal
                     
        let data = (input, casing) ||> Json.parse

        let classSuffix = (settings.ClassSuffix |> stringValidators.valueExists |> Option.defaultValue "Model")
        let classPrefix = (settings.ClassPrefix |> stringValidators.valueExists |> Option.defaultValue String.Empty)
        
        let rec stringifyObject (property : Property) (useNewline : bool) : string =
            let (key, value) = property
            let className = classPrefix + key + classSuffix
            let stringifiedValue = stringifyValue value
            let newLine = (if useNewline then Environment.NewLine else String.Empty)

            match value with
            | Object x -> sprintf "%spublic class %s { %s }" newLine className stringifiedValue
            | _ -> sprintf "%spublic %s %s { get; set; }" newLine stringifiedValue key

        and stringifyValue value : string =
            match value with
            | DateTime x -> "System.DateTime"
            | Decimal x -> "decimal"
            | String x -> "string"
            | Boolean x -> "bool"
            | Guid x -> "System.Guid"
            | Double x -> "double"
            | Null -> String.Empty
            | Array x -> x |> Seq.map stringifyValue |> Seq.reduce (fun x y -> x + y)
            | Object x -> x |> Seq.mapi (fun x y -> (x, y)) |> Seq.fold (fun acc (index, x) -> acc + stringifyObject x (index <> 0)) String.Empty

        let classFormatter = match settings.RootObjectName |> stringValidators.valueExists with
                             | Some x -> sprintf "public class %s%s%s { %s }" classPrefix x classSuffix
                             | None -> sprintf "public class %sRoot%s { %s }" classPrefix classSuffix

        let nameSpaceFormatter = match settings.NameSpace |> stringValidators.valueExists with
                                 | Some x -> sprintf "namespace %s { %s }" x
                                 | None -> sprintf "%s"

        data |> (stringifyValue >> classFormatter >> nameSpaceFormatter)
