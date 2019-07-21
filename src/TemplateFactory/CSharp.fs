namespace TemplateFactory
open System.Runtime.InteropServices
open JsonParser
open System

type CSharp =
    // With this syntax It's possible for CSharp to also call this function
    static member CreateFile(input : string,  [<Optional>] ?nameSpace) =
        let data = (input, nameSpace) ||> Json.parse
        let rec stringifyObject (property : Property) (useNewline : bool) : string =
            let (key, value) = property
            let className = key + "Model"
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

        match nameSpace with
        | Some x -> sprintf "namespace %s {%s}" x (stringifyValue (data))
        | None -> stringifyValue (data)

