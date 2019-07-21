namespace TemplateFactory
open System.Runtime.InteropServices
open JsonParser
open System

type CSharp =
    // With this syntax It's possible for CSharp to also call this function
    static member CreateFile(input : string,  [<Optional>] ?nameSpace) =
        let data = (input, nameSpace) ||> Json.parse
        let rec stringifyObject property : string =
            let (key, value) = property
            let className = key + "Model"
            let stringifiedValue = stringifyValue value

            match value with
            | Object x -> sprintf "public class %s { %s }" className stringifiedValue
            | _ -> sprintf "public %s %s { get; set; }" stringifiedValue key

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
            | Object x -> x |> Seq.fold (fun acc x -> stringifyObject x) String.Empty

        match nameSpace with
        | Some x -> sprintf "namespace %s {%s}" x (stringifyValue (data))
        | None -> stringifyValue (data)

