namespace TemplateFactory
open System.Runtime.InteropServices
open JsonParser
open System

type CSharp =
    static member CreateFile(input : string,  [<Optional>] ?nameSpace) =
        let data = (input, nameSpace) ||> Json.parse
        let rec stringifyObject property : string =
            let (key, value) = property
            let className = key + "Model"
            let stringifiedValue = stringifyValue value

            match value with
            | Value.Object x -> sprintf "public class %s { %s }" className stringifiedValue
            | _ -> sprintf "public %s %s { get; set; }" stringifiedValue key

        and stringifyValue value : string =
            match value with
            | Value.DateTime x -> "System.DateTime"
            | Value.Decimal x -> "decimal"
            | Value.String x -> "string"
            | Value.Boolean x -> "bool"
            | Value.Guid x -> "System.Guid"
            | Value.Double x -> "double"
            | Value.Null -> String.Empty
            | Value.Array x -> x |> Seq.map stringifyValue |> Seq.reduce (fun x y -> x + y)
            | Value.Object x -> x |> Seq.fold (fun acc x -> stringifyObject x) String.Empty

        match nameSpace with
        | Some x -> sprintf "namespace %s {%s}" x (stringifyValue (data))
        | None -> stringifyValue (data)

