namespace TemplateFactory
open System.Runtime.InteropServices
open JsonParser
open System

type Settings = {
    Casing : CasingRule Option
    NameSpace : String Option
 }

type CSharp =
    static member CreateFile(input : string, ( [<Optional>] ?settings : Settings)) =
        // Pretty ugly solution
        // This neeeds to be solved differently,
        // arguments should be optional without being this complex.
        let settings = match settings with
                       | Option.Some x -> x
                       | Option.None -> {
                           Casing = Option.None
                           NameSpace = Option.None
                       }
                       
        let casing = match settings.Casing with
                     | Option.Some x -> x
                     | Option.None -> Pascal
        // Pretty ugly solution ends

        let data = (input, casing) ||> Json.parse

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

        match settings.NameSpace with
        | Some x -> sprintf "namespace %s {%s}" x (stringifyValue (data))
        | None -> stringifyValue (data)

