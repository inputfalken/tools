namespace TemplateFactory
open FSharp.Data
open System
open FSharp.Data.Runtime


module public CSharp =
    open System

    type public Key = string
    type public NameSpace = string
    type public Value =
                | Double of double
                | Decimal of decimal
                | String of string
                | DateTime of DateTime
                | Boolean of Boolean
                | Array of Value seq
                | Guid of Guid
                | Null
                | Object of Property seq
    and public Property = Key * Value
    type public File = {
        NameSpace : NameSpace
        Data : Value
    }

    let private stringParser =
         function
            | TryParse.Date x -> Value.DateTime x
            | TryParse.Guid x -> Value.Guid x
            | x -> Value.String x

    // JSON is built on two structures:
    // 1: A collection of name/value pairs
    // 2: An ordered list of values.
    let rec private map (value : JsonValue) =
         match value with
         | JsonValue.Number x -> Value.Decimal(x)
         | JsonValue.Float x -> Value.Decimal(decimal x)
         | JsonValue.String x -> stringParser x
         | JsonValue.Boolean x -> Value.Boolean(x)
         | JsonValue.Null -> Value.Null
         | JsonValue.Array x -> x |> Seq.map (fun x -> map x) |> Value.Array
         | JsonValue.Record x -> x |> Seq.map (fun x -> x ||> propertyMap) |> Value.Object
    and private propertyMap (key : string) (value : JsonValue) = (key, map value)

    let public ParseJson input =
        {
            NameSpace = "TODO"
            Data = input |> JsonValue.Parse |> map
        }

    let public CreateFile(file : File) : string =
        let rec processObject property =
            let (key, value) = property
            let typeKey = key |> NameUtils.nicePascalName
            let className = typeKey + "Model"

            // If value is not an object; then we need to to create getters and setters!
            let isObject = match value with
                           | Value.Object x -> true
                           | _ -> false
            let value = processValue value
            if isObject then sprintf "public class %s { %s }" className value
            else sprintf "public %s %s { get; set; }" value typeKey

        and processValue value : string =
            // There's sadly no nameof operator available, don't want to use reflection to get types.
            // Might as well hard code it...
            let foo = match value with
                      | DateTime x -> "System.DateTime"
                      | Decimal x -> "decimal"
                      | String x -> "string"
                      | Boolean x -> "bool"
                      | Guid x -> "System.Guid"
                      | Double x -> "double"
                      | Null -> String.Empty
                      | Array x -> x |> Seq.map processValue |> Seq.reduce (fun x y -> x + y)
                      | Object x -> x |> Seq.fold (fun acc x -> processObject x) String.Empty

            foo

        sprintf "namespace %s {%s}" file.NameSpace (processValue (file.Data))
