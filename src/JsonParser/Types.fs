namespace JsonParser
open System
open Microsoft.FSharp.Reflection


module private UnionFunctions =
    let toString (x : 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> (s : string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None


type public Key = string
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
and public Property = { Key : Key ; Value : Value }


type public CasingRule =
    | Pascal
    | Camel
    override this.ToString() = UnionFunctions.toString this static member fromString s = UnionFunctions.fromString<CasingRule> s
