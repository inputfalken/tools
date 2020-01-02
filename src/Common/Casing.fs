namespace Common.Casing

open Microsoft.FSharp.Reflection
open FSharp.Data.Runtime

module private UnionFunctions =
    let toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> s  =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None

type public Casing =
    | Pascal
    | Camel
    | None
    override this.ToString() = UnionFunctions.toString this
    static member fromString s = UnionFunctions.fromString<Casing> s
    member this.apply x : string =
        match this with 
        | Pascal -> NameUtils.nicePascalName x
        | Camel -> NameUtils.niceCamelName x
        | None -> x
        

