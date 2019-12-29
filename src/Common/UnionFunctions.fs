namespace Common

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
    override this.ToString() = UnionFunctions.toString this
    static member fromString s = UnionFunctions.fromString<Casing> s
    static member apply =
        function
        | Pascal -> NameUtils.nicePascalName
        | Camel -> NameUtils.niceCamelName
