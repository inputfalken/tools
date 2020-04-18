namespace Common.Casing

open Common
open Common.CaseInsensitiveString
open System
open Microsoft.FSharp.Reflection
open FSharp.Data.Runtime

module private UnionFunctions =
    let toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> s =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None

    let fromStringWithUnionCases<'a> s (arr: UnionCaseInfo []) =
        let ciString = s |> CI
        match arr |> Array.filter (fun case -> ciString.Equals(case.Name)) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None

type public Casing =
    | Pascal
    | Camel
    | None
    override this.ToString() = UnionFunctions.toString this
    static member private CasesTypes = FSharpType.GetUnionCases typeof<Casing>
    static member public Cases = Casing.CasesTypes |> Array.map (fun x -> x.Name)
    static member public PascalCase = Casing.Cases.[0]
    static member public CamelCase = Casing.Cases.[1]
    static member public NoneCase = Casing.Cases.[2]
    static member fromString s = UnionFunctions.fromStringWithUnionCases s Casing.CasesTypes

    member this.apply x =
        match this with
        | Pascal -> NameUtils.nicePascalName x
        | Camel -> NameUtils.niceCamelName x
        | None -> x.Replace(" ", String.Empty)

    member this.applyMultiple items =
        let items = items |> Seq.filter (fun x -> String.IsNullOrWhiteSpace(x) = false)
        match this with
        | None -> items |> StringJoin.joinStrings
        | x ->
            items
            |> StringJoin.joinStringsWithSpaceSeparation
            |> x.apply
