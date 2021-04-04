module Common

open System
open Microsoft.FSharp.Reflection
open FSharp.Data.Runtime

[<CustomEquality; CustomComparison>]
type CIString =
    | CI of string

    override this.Equals x =
        match x with
        | :? CIString as s -> StringComparer.OrdinalIgnoreCase.Equals(this.String, s.String)
        | :? string as s -> StringComparer.OrdinalIgnoreCase.Equals(this.String, s)
        | x when x = null && this.String = null -> true
        | :? (Option<String>) as s when s.IsSome -> StringComparer.OrdinalIgnoreCase.Equals(this.String, s.Value)
        | :? (Option<Object>) as s when s.IsSome -> StringComparer.OrdinalIgnoreCase.Equals(this.String, s.Value)
        | _ -> false

    member this.String =
        match this with
        | CI x -> x

    override this.GetHashCode() =
        StringComparer.OrdinalIgnoreCase.GetHashCode(this.String)

    interface System.IComparable<CIString> with
        member this.CompareTo x =
            StringComparer.OrdinalIgnoreCase.Compare(this.String, x.String)

    interface IComparable with
        member this.CompareTo x =
            match x with
            | :? CIString as s -> StringComparer.OrdinalIgnoreCase.Compare(this.String, s.String)
            | :? string as s -> StringComparer.OrdinalIgnoreCase.Compare(this.String, s)
            | _ -> -1

module public StringValidator =
    let public valueExists input =
        input
        |> Option.Some
        |> Option.filter (fun x -> not (String.IsNullOrWhiteSpace(x)))
        |> Option.map (fun x -> x.Trim())

module public StringJoin =
    let joinStrings strings: string =
        strings |> String.concat String.Empty

    let joinStringsWithSpaceSeparation strings: string = strings |> String.concat " "

    let joinStringsWithCommaSpaceSeparation strings: string = strings |> String.concat ", "


module private UnionFunctions =
    let toString (x: 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString<'a> s =
        match FSharpType.GetUnionCases typeof<'a>
              |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None

    let fromStringWithUnionCases<'a> s (arr: UnionCaseInfo []) =
        let ciString = s |> CI

        match arr
              |> Array.filter (fun case -> ciString.Equals(case.Name)) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None

type public Casing =
    | Pascal
    | Camel
    | None
    override this.ToString() = UnionFunctions.toString this
    static member private CasesTypes = FSharpType.GetUnionCases typeof<Casing>

    static member public Cases =
        Casing.CasesTypes |> Array.map (fun x -> x.Name)

    static member public PascalCase = Casing.Cases.[0]
    static member public CamelCase = Casing.Cases.[1]
    static member public NoneCase = Casing.Cases.[2]

    static member fromString s =
        UnionFunctions.fromStringWithUnionCases s Casing.CasesTypes

    member this.apply x =
        match this with
        | Pascal -> NameUtils.nicePascalName x
        | Camel -> NameUtils.niceCamelName x
        | None -> x.Replace(" ", String.Empty)

    member this.applyMultiple items =
        let items =
            items
            |> Seq.filter (fun x -> String.IsNullOrWhiteSpace(x) = false)

        match this with
        | None -> items |> StringJoin.joinStrings
        | x ->
            items
            |> StringJoin.joinStringsWithSpaceSeparation
            |> x.apply
