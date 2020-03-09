module Common.CaseInsensitiveString
open System

[<CustomEquality; CustomComparison>]
type CIString =
    | CI of string
    override this.Equals x =
        match x with
        | :? CIString as s -> StringComparer.OrdinalIgnoreCase.Equals(this.String, s.String) 
        | :? string as s -> StringComparer.OrdinalIgnoreCase.Equals(this.String, s)
        | x when x = null && this.String = null -> true
        | :? Option<String> as s when s.IsSome -> StringComparer.OrdinalIgnoreCase.Equals(this.String, s.Value)
        | :? Option<Object> as s when s.IsSome -> StringComparer.OrdinalIgnoreCase.Equals(this.String, s.Value)
        | _ -> false
    member this.String =
        match this with
        | CI x -> x
    override this.GetHashCode() =
        StringComparer.OrdinalIgnoreCase.GetHashCode(this.String)
        
    interface System.IComparable<CIString> with
            member this.CompareTo x = StringComparer.OrdinalIgnoreCase.Compare(this.String, x.String)
    interface System.IComparable with
            member this.CompareTo x =
                match x with 
                | :? CIString as s -> StringComparer.OrdinalIgnoreCase.Compare(this.String, s.String) 
                | :? string as s -> StringComparer.OrdinalIgnoreCase.Compare(this.String, s) 
                | _ -> -1
