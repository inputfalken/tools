module Common.CaseInsensitiveString
open System

[<CustomEquality; CustomComparison>]
type CIString =
    | CI of string
    override this.Equals x =
        match x with
        | :? CIString as s -> StringComparer.OrdinalIgnoreCase.Compare(this.string, s.string) = 0
        | :? string as s -> StringComparer.OrdinalIgnoreCase.Compare(this.string, s) = 0
        | _ -> false
    member this.string =
        match this with
        | CI x -> x
    override this.GetHashCode() =
        StringComparer.OrdinalIgnoreCase.GetHashCode(this.string)
        
    interface System.IComparable<CIString> with
            member this.CompareTo x = StringComparer.OrdinalIgnoreCase.Compare(this.string, x.string)
    interface System.IComparable with
            member this.CompareTo x =
                match x with 
                | :? CIString as s -> StringComparer.OrdinalIgnoreCase.Compare(this.string, s.string) 
                | :? string as s -> StringComparer.OrdinalIgnoreCase.Compare(this.string, s) 
                | _ -> -1
