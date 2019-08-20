namespace CSTypeTemp

    module private Formatters =
        let ``class`` name content =
            sprintf "public class %s { %s }" name content

        let property ``type`` name: string =
            sprintf "public %s %s { get; set; }" ``type`` name

        let arrayProperty ``type`` name =
            property (sprintf "%s[]" ``type``) name

    type BaseType = {
        Name: string
        Namespace: string
        Alias: string option
     } with
        static member Object = { Name = "Object"; Namespace = "System"; Alias = option.Some "object" }
        static member DateTime = { Namespace = "System"; Name = "DateTime"; Alias = option.None }
        static member Decimal = { Namespace = "System"; Name = "Decimal"; Alias = Option.Some "decimal" }
        static member String = { Namespace = "System"; Name = "String"; Alias = option.Some "string" }
        static member Boolean = { Namespace = "System"; Name = "Boolean"; Alias = option.Some "bool" }
        static member Guid = { Namespace = "System"; Name = "Guid"; Alias = option.None }
        static member Double = { Namespace = "System"; Name = "Double"; Alias = option.Some "double" }
        member this.FormatArray key = Formatters.arrayProperty (this.ToString()) key
        member this.FormatProperty key = Formatters.property (this.ToString()) key
        override this.ToString() = this.Alias |> Option.defaultValue (this.Namespace + "." + this.Name)

    type GeneratedType = {
        Name: string
        Members: (string * CSType) seq
        NamePrefix: string
        NameSuffix: string
     } with
        member this.FormatArray key = Formatters.arrayProperty (this.NamePrefix + this.Name + this.NameSuffix) key
        member this.FormatProperty ``type`` name = Formatters.property ``type`` name
        member this.FormatClass: string =
            this.Members
            |> Seq.map (fun (name, ``type``) ->
                match ``type`` with
                | GeneratedType x -> x.FormatClass + " " + x.FormatProperty (x.NamePrefix + x.Name + x.NameSuffix) x.Name
                | ArrType x -> x.FormatArray name
                | BaseType x -> x.FormatProperty name
            )
            |> Seq.reduce (fun x y -> x + " " + y)
            |> (fun x -> Formatters.``class`` (this.NamePrefix + this.Name + this.NameSuffix) x)

    and CSType =
        | BaseType of BaseType
        | GeneratedType of GeneratedType
        | ArrType of CSType

        member this.FormatArray key = match this with
                                      | BaseType x -> x.FormatArray key
                                      | GeneratedType x -> x.FormatClass + " " + x.FormatArray key
                                      | ArrType x -> x.FormatArray key
