namespace CSharpGenerator.Types
    open JsonParser

    module private Formatters =
        let ``class`` name content =
            sprintf "public class %s { %s }" name content

        let property ``type`` name: string =
            sprintf "public %s %s { get; set; }" ``type`` name

        let arrayProperty ``type`` name =
            property (sprintf "%s[]" ``type``) name

    type internal BaseType = {
        Name: string
        Namespace: string
        Alias: string option
    }
    with
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

    type internal GeneratedType = {
        Members: Property list
        NamePrefix: string
        NameSuffix: string
    }
    with
        member this.FormatProperty ``type`` name = Formatters.property ``type`` name
        member this.ClassDeclaration name: string =
            let name = this.NamePrefix + name + this.NameSuffix
            this.Members
            |> Seq.map (fun property ->
                match property.Type with
                | GeneratedType x -> x.ClassDeclaration property.Name + " " + x.FormatProperty (x.NamePrefix + property.Name + x.NameSuffix) property.Name
                | ArrType x -> x.FormatArray property.Name
                | BaseType x -> x.FormatProperty property.Name
            )
            |> Seq.reduce (fun x y -> x + " " + y)
            |> (fun x -> Formatters.``class`` name x)

    and internal Property = {
        Name: string
        Type : CSType
    }
    and internal CSType =
        | BaseType of BaseType
        | GeneratedType of GeneratedType
        | ArrType of CSType
        member this.FormatArray key = match this with
                                      | BaseType x -> x.FormatArray key
                                      | GeneratedType x -> x.ClassDeclaration key + " " + Formatters.arrayProperty (x.NamePrefix + key + x.NameSuffix) key
                                      | ArrType x -> x.FormatArray key
