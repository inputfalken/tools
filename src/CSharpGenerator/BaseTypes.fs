namespace CSharpGenerator.Types
    open System.Reflection
    open System.Reflection
    open System
    open JsonParser

    module private Formatters =
        let ``class`` name content =
            sprintf "public class %s { %s }" name content

        let property ``type`` name: string =
            sprintf "public %s %s { get; set; }" ``type`` name

        let arrayProperty ``type`` name =
            property (sprintf "%s[]" ``type``) name

    type internal TypeInfo = {
        Name: string
        Namespace: string
        Alias: string option
    }
    with
        member this.FormatArray key = Formatters.arrayProperty (this.ToString()) key
        member this.FormatProperty key = Formatters.property (this.ToString()) key
        override this.ToString() = this.Alias |> Option.defaultValue (this.Namespace + "." + this.Name)

    type internal ReferenceType = {
        Info : TypeInfo
    }
    with
        static member String = {Info = { Namespace = "System"; Name = "String"; Alias = option.Some "string" }} : ReferenceType
        static member Object = {Info= { Name = "Object"; Namespace = "System"; Alias = option.Some "object" }} : ReferenceType
    type internal ValueType = {
        Info : TypeInfo
    }
    with
        static member Decimal = {Info = { Namespace = "System"; Name = "Decimal"; Alias = Option.Some "decimal" }} : ValueType
        static member DateTime = {Info = { Namespace = "System"; Name = "DateTime"; Alias = option.None } } : ValueType
        static member Boolean = {Info = { Namespace = "System"; Name = "Boolean"; Alias = option.Some "bool" }} : ValueType
        static member Guid = {Info = { Namespace = "System"; Name = "Guid"; Alias = option.None }} : ValueType
        static member Double = {Info = { Namespace = "System"; Name = "Double"; Alias = option.Some "double" }} : ValueType
    
    type internal BaseType =
        | ReferenceType of ReferenceType
        | ValueType of ValueType
        
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
                | BaseType x ->
                    match x with 
                    | ReferenceType x -> x.Info.FormatProperty property.Name
                    | ValueType x -> x.Info.FormatProperty property.Name
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
                                      | BaseType x ->
                                        match x with 
                                        | ReferenceType x -> x.Info.FormatArray key
                                        | ValueType x -> x.Info.FormatArray key
                                      | GeneratedType x -> x.ClassDeclaration key + " " + Formatters.arrayProperty (x.NamePrefix + key + x.NameSuffix) key
                                      | ArrType x -> x.FormatArray key
