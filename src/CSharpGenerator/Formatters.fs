module internal Formatters 
    let ``class`` name content =
        sprintf "public class %s { %s }" name content

    let ``namespace`` name content =
        sprintf "namespace %s { %s }" name content

    let property ``type`` name : string =
        sprintf "public %s %s { get; set; }" ``type`` name

    let arrayProperty ``type`` name =
        property (sprintf "%s[]" ``type``) name


