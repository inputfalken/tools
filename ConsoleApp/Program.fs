open System
open TemplateFactory

[<EntryPoint>]
let main argv =
    let result = CSharp.ParseJson """
    {
        "bar" : {
            "test":2
        }
    }
"""
    let res = CSharp.CreateFile result


    printfn "%O" result
    printfn "%s" res
    1

