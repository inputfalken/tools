open System
open TemplateFactory

[<EntryPoint>]
let main argv =
    let result = CSharp.CreateFile"""
    {
        "bar" : {
            "test":2
        }
    }
""" 


    printfn "%O" result
    1

