namespace PowerShellModule
open JsonParser
open System.Management.Automation
open TemplateFactory

[<Cmdlet(VerbsCommon.New, "CSharpModel")>]
type CSharpFactory() =
    inherit PSCmdlet()

    [<Parameter(Mandatory = true, Position = 0, ValueFromPipeline = true)>]
    member val Input : string = "" with get, set

    [<ValidateSet("Camel", "Pascal")>]
    [<Parameter(Mandatory = false, Position = 1)>]
    member val Casing : string = "Pascal" with get, set


    [<Parameter(Mandatory = false, Position = 2 )>]
    member val NameSpace : string = "" with get, set

    override x.EndProcessing() =
        let settings = {
            NameSpace = x.NameSpace |> Option.Some |> Option.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))
            Casing = x.Casing |> CasingRule.fromString
        }
        CSharp.CreateFile(x.Input, settings) |> x.WriteObject
