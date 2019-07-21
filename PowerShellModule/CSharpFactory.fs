namespace PowerShellModule
open System.Management.Automation
open TemplateFactory


[<Cmdlet(VerbsCommon.New, "CSharpModel")>]
type CSharpFactory() =
    inherit PSCmdlet()

    [<Parameter(Mandatory = true, Position = 0, ValueFromPipeline = true)>]
    member val Input : string = "" with get, set

    override x.EndProcessing() =
        x.Input |> CSharp.CreateFile |> x.WriteObject
