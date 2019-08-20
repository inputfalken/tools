namespace PowerShellModule
open System
open System.Collections.Generic
open System.Collections.Generic
open System.Management.Automation
open TemplateFactory

[<Cmdlet(VerbsCommon.New, "CSharpModel")>]
type CSharpFactory() =
    inherit PSCmdlet()

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = true, Position = 0, ValueFromPipeline = true)>]
    member val InputObject: string = "" with get, set

    [<ValidateSet("Camel", "Pascal")>]
    [<Parameter(Mandatory = false, Position = 1)>]
    member val Casing: string = "Pascal" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 2 )>]
    member val NameSpace: string = "" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 3 )>]
    member val RootObjectName: string = "Root" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 4 )>]
    member val ClassPrefix: string = "" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 5 )>]
    member val ClassSuffix: string = "" with get, set

    member val private Buffer: List<string> = new List<string>() with get

    override x.ProcessRecord() = x.Buffer.Add(x.InputObject)
    
    override x.EndProcessing() =
        let settings = new Settings(
                           Casing = x.Casing,
                           NameSpace = x.NameSpace,
                           ClassPrefix = x.ClassPrefix,
                           ClassSuffix = x.ClassSuffix,
                           RootObjectName = x.RootObjectName
                       )
        // TODO add proper support like https://github.com/PowerShell/PowerShell/blob/master/src/Microsoft.PowerShell.Commands.Utility/commands/utility/WebCmdlet/ConvertFromJsonCommand.cs
        CSharp.CreateFile(System.String.Join(Environment.NewLine, x.Buffer), settings) |> x.WriteObject
        
        
        
