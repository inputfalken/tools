namespace PowerShellModule
open System.Management.Automation
open TemplateFactory

[<Cmdlet(VerbsCommon.New, "CSharpModel")>]
type CSharpFactory() =
    inherit PSCmdlet()

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = true, Position = 0, ValueFromPipeline = true)>]
    member val Input : string = "" with get, set

    [<ValidateSet("Camel", "Pascal")>]
    [<Parameter(Mandatory = false, Position = 1)>]
    member val Casing : string = "Pascal" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 2 )>]
    member val NameSpace : string = "" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 3 )>]
    member val RootObjectName : string = "Root" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 4 )>]
    member val ClassPrefix : string = "" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 5 )>]
    member val ClassSuffix : string = "" with get, set


    override x.EndProcessing() =
        let settings = new Settings(
                           Casing = x.Casing,
                           NameSpace = x.NameSpace,
                           ClassPrefix = x.ClassPrefix,
                           ClassSuffix = x.ClassSuffix,
                           RootObjectName = x.RootObjectName
                       )
        CSharp.CreateFile(x.Input, settings) |> x.WriteObject
