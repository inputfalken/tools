namespace PowerShellModule

open System
open System.Collections.Generic
open System.Management.Automation
open CSharpGenerator.Arguments
open CSharpGenerator

/// <summary>
/// The ConvertFrom-Json command.
/// This command converts a Json string representation to a CSharp string.
/// </summary>
[<Cmdlet(VerbsCommon.New, "CSharpModel")>]
type CSharpFactory() =
    inherit PSCmdlet()

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = true, Position = 0, ValueFromPipeline = true, HelpMessage = "The JSON.")>]
    member val InputObject: string = "" with get, set

    [<ValidateSet("Camel", "Pascal", "None")>]
    [<Parameter(Mandatory = false, Position = 1, HelpMessage = "Sets the casing rules for class and property names.")>]
    member val Casing: string = "Pascal" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 2, HelpMessage = "Sets the name of the namespace.")>]
    member val NameSpace: string = "" with get, set

    [<ValidateNotNullOrEmpty>]
    [<Parameter(Mandatory = false, Position = 3, HelpMessage = "Sets the name of the root object.")>]
    member val ObjectName: string = "Root" with get, set

    [<Parameter(Mandatory = false, Position = 4, HelpMessage = "Sets a prefix for the class names.")>]
    member val Prefix: string = "" with get, set

    [<Parameter(Mandatory = false, Position = 5, HelpMessage = "Suffix a prefix for the class names.")>]
    member val Suffix: string = "" with get, set

    member val private Buffer: List<string> = List<string>()

    override x.ProcessRecord() = x.Buffer.Add(x.InputObject)

    override x.EndProcessing() =
        let settings =
            Settings
                (PropertyCasing = x.Casing, NameSpace = x.NameSpace, ClassPrefix = x.Prefix, ClassSuffix = x.Suffix,
                 RootObjectName = x.ObjectName)
        // TODO add proper support like https://github.com/PowerShell/PowerShell/blob/master/src/Microsoft.PowerShell.Commands.Utility/commands/utility/WebCmdlet/ConvertFromJsonCommand.cs
        CSharp.CreateFile(String.Join(Environment.NewLine, x.Buffer), settings) |> x.WriteObject
