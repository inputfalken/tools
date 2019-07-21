using System.Management.Automation;
using TemplateFactory;

namespace CodeGenerator {
    [Cmdlet(VerbsCommon.New, "CSharpModel")]
    public class CSharpFactory : PSCmdlet {
        [Parameter(ValueFromPipeline = true, Mandatory = true, Position = 0)]
        public string Input { get; set; }

        protected override void ProcessRecord() {
            WriteObject(CSharp.CreateFile(Input));
        }
    }
}