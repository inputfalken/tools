using System.Threading.Tasks;
using CurrieTechnologies.Razor.Clipboard;
using Microsoft.AspNetCore.Blazor.Hosting;

namespace GitHubPage
{
    public class Program
    {
        public static Task Main(string[] args)
        {
            var builder = WebAssemblyHostBuilder.CreateDefault(args);
            builder.RootComponents.Add<App>("app");
            builder.Services.AddClipboard();
            return builder.Build().RunAsync();
        }
    }
}
