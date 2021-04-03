using System.Threading.Tasks;
using CurrieTechnologies.Razor.Clipboard;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;

namespace GitHubPage
{
    public class Program
    {
        public static async Task Main(string[] args)
        {
            var builder = WebAssemblyHostBuilder.CreateDefault(args);
            builder.RootComponents.Add<App>("app");

            builder.Services
                .AddClipboard();


            await builder.Build().RunAsync();
        }
    }
}