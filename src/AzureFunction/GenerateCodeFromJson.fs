namespace AzureFunction

open System
open System.IO
open Microsoft.AspNetCore.Mvc
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open Microsoft.Extensions.Logging

module GenerateCodeFromJsonFN =
    // Define a nullable container to deserialize into.
    [<AllowNullLiteral>]
    type JsonContainer() =
        member val Json = String.Empty with get, set

    // For convenience, it's better to have a central place for the literal.
    [<Literal>]
    let Name = "name"

    [<FunctionName("GenerateCodeFromJsonFN")>]
    let run ([<HttpTrigger(AuthorizationLevel.Function, "post", Route = null)>] req: HttpRequest) (log: ILogger) =
        async {
            log.LogInformation("F# HTTP trigger function processed a request.")

            use stream = new StreamReader(req.Body)
            let! reqBody = stream.ReadToEndAsync() |> Async.AwaitTask

            let data = JsonConvert.DeserializeObject<JsonContainer>(reqBody)
            
            let json =
                match data with
                | null -> ""
                | nc -> nc.Json

            if not (String.IsNullOrWhiteSpace(json)) then
                return OkObjectResult(CSharpGenerator.CSharp.CreateFile(json)) :> IActionResult
            else
                return BadRequestObjectResult("Could not find json") :> IActionResult
        }
        |> Async.StartAsTask
