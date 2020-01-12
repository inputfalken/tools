# Running localy

To run this function in a local environment; you must add a file called `local.settings.json` to this directory. The default content is the following:

```
{
    "IsEncrypted": false,
    "Values": {
        "AzureWebJobsStorage": "UseDevelopmentStorage=True",
        "FUNCTIONS_WORKER_RUNTIME": "dotnet"
    }
}
```
