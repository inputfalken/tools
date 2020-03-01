# Installation

* `> dotnet publish --configuration Release`
* `> Import-Module .\bin\Release\netcoreapp3.1\publish\PowerShellModule.dll`

## Cmdlets

### `New-CSharpModel`

Creates a CSharp model from json

```powerShell
$json = '{"foo": "bar"}'
$CSharpModel = $json | New-CsharpModel
$CSharpModel | New-Item -Type File -Name 'Model.cs'
```
The `$CSharpModel` would in this case look like 

```csharp
public class RootModel { public string Foo { get; set; } }
```
