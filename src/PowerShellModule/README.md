# Installation

* `> dotnet publish --configuration Release --framework netcoreapp2.2`
* `> Import-Module .\bin\Release\netcoreapp2.2\publish\PowerShellModule.dll`

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
