# Install

In order to install this module, you will have to run `dotnet publish` in this directory and then import the module from the publish folder.

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
