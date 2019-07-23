# Install

In order to install this module, you will have to run `dotnet publish` in this directory and then import the module from the publish folder.

## Cmdlets

### `New-CSharpModel`

Creates a CSharp model from json

```powerShell
  $json = '{"foo": "bar"}'
  $jsonModel = $json | New-CsharpModel
  $jsonModel | New-Item -Type File -Name 'Model.cs'
```

