module CSharpGeneratorTests.SettingsTransformationTests

open CSharp.Types
open Common.Casing
open Generator
open Xunit
open Generator.Config


let defaultSettings() =
    { RootName = "root"
      NameSpace = Option.None
      LetterRule = "model" |> LetterRule.Suffix
      ClassCasing = Casing.Pascal
      PropertyCasing = Casing.Pascal }


[<Fact>]
let ``Empty object``() =
    let result = transformCSharpSettings (CSharpSettings())
    Assert.Equal(defaultSettings(), result)

[<Theory>]
[<InlineData("Foobar")>]
[<InlineData("Foo")>]
[<InlineData("Bar")>]
[<InlineData("John")>]
[<InlineData("Doe")>]
let ``Setting namespace`` nameSpace =
    let result = transformCSharpSettings (CSharpSettings(NameSpace = nameSpace))
    Assert.Equal(Option.Some(nameSpace), result.NameSpace)

[<Fact>]
let ``Expect Suffix when ClassSuffix is set``() =
    let result = transformCSharpSettings (CSharpSettings(ClassPrefix = System.String.Empty, ClassSuffix = "Foo"))
    Assert.Equal(LetterRule.Suffix "Foo", result.LetterRule)

[<Fact>]
let ``Expect prefix when ClassPrefix is set``() =
    let result = transformCSharpSettings (CSharpSettings(ClassPrefix = "Foo", ClassSuffix = System.String.Empty))
    Assert.Equal(LetterRule.Prefix "Foo", result.LetterRule)
    
[<Fact>]
let ``Expect prefix and suffix when ClassPrefix and ClassSuffix is set``() =
    let result = transformCSharpSettings (CSharpSettings(ClassPrefix = "Foo", ClassSuffix = "Foo"))
    Assert.Equal(LetterRule.``Prefix and Suffix`` ("Foo", "Foo"), result.LetterRule)

[<Theory>]
[<InlineData(" ", " ")>]
[<InlineData("\t", "\t")>]
[<InlineData("", "")>]
[<InlineData(null, null)>]
[<InlineData(null, "")>]
[<InlineData("", null)>]
let ``Uses default settings if ClassSuffix or ClassPrefix is set to empty string or null`` classPrefix classSuffix =
    let result = transformCSharpSettings (CSharpSettings(ClassPrefix = classPrefix, ClassSuffix = classSuffix))
    Assert.Equal(defaultSettings(), result)

[<Theory>]
[<InlineData(" ", " ")>]
[<InlineData("\t", "\t")>]
[<InlineData("", "")>]
[<InlineData(null, null)>]
[<InlineData(null, "")>]
[<InlineData("", null)>]
let ``Uses default settings if PropertyCasing or ClassCasing is set to empty string or null`` classCasing propertyCasing =
    let result = transformCSharpSettings (CSharpSettings(ClassCasing = classCasing, PropertyCasing = propertyCasing))
    Assert.Equal(defaultSettings(), result)

// TODO unit test if casing can be translated.
