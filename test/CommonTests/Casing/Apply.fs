module CommonTests.Casing.Apply

open Xunit
open Common.Casing

[<Theory>]
[<InlineData("foo bar")>]
[<InlineData("fooBar")>]
[<InlineData("FooBar")>]
[<InlineData("foo_bar")>]
[<InlineData("Foo_Bar")>]
let ``Camel`` data =
    Assert.Equal("fooBar", data |> Casing.Camel.apply)
    
[<Theory>]
[<InlineData("foo bar")>]
[<InlineData("fooBar")>]
[<InlineData("FooBar")>]
[<InlineData("foo_bar")>]
[<InlineData("Foo_Bar")>]
let ``Pascal`` data =
    Assert.Equal("FooBar", data |> Casing.Pascal.apply)
    
[<Theory>]
[<InlineData("foo bar", "foobar")>]
[<InlineData("fooBar", "fooBar")>]
[<InlineData("FooBar", "FooBar")>]
[<InlineData("foo_bar", "foo_bar")>]
[<InlineData("Foo_Bar", "Foo_Bar")>]
let ``None`` data expected =
    Assert.Equal(expected, data |> Casing.None.apply)
