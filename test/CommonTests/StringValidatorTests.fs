module Tests.StringValidator

open Xunit
open Common.StringValidator

[<Theory>]
[<InlineData(null)>]
[<InlineData("")>]
[<InlineData(" ")>]
[<InlineData("   ")>]
[<InlineData("\t")>]
let ``Strings whose expected to not have value`` string =
    let res = string |> valueExists
    Assert.Equal(Option.None, res)

[<Theory>]
[<InlineData("foo", "foo")>]
[<InlineData("123", "123")>]
[<InlineData(" foo", "foo")>]
[<InlineData(" 123", "123")>]
[<InlineData("\t123", "123")>]
[<InlineData("\tfoo", "foo")>]
[<InlineData("foo ", "foo")>]
[<InlineData("123 ", "123")>]
[<InlineData("123\t", "123")>]
[<InlineData("foo\t", "foo")>]
[<InlineData(" foo ", "foo")>]
[<InlineData(" 123 ", "123")>]
[<InlineData("\t123\t", "123")>]
[<InlineData("\tfoo\t", "foo")>]
[<InlineData(" \tfoo\t ", "foo")>]
[<InlineData(" \t123\t ", "123")>]
[<InlineData("\t 123 \t", "123")>]
[<InlineData("\t foo \t", "foo")>]
let ``Strings whose expected to have value`` string expected =
    let res = string |> valueExists
    Assert.Equal(Option.Some expected, res)
