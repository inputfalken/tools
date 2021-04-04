module Tests.CIString

open Xunit
open Common

[<Theory>]
[<InlineData("foo", "foo")>]
[<InlineData("Foo", "foo")>]
[<InlineData("FOO", "fOo")>]
[<InlineData(null, null)>]
[<InlineData("", "")>]
let ``Equal with CIString`` left right =
    let left = left |> CI
    let right = right |> CI
    Assert.Equal(left, right)

[<Theory>]
[<InlineData("foo", "bar")>]
[<InlineData("Foo", "foo ")>]
[<InlineData("FOO", "fOo2")>]
[<InlineData(null, "")>]
let ``Not equal with CIString`` left right =
    let left = left |> CI
    let right = right |> CI
    Assert.NotEqual(left, right)

[<Theory>]
[<InlineData("foo", "foo")>]
[<InlineData("Foo", "foo")>]
[<InlineData("FOO", "fOo")>]
[<InlineData(null, null)>]
[<InlineData("", "")>]
let ``Equal with string`` left right =
    let left = left |> CI
    Assert.True(left.Equals(right))

[<Theory>]
[<InlineData("foo", "bar")>]
[<InlineData("Foo", "foo ")>]
[<InlineData("FOO", "fOo2")>]
[<InlineData(null, "")>]
let ``Not equal with string`` left right =
    let left = left |> CI
    let right = right
    Assert.False(left.Equals(right))

[<Theory>]
[<InlineData("foo", "foo")>]
[<InlineData("Foo", "foo")>]
[<InlineData("FOO", "fOo")>]
[<InlineData(null, null)>]
[<InlineData("", "")>]
let ``Equal with Option Some`` left right =
    let left = left |> CI
    let right = right |> Option.Some
    Assert.True(left.Equals(right))

[<Theory>]
[<InlineData("foo", "bar")>]
[<InlineData("Foo", "foo ")>]
[<InlineData("FOO", "fOo2")>]
[<InlineData(null, "")>]
let ``Not equal with Option Some`` left right =
    let left = left |> CI
    let right = right |> Option.Some
    Assert.False(left.Equals(right))

[<Theory>]
[<InlineData(null)>]
let ``Equal with Option None`` left =
    let left = left |> CI
    let right = Option.None
    Assert.True(left.Equals(right))

[<Theory>]
[<InlineData("foo")>]
[<InlineData("Foo")>]
[<InlineData("FOO")>]
let ``Not equal with Option None`` left =
    let left = left |> CI
    let right = Option.None
    Assert.False(left.Equals(right))

[<Theory>]
[<InlineData("foo", "foo")>]
[<InlineData("Foo", "foo")>]
[<InlineData("FOO", "fOo")>]
[<InlineData(null, null)>]
[<InlineData("", "")>]
let ``Set does contain`` left right =
    let set = Set.empty

    let set =
        left
        |> CI
        |> set.Add

    let right = right |> CI
    Assert.True(set.Contains right)

[<Theory>]
[<InlineData("foo", "bar")>]
[<InlineData("Foo", "foo ")>]
[<InlineData("FOO", "fOo2")>]
[<InlineData(null, "")>]
let ``Set does not contain`` left right =
    let set = Set.empty

    let set =
        left
        |> CI
        |> set.Add

    let right = right |> CI
    Assert.False(set.Contains right)
