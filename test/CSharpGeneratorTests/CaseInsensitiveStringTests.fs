module CSharp.CaseInsensitiveStringTests

open Xunit
open Common.CaseInsensitiveString

// TODO move to seperate assembly.
[<Theory>]
[<InlineData("foo","foo")>]
[<InlineData("Foo", "foo")>]
[<InlineData("FOO", "fOo")>]
[<InlineData(null, null)>]
[<InlineData("", "")>]
let ```Equal`` left right =
    let left = left |> CI
    let  right = right |>  CI
    Assert.Equal(left, right)
    
[<Theory>]
[<InlineData("foo","foo")>]
[<InlineData("Foo", "foo")>]
[<InlineData("FOO", "fOo")>]
[<InlineData(null, null)>]
[<InlineData("", "")>]
let ```Equal works on string`` left right =
    let left = left |> CI
    Assert.True(left.Equals(right))
        

[<Theory>]
[<InlineData("foo","bar")>]
[<InlineData("Foo", "foo ")>]
[<InlineData("FOO", "fOo2")>]
[<InlineData(null, "")>]
let ```Not equal`` left right =
    let left = left |> CI
    let  right = right |>  CI
    Assert.NotEqual(left, right)
    
[<Theory>]
[<InlineData("foo","foo")>]
[<InlineData("Foo", "foo")>]
[<InlineData("FOO", "fOo")>]
[<InlineData(null, null)>]
[<InlineData("", "")>]
let ```Set does contain`` left right =
    let set = Set.empty 
    let set  = left  |> CI |> set.Add
    let right = right |> CI
    Assert.True(set.Contains right)
    
[<Theory>]
[<InlineData("foo","bar")>]
[<InlineData("Foo", "foo ")>]
[<InlineData("FOO", "fOo2")>]
[<InlineData(null, "")>]
let ```Set does not contain`` left right =
    let set = Set.empty 
    let set  = left  |> CI |> set.Add
    let right = right |> CI
    Assert.False(set.Contains right)
