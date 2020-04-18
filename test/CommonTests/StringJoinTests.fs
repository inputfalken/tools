module Tests.StringJoinTests

open Xunit
open Common.StringJoin

[<Fact>]
let ``Space seperation with empty list`` () =
    let result = [] |> joinStringsWithSpaceSeparation
    Assert.Equal("", result)

[<Fact>]
let ``Space seperation with single element list`` () =
    let result = [ "foo" ] |> joinStringsWithSpaceSeparation
    Assert.Equal("foo", result)

[<Fact>]
let ``Space seperation with double element list`` () =
    let result = [ "foo"; "bar" ] |> joinStringsWithSpaceSeparation
    Assert.Equal("foo bar", result)

[<Fact>]
let ``With empty list`` () =
    let result = [] |> joinStrings
    Assert.Equal("", result)

[<Fact>]
let ``With single element list`` () =
    let result = [ "foo" ] |> joinStrings
    Assert.Equal("foo", result)

[<Fact>]
let ``With double element list`` () =
    let result = [ "foo"; "bar" ] |> joinStrings
    Assert.Equal("foobar", result)
