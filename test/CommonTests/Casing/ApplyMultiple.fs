module CommonTests.Casing.ApplyMultiple

open Xunit
open Common

[<Fact>]
let Camel() =
    let list = [ "foo"; null; "bar"; ""; "john"; "doe" ]
    Assert.Equal("fooBarJohnDoe", list |> Camel.applyMultiple)

[<Fact>]
let Pascal() =
    let list = [ "foo"; null; "bar"; ""; "john"; "doe" ]
    Assert.Equal("FooBarJohnDoe", list |> Pascal.applyMultiple)

[<Fact>]
let None() =
    let list = [ "foo"; null; "bar"; ""; "john"; "doe" ]
    Assert.Equal("foobarjohndoe", list |> None.applyMultiple)
    
[<Fact>]
let ``Camel mixed with cased strings``() =
    let list = ["fooBar"; "foo"]
    Assert.Equal("fooBarFoo", list |> Casing.Camel.applyMultiple)

[<Fact>]
let ``Pascal mixed with cased strings``() =
    let list = ["fooBar"; "foo"]
    Assert.Equal("FooBarFoo", list |> Casing.Pascal.applyMultiple)

[<Fact>]
let ``None mixed with cased strings``() =
    let list = ["fooBar"; "foo"]
    Assert.Equal("fooBarfoo", list |> Casing.None.applyMultiple)
