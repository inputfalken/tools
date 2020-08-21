module Tests.Casing.FromString

open Xunit
open Common.Casing

let CamelCase = Casing.CamelCase
let PascalCase = Casing.PascalCase
let NoneCase = Casing.NoneCase

[<Fact>]
let ``Camel``() =
    Assert.Equal(Option.Some Camel, CamelCase |> Casing.fromString)
    
[<Fact>]
let ``Pascal``() =
    Assert.Equal(Option.Some Pascal, PascalCase |> Casing.fromString)
    
[<Fact>]
let ``None``() =
    Assert.Equal(Option.Some None, NoneCase |> Casing.fromString)
    
[<Theory>]
[<InlineData("")>]
[<InlineData(null)>]
[<InlineData("foo")>]
[<InlineData("bar")>]
[<InlineData("foobar")>]
let ``Other`` data =
    Assert.Equal(Option.None, data |> Casing.fromString)
    
    
