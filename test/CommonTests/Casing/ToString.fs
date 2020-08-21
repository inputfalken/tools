module Tests.Casing.ToString

open Xunit
open Common.Casing

let CamelCase = Camel
let PascalCase = Pascal
let NoneCase = None

[<Fact>]
let ``Camel``() =
    Assert.Equal(Casing.CamelCase, CamelCase.ToString())
    
[<Fact>]
let ``Pascal``() =
    Assert.Equal(Casing.PascalCase, Pascal.ToString())
    
[<Fact>]
let ``None``() =
    Assert.Equal(Casing.NoneCase, None.ToString())
