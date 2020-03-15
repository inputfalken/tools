module Tests.Casing.ToString

open Xunit
open Common.Casing

let CamelCase = Common.Casing.Camel
let PascalCase = Common.Casing.Pascal
let NoneCase = Common.Casing.None

[<Fact>]
let ``Camel``() =
    Assert.Equal(Common.Casing.Casing.CamelCase, CamelCase.ToString())
    
[<Fact>]
let ``Pascal``() =
    Assert.Equal(Common.Casing.Casing.PascalCase, Pascal.ToString())
    
[<Fact>]
let ``None``() =
    Assert.Equal(Common.Casing.Casing.NoneCase, None.ToString())
