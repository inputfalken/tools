module Tests
open System
open TemplateFactory
open Xunit

let assertion expected actual =
    let expected = expected |> sprintf "public class RootModel { %s }"
    Assert.Equal(expected, actual)
    
     
[<Fact>]
let Object() =
    let result = CSharp.CreateFile("""{"Foo": 2}""")
    let expected = "public decimal Foo { get; set; }" 
    assertion expected result

[<Fact>]
let ``Object with nested string``() =
    let result = CSharp.CreateFile("""{

  "aps":

  {
       "alert":"tit}"

  }
}""")
    let expected = "public class ApsModel { public string Alert { get; set; } }"
    assertion expected result

[<Fact>]
let ``Two Objects``() =
    let result = CSharp.CreateFile """{"foo": 2, "bar": "this is a test"}"""
    let expected = sprintf "public decimal Foo { get; set; }%spublic string Bar { get; set; }" Environment.NewLine
    assertion expected result

[<Fact>]
let ``Object with nested int array``() =
    let result = CSharp.CreateFile """{"foo": 2, "items": [1,2,3,4,5]}"""
    let expected = "public decimal Foo { get; set; }public string Bar { get; set; }"
    assertion expected result

