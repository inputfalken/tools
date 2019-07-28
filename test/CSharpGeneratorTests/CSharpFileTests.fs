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
let ``Empty Array``() =
    let result = CSharp.CreateFile("""[]""")
    let expected = "public object[] RootModel { get; set; }"
    assertion expected result
    
[<Fact>]
let ``Array with single number``() =
    let result = CSharp.CreateFile("""[1]""")
    let expected = "public decimal[] RootModel { get; set; }"
    assertion expected result

[<Fact>]
let ``Array with single string``() =
    let result = CSharp.CreateFile("""["bar"]""")
    let expected = "public string[] RootModel { get; set; }"
    assertion expected result

[<Fact>]
let ``Array with numbers``() =
    let result = CSharp.CreateFile("""[1,2,3,4]""")
    let expected = "public decimal[] RootModel { get; set; }"
    assertion expected result

[<Fact>]
let ``Array with strings``() =
    let result = CSharp.CreateFile("""["foo","bar"]""")
    let expected = "public string[] RootModel { get; set; }"
    assertion expected result

[<Fact>]
let ``Array with strings and numbers``() =
    let result = CSharp.CreateFile("""[1,2,3,"foo"]""")
    let expected = "public object[] RootModel { get; set; }"
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
    let expected = "public decimal Foo { get; set; } public string Bar { get; set; }"
    assertion expected result

[<Fact>]
let ``Object with nested int array``() =
    let result = CSharp.CreateFile """{"foo": 2, "items": [1,2,3,4,5]}"""
    let expected = "public decimal Foo { get; set; } public decimal[] Items { get; set; }"
    assertion expected result

