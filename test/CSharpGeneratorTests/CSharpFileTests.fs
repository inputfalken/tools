module Tests
open TemplateFactory
open Xunit

[<Fact>]
let ``One Object``() =
    let result = CSharp.CreateFile("""{"Foo": 2}""")
    Assert.Equal("public decimal Foo { get; set; }", result)
    
[<Fact>]
let ``One object with nested object using white spaces``() =
    let result = CSharp.CreateFile("""{

  "aps":

  {
       "alert":"tit}"

  }
}""")
    Assert.Equal("public class ApsModel { public string Alert { get; set; } }", result)
[<Fact>]
let ``Two  Objects``() =
    let result = CSharp.CreateFile """{"foo": 2, "bar": "this is a test"}"""
    Assert.Equal("public decimal Foo { get; set; }public string Bar { get; set; }", result)
