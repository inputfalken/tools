module Tests
open JsonParser
open TemplateFactory
open Xunit

[<Fact>]
let ``One Object``() =
    let result = CSharp.CreateFile("""{"FooBar": 2}""")
    Assert.Equal("public decimal Foo { get; set; }", result)

[<Fact>]
let ``Two  Objects``() =
    let result = CSharp.CreateFile """{"foo": 2, "bar": "this is a test"}"""
    Assert.Equal("public decimal Foo { get; set; }public string Bar { get; set; }", result)
