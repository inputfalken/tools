module Tests
open TemplateFactory
open Xunit

let objectEntryAssertion expected actual =
    let expected = expected |> sprintf "public class RootModel { %s }"
    Assert.Equal(expected, actual)
    
let arrayEntryAssertion expected actual =
    Assert.Equal(expected, actual)


[<Fact>]
let Object() =
    let result = CSharp.CreateFile("""{"Foo": 2}""")
    let expected = "public decimal Foo { get; set; }"
    objectEntryAssertion expected result

[<Fact>]
let ``Empty Array``() =
    let result = CSharp.CreateFile("""[]""")
    let expected = "public object[] RootModel { get; set; }"
    arrayEntryAssertion expected result
    
[<Fact>]
let ``Array with single number``() =
    let result = CSharp.CreateFile("""[1]""")
    let expected = "public decimal[] RootModel { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with single string``() =
    let result = CSharp.CreateFile("""["bar"]""")
    let expected = "public string[] RootModel { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with numbers``() =
    let result = CSharp.CreateFile("""[1,2,3,4]""")
    let expected = "public decimal[] RootModel { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with strings``() =
    let result = CSharp.CreateFile("""["foo","bar"]""")
    let expected = "public string[] RootModel { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with strings and numbers``() =
    let result = CSharp.CreateFile("""[1,2,3,"foo"]""")
    let expected = "public object[] RootModel { get; set; }"
    arrayEntryAssertion expected result
    
[<Fact>]
let ``Array with object with decimal property``() =
    let result = CSharp.CreateFile """[
    {
        "foo": 1
    },
    {
        "foo": 2
    }
]
"""
    let expected = "public class RootModel { public decimal Foo { get; set; } }"
    arrayEntryAssertion expected result
    
[<Fact>]
let ``Array with object with decimal and string properties``() =
    let result = CSharp.CreateFile """[
    {
        "foo": 1,
        "bar": "Hi"
    },
    {
        "foo": 2,
        "bar": "Hi"
    }
]
"""
    let expected = "public class RootModel { public decimal Foo { get; set; } public string Bar { get; set; } }"
    arrayEntryAssertion expected result
    
    
[<Fact>]
let ``Array with object with array property with strings mixed items``() =
    let result = CSharp.CreateFile """[
    {
        "bar": [1, "foo"]
    },
    {
        "bar": [1, 2]
    }
]
"""
    let expected = "public class RootModel { public decimal Foo { get; set; } public object[] Bar { get; set; } }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Object with nested string``() =
    let result = CSharp.CreateFile("""{

  "aps":

  {
       "alert":"tit}"

  }
}""")
    let expected = "public class ApsModel { public string Alert { get; set; } }"
    objectEntryAssertion expected result

[<Fact>]
let ``Two Objects``() =
    let result = CSharp.CreateFile """{"foo": 2, "bar": "this is a test"}"""
    let expected = "public decimal Foo { get; set; } public string Bar { get; set; }"
    objectEntryAssertion expected result

[<Fact>]
let ``Object with nested int array``() =
    let result = CSharp.CreateFile """{"foo": 2, "items": [1,2,3,4,5]}"""
    let expected = "public decimal Foo { get; set; } public decimal[] Items { get; set; }"
    objectEntryAssertion expected result
    
[<Fact>]
let ``Object with nested string array``() =
    let result = CSharp.CreateFile """{"foo": 2, "items": ["foo","bar", "doe"]}"""
    let expected = "public decimal Foo { get; set; } public string[] Items { get; set; }"
    objectEntryAssertion expected result
    
    

