module Tests

open CSharpGenerator
open CSharpGenerator.Arguments
open Xunit

let objectEntryAssertion expected actual =
    let expected = expected |> sprintf "public class RootModel { %s }"
    Assert.Equal(expected, actual)

let arrayEntryAssertion expected actual =
    Assert.Equal(expected, actual)


[<Fact>]
let ``Camel case works for generated classes``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = "Camel"))

    let expected = "public class rootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result)

[<Fact>]
let ``No case rules works``() =
    let result =
        CSharp.CreateFile("""
    {
        "foobar" : []
    }
    """, Settings(Casing = "None"))
        
    let expected = "public class rootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result)
    

[<Fact>]
let ``Two array Objects next to each other``() =
    let json = """
    [
      {
        "tags": [ "non" ],
        "friends": [
          {
            "id": 0,
            "name": "Henrietta Tillman"
          }
        ]
      },
      {
    "tags": [ "aliqua" ],
    "friends": [
      {
        "id": 0,
        "name": "Kristy Calhoun"
      }
    ]
  }
    ]
    """
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public string[] Tags { get; set; } public class FriendsModel { public decimal Id { get; set; } public string Name { get; set; } } public FriendsModel[] Friends { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let Object() =
    let result = CSharp.CreateFile("""{"Foo": 2}""")
    let expected = "public decimal Foo { get; set; }"
    objectEntryAssertion expected result

[<Fact>]
let ``Object with Empty Array``() =
    let result =
        CSharp.CreateFile """
    {
        "Foo" : []
    }
    """

    let expected = "public object[] Foo { get; set; }"
    objectEntryAssertion expected result

[<Fact>]
let ``Object with Empty object``() =
    let result =
        CSharp.CreateFile """
    {
        "Foo" : {}
    }
    """

    let expected = "public object Foo { get; set; }"
    objectEntryAssertion expected result

[<Fact>]
let ``Empty Array``() =
    let result = CSharp.CreateFile("""[]""")
    let expected = "public object[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Empty Object``() =
    let result = CSharp.CreateFile("""{}""")
    let expected = "public object Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with single number``() =
    let result = CSharp.CreateFile("""[1]""")
    let expected = "public decimal[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with single string``() =
    let result = CSharp.CreateFile("""["bar"]""")
    let expected = "public string[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with numbers``() =
    let result = CSharp.CreateFile("""[1,2,3,4]""")
    let expected = "public decimal[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with strings``() =
    let result = CSharp.CreateFile("""["foo","bar"]""")
    let expected = "public string[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with strings and numbers``() =
    let result = CSharp.CreateFile("""[1,2,3,"foo"]""")
    let expected = "public object[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with object with decimal property``() =
    let result =
        CSharp.CreateFile """[
    {
        "foo": 1
    },
    {
        "foo": 2
    }
]
"""

    let expected = "public class RootModel { public decimal Foo { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with object with decimal and string properties``() =
    let result =
        CSharp.CreateFile """[
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

    let expected =
        "public class RootModel { public decimal Foo { get; set; } public string Bar { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result


[<Fact>]
let ``Array with object with array property with strings mixed items``() =
    let result =
        CSharp.CreateFile """[
    {
        "bar": [1, "foo"]
    },
    {
        "bar": [1, 2]
    }
]
"""

    let expected = "public class RootModel { public object[] Bar { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with object with array property with strings mixed items reversed order``() =
    let result =
        CSharp.CreateFile """[
    {
        "bar": [1, 2]
    },
    {
        "bar": [1, "foo"]
    }
]
"""

    let expected = "public class RootModel { public object[] Bar { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Object with nested string``() =
    let result =
        CSharp.CreateFile
            ("""{
  "aps":
  {
       "alert":"tit}"
  }
}"""



            )
    let expected = "public class ApsModel { public string Alert { get; set; } } public ApsModel Aps { get; set; }"
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

[<Fact>]
let ``Object with object array``() =
    let result =
        CSharp.CreateFile """{ "Models": [
        {
            "foo": 1
        },
        {
            "foo": 2
        }
    ]}
    """

    let expected =
        "public class ModelsModel { public decimal Foo { get; set; } } public ModelsModel[] Models { get; set; }"
    objectEntryAssertion expected result

[<Fact>]
let ``Object with null value``() =
    let result = CSharp.CreateFile """{ "Foo" : null }"""
    let expected = "public object Foo { get; set; }"
    objectEntryAssertion expected result

[<Fact>]
let ``Object with null value and none null value``() =
    let result = CSharp.CreateFile """{ "Foo" : null, "Bar" : 1 }"""
    let expected = "public object Foo { get; set; } public decimal Bar { get; set; }"
    objectEntryAssertion expected result

[<Fact>]
let ``Array with strings that are mixed with null values``() =
    let result =
        CSharp.CreateFile """
    [
        "foo",
        "bar",
        "null"
    ]
    """

    let expected = "public string[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with numbers that are mixed with null values``() =
    let result =
        CSharp.CreateFile """
    [
        1,
        2,
        null,
        4
    ]
    """

    let expected = "public decimal?[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array mixed with strings and numbers``() =
    let result =
        CSharp.CreateFile """
    [
        "foo",
        2,
        4
    ]
    """

    let expected = "public object[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with missmatched properties``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : "foobar",
            "John" : 2
        },
        {
            "Bar" : null,
            "Doe" : 2
        }
    ]
    """

    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal? John { get; set; } public object Bar { get; set; } public decimal? Doe { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result
    
[<Fact>]
let ``Array with without values first``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : null,
            "John" : null
        },
        {
            "Bar" : null,
            "Doe" : null
        },
        {
            "Foo" : "foo",
            "John" : "doo",
            "Bar" : 2,
            "Doe" : 4
        }
    ]
    """

    let expected =
        "public class RootModel { public string Foo { get; set; } public string John { get; set; } public decimal? Bar { get; set; } public decimal? Doe { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result
[<Fact>]
let ``Array with objects with two structures``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : "foobar",
            "John" : 2
        },
        {
            "Bar" : null,
            "Doe" : 2
        },
        {
            "Foo" : null,
            "John" : 2
        },
        {
            "Bar" : null,
            "Doe" : 2
        }
    ]
    """

    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal? John { get; set; } public object Bar { get; set; } public decimal? Doe { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with objects with properties whose values are mixed with nullable value types``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : null,
            "Bar" : 2
        },
        {
            "Foo" : 4,
            "Bar" : 2
        }
    ]
    """

    let expected =
        "public class RootModel { public decimal? Foo { get; set; } public decimal Bar { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with objects with properties whose values are mixed with nullable value types reversed``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : null,
            "Bar" : 2
        },
        {
            "Foo" : 4,
            "Bar" : 2
        }
    ]
    """

    let expected =
        "public class RootModel { public decimal? Foo { get; set; } public decimal Bar { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with objects with properties whose values are mixed with null``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : null,
            "Bar" : 2
        },
        {
            "Foo" : "foobar",
            "Bar" : 2
        }
    ]
    """

    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal Bar { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with objects with different amount of properties reversed``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : null,
            "Bar" : 2,
            "test" : 2
        },
        {
            "Foo" : "foobar",
            "Bar" : 2
        }
    ]
    """

    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal Bar { get; set; } public decimal? Test { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with objects with different amount of properties``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : null,
            "Bar" : 2
        },
        {
            "Foo" : "foobar",
            "Bar" : 2,
            "test" : 2
        }
    ]
    """

    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal Bar { get; set; } public decimal? Test { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result

[<Fact>]
let ``Array with objects with properties whose values are mixed with null reversed``() =
    let result =
        CSharp.CreateFile """
    [
        {
            "Foo" : "foobar",
            "Bar" : 2
        },
        {
            "Foo" : null,
            "Bar" : 2
        }
    ]
    """

    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal Bar { get; set; } } public RootModel[] Root { get; set; }"
    arrayEntryAssertion expected result
