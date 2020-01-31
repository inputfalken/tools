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
let ``Passing valid JSON sets a value in Value``() =
    let result = CSharp.CreateFile(""" { } """)
    Assert.NotNull(result.Either.Value)
    Assert.Null(result.Either.Error)

[<Fact>]
let ``Passing invalid JSON sets a value in Value``() =
    let result = CSharp.CreateFile("""abc""")
    Assert.Null(result.Either.Value)
    Assert.NotNull(result.Either.Error)

[<Fact>]
let ``Allow prefix to be empty if suffix is set``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(ClassSuffix = "foo", ClassPrefix = System.String.Empty))

    let expected = "public class RootFoo { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Fact>]
let ``Allow suffix to be empty if prefix is set``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(ClassPrefix = "foo", ClassSuffix = System.String.Empty))

    let expected = "public class FooRoot { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Fact>]
let ``Handle reserved words skip prefixing property name if casing is not matched``() =
    let result =
        CSharp.CreateFile("""
    {
        "object" : []
    }
    """, Settings(ClassPrefix = "foo", ClassSuffix = System.String.Empty))

    let expected = "public class FooRoot { public object[] Object { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let ``Handle reserved words prefixing property name if casing is matched``() =
    let result =
        CSharp.CreateFile("""
    {
        "object" : []
    }
    """, Settings(ClassPrefix = "foo", ClassSuffix = System.String.Empty, Casing = Settings.None))

    let expected = "public class fooroot { public object[] @object { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let ``Uses default suffix if prefix and sufifx is set to empty string``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(ClassPrefix = System.String.Empty, ClassSuffix = System.String.Empty))

    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Fact>]
let ``Uses default suffix if prefix and sufifx is set to null``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(ClassPrefix = null, ClassSuffix = null))

    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let ``None case does not try to change casing at all``() =
    let result =
        CSharp.CreateFile("""
    {
        "FOOBAR" : []
    }
    """, Settings(Casing = Settings.None, ClassSuffix = "MODEL"))

    let expected = "public class rootMODEL { public object[] FOOBAR { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
[<Fact>]
let ``None case works for generated classes with explicit root argument``() =
    let result =
        CSharp.CreateFile("""
    {
        "foobar" : []
    }
    """, Settings(Casing = Settings.None, ClassSuffix = "model"))

    let expected = "public class rootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
[<Fact>]
let ``None case works for generated classes with explicit  suffix, root arguments``() =
    let result =
        CSharp.CreateFile("""
    {
        "foobar" : []
    }
    """, Settings(Casing = Settings.None, ClassSuffix = "model", RootObjectName = "root"))

    let expected = "public class rootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let ``None case works for generated classes with explicit prefix, suffix, root arguments``() =
    let result =
        CSharp.CreateFile("""
    {
        "foobar" : []
    }
    """, Settings(Casing = Settings.None, ClassPrefix = "x", ClassSuffix = "model", RootObjectName = "root"))

    let expected = "public class xrootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Fact>]
let ``None case works for generated classes``() =
    let result =
        CSharp.CreateFile("""
    {
        "foobar" : []
    }
    """, Settings(Casing = Settings.None))

    let expected = "public class rootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
[<Fact>]
let ``Pascal case works for generated classes with explicit root argument``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = Settings.Pascal, ClassSuffix = "model"))

    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
[<Fact>]
let ``Pascal case works for generated classes with explicit  suffix, root arguments``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = Settings.Pascal, ClassSuffix = "model", RootObjectName = "root"))

    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let ``Pascal case works for generated classes with explicit prefix, suffix, root arguments``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = Settings.Pascal, ClassPrefix = "x", ClassSuffix = "model", RootObjectName = "root"))

    let expected = "public class XRootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Fact>]
let ``Pascal case works for generated classes``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = Settings.Pascal))

    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let ``Camel case works for generated classes with explicit root argument``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = Settings.Camel, ClassSuffix = "model"))

    let expected = "public class rootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
[<Fact>]
let ``Camel case works for generated classes with explicit  suffix, root arguments``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = Settings.Camel, ClassSuffix = "model", RootObjectName = "root"))

    let expected = "public class rootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let ``Camel case works for generated classes with explicit prefix, suffix, root arguments``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = Settings.Camel, ClassPrefix = "x", ClassSuffix = "model", RootObjectName = "root"))

    let expected = "public class xRootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Fact>]
let ``Camel case works for generated classes``() =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(Casing = Settings.Camel))

    let expected = "public class rootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)


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
    arrayEntryAssertion expected result.Either.Value

[<Fact>]
let ``Camel case Object``() =
    let result = CSharp.CreateFile("""{"foo": 2}""", Settings(Casing="Camel"))
    let expected = "public decimal foo { get; set; }" |> sprintf "public class rootModel { %s }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let ``Pascal case Object``() =
    let result = CSharp.CreateFile("""{"foo": 2}""", Settings(Casing="Pascal"))
    let expected = "public decimal Foo { get; set; }"
    objectEntryAssertion expected result.Either.Value
    
[<Fact>]
let ``None case Object``() =
    let result = CSharp.CreateFile("""{"FoO": 2}""", Settings(Casing="None"))
    let expected = "public decimal FoO { get; set; }" |> sprintf "public class rootmodel { %s }"
    Assert.Equal(expected, result.Either.Value)
    
[<Fact>]
let Object() =
    let result = CSharp.CreateFile("""{"Foo": 2}""")
    let expected = "public decimal Foo { get; set; }"
    objectEntryAssertion expected result.Either.Value

[<Fact>]
let ``Object with Empty Array``() =
    let result =
        CSharp.CreateFile """ { "Foo" : [] } """ 
    let expected = "public object[] Foo { get; set; }"
    objectEntryAssertion expected result.Either.Value

[<Fact>]
let ``Object with Empty object``() =
    let result =
        CSharp.CreateFile """
    {
        "Foo" : {}
    }
    """

    let expected = "public object Foo { get; set; }"
    objectEntryAssertion expected result.Either.Value

[<Fact>]
let ``Empty Array``() =
    let result = CSharp.CreateFile("""[]""")
    let expected = "public object[] Root { get; set; }"
    arrayEntryAssertion expected result.Either.Value

[<Fact>]
let ``Empty Object``() =
    let result = CSharp.CreateFile("""{}""")
    let expected = "public object Root { get; set; }"
    arrayEntryAssertion expected result.Either.Value

[<Fact>]
let ``Array with single number``() =
    let result = CSharp.CreateFile("""[1]""")
    let expected = "public decimal[] Root { get; set; }"
    arrayEntryAssertion expected result.Either.Value

[<Fact>]
let ``Array with single string``() =
    let result = CSharp.CreateFile("""["bar"]""")
    let expected = "public string[] Root { get; set; }"
    arrayEntryAssertion expected result.Either.Value

[<Fact>]
let ``Array with numbers``() =
    let result = CSharp.CreateFile("""[1,2,3,4]""")
    let expected = "public decimal[] Root { get; set; }"
    arrayEntryAssertion expected result.Either.Value

[<Fact>]
let ``Array with strings``() =
    let result = CSharp.CreateFile("""["foo","bar"]""")
    let expected = "public string[] Root { get; set; }"
    arrayEntryAssertion expected result.Either.Value

[<Fact>]
let ``Array with strings and numbers``() =
    let result = CSharp.CreateFile("""[1,2,3,"foo"]""")
    let expected = "public object[] Root { get; set; }"
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value


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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    objectEntryAssertion expected result.Either.Value

[<Fact>]
let ``Two Objects``() =
    let result = CSharp.CreateFile """{"foo": 2, "bar": "this is a test"}"""
    let expected = "public decimal Foo { get; set; } public string Bar { get; set; }"
    objectEntryAssertion expected result.Either.Value

[<Fact>]
let ``Object with nested int array``() =
    let result = CSharp.CreateFile """{"foo": 2, "items": [1,2,3,4,5]}"""
    let expected = "public decimal Foo { get; set; } public decimal[] Items { get; set; }"
    objectEntryAssertion expected result.Either.Value

[<Fact>]
let ``Object with nested string array``() =
    let result = CSharp.CreateFile """{"foo": 2, "items": ["foo","bar", "doe"]}"""
    let expected = "public decimal Foo { get; set; } public string[] Items { get; set; }"
    objectEntryAssertion expected result.Either.Value

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
    objectEntryAssertion expected result.Either.Value

[<Fact>]
let ``Object with null value``() =
    let result = CSharp.CreateFile """{ "Foo" : null }"""
    let expected = "public object Foo { get; set; }"
    objectEntryAssertion expected result.Either.Value

[<Fact>]
let ``Object with null value and none null value``() =
    let result = CSharp.CreateFile """{ "Foo" : null, "Bar" : 1 }"""
    let expected = "public object Foo { get; set; } public decimal Bar { get; set; }"
    objectEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value

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
    arrayEntryAssertion expected result.Either.Value
