module Tests

open CSharpGenerator
open CSharpGenerator.Arguments
open Common.Casing
open Xunit

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

[<Theory>]
[<InlineData("Foo", "")>]
[<InlineData("", "Foo")>]
let ``Allow prefix to be empty if suffix is set`` prefix suffix =
    let result = CSharp.CreateFile("""{ "FooBar" : [] }""", Settings(ClassPrefix = prefix, ClassSuffix = suffix))
    let expected = sprintf "public class %sRoot%s { public object[] FooBar { get; set; } }" prefix suffix
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("None", "public class rootmodel { public object[] @object { get; set; } }")>]
[<InlineData("Pascal", "public class RootModel { public object[] Object { get; set; } }")>]
let ``Handle reserved words`` casing expected =
    let result = CSharp.CreateFile("""{ "object" : [] }""", Settings(Casing = casing))
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("", "")>]
[<InlineData(null, null)>]
[<InlineData(null, "")>]
[<InlineData("", null)>]
let ``Uses default suffix if prefix and sufifx is set to empty string or null`` prefix suffix =
    let result =
        CSharp.CreateFile("""
    {
        "FooBar" : []
    }
    """, Settings(ClassPrefix = prefix, ClassSuffix = suffix))

    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FOOBAR" : [] }""")>]
let ``None case does not try to change casing at all`` json =
    let result = CSharp.CreateFile(json, Settings(Casing = Casing.NoneCase, ClassSuffix = "MODEL"))
    let expected = "public class rootMODEL { public object[] FOOBAR { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "foobar" : [] }""")>]
let ``None case works for generated classes with explicit root argument`` json =
    let result = CSharp.CreateFile(json, Settings(Casing = Casing.NoneCase, ClassSuffix = "model"))
    let expected = "public class rootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "foobar" : [] }""")>]
let ``None case works for generated classes with explicit  suffix, root arguments`` json =
    let result =
        CSharp.CreateFile(json, Settings(Casing = Casing.NoneCase, ClassSuffix = "model", RootObjectName = "root"))

    let expected = "public class rootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "foobar" : [] }""")>]
let ``None case works for generated classes with explicit prefix, suffix, root arguments`` json =
    let result =
        CSharp.CreateFile
            (json, Settings(Casing = Casing.NoneCase, ClassPrefix = "x", ClassSuffix = "model", RootObjectName = "root"))
    let expected = "public class xrootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "foobar" : [] }""")>]
let ``None case works for generated classes`` json =
    let result =
        CSharp.CreateFile(json, Settings(Casing = Casing.NoneCase))

    let expected = "public class rootmodel { public object[] foobar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FooBar" : [] }""")>]
let ``Pascal case works for generated classes with explicit root argument`` json =
    let result = CSharp.CreateFile(json, Settings(Casing = Casing.PascalCase, ClassSuffix = "model"))
    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FooBar" : [] }""")>]
let ``Pascal case works for generated classes with explicit  suffix, root arguments`` json =
    let result =
        CSharp.CreateFile(json, Settings(Casing = Casing.PascalCase, ClassSuffix = "model", RootObjectName = "root"))
    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FooBar" : [] }""")>]
let ``Pascal case works for generated classes with explicit prefix, suffix, root arguments`` json =
    let result =
        CSharp.CreateFile
            (json,
             Settings(Casing = Casing.PascalCase, ClassPrefix = "x", ClassSuffix = "model", RootObjectName = "root"))
    let expected = "public class XRootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FooBar" : [] }""")>]
let ``Pascal case works for generated classes`` json =
    let result = CSharp.CreateFile(json, Settings(Casing = Casing.PascalCase))
    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FooBar" : [] }""")>]
let ``Camel case works for generated classes with explicit root argument`` json =
    let result = CSharp.CreateFile(json, Settings(Casing = Casing.CamelCase, ClassSuffix = "model"))
    let expected = "public class rootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FooBar" : [] }""")>]
let ``Camel case works for generated classes with explicit  suffix, root arguments`` json =
    let result =
        CSharp.CreateFile(json, Settings(Casing = Casing.CamelCase, ClassSuffix = "model", RootObjectName = "root"))
    let expected = "public class rootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FooBar" : [] }""")>]
let ``Camel case works for generated classes with explicit prefix, suffix, root arguments`` json =
    let result =
        CSharp.CreateFile
            (json,
             Settings(Casing = Casing.CamelCase, ClassPrefix = "x", ClassSuffix = "model", RootObjectName = "root"))
    let expected = "public class xRootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "FooBar" : [] }""")>]
let ``Camel case works for generated classes`` json =
    let result = CSharp.CreateFile(json, Settings(Casing = Casing.CamelCase))
    let expected = "public class rootModel { public object[] fooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"foo": 2}""")>]
let ``Camel case Object`` json =
    let result = CSharp.CreateFile(json, Settings(Casing = "Camel"))
    let expected = "public decimal foo { get; set; }" |> sprintf "public class rootModel { %s }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"foo": 2}""")>]
let ``Pascal case Object`` json =
    let result = CSharp.CreateFile(json, Settings(Casing = "Pascal"))
    Assert.Equal("public class RootModel { public decimal Foo { get; set; } }", result.Either.Value)

[<Theory>]
[<InlineData("""[ { "tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ] }, { "tags": [ "aliqua" ], "friends": [ { "id": 0, "name": "Kristy Calhoun" } ] } ]""")>]
let ``Two array Objects next to each other`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public string[] Tags { get; set; } public class FriendsModel { public decimal Id { get; set; } public string Name { get; set; } } public FriendsModel[] Friends { get; set; } }"

    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ], "john" : "" }, { "tags": [ "aliqua" ], "friends": [ { "id": 0, "name": "Kristy Calhoun" } ], "doe" : "" } ]""")>]
[<InlineData("""[ { "tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ], "john" : "" }, { "tags": [ "aliqua" ], "doe" : "" } ]""")>]
[<InlineData("""[ { "tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ], "john" : "" }, { "tags": [ "aliqua" ], "friends": [ { "id": 0 } ], "doe" : "" } ]""")>]
let ``Two array Objects next to each other with different properties should expand properties`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public string[] Tags { get; set; } public class FriendsModel { public decimal Id { get; set; } public string Name { get; set; } } public FriendsModel[] Friends { get; set; } public string John { get; set; } public string Doe { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"Foo": 2}""")>]
let Object json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public decimal Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "Foo" : [] }""")>]
let ``Object with Empty Array`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public object[] Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "Foo" : {} }""")>]
[<InlineData("""{ "Foo" : null }""")>]
let ``Object with Empty or null object`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public object Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{}""")>]
let ``Empty object`` json =
    let result = CSharp.CreateFile json
    let expected = "public object Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[]""")>]
let ``Empty Array`` json =
    let result = CSharp.CreateFile json
    let expected = "public object[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[1]""")>]
let ``Array with single number`` json =
    let result = CSharp.CreateFile json
    let expected = "public decimal[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""["bar"]""")>]
let ``Array with single string`` json =
    let result = CSharp.CreateFile json
    let expected = "public string[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[1,2,3,4]""")>]
let ``Array with numbers`` json =
    let result = CSharp.CreateFile json
    let expected = "public decimal[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""["foo","bar"]""")>]
let ``Array with strings`` json =
    let result = CSharp.CreateFile json
    let expected = "public string[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[1,2,3,"foo"]""")>]
let ``Array with strings and numbers`` json =
    let result = CSharp.CreateFile json
    let expected = "public object[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "foo": 1 }, { "foo": 2 } ]""")>]
let ``Array with object with decimal property`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public decimal Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "foo": 1, "bar": "Hi" }, { "foo": 2, "bar": "Hi" } ] """)>]
let ``Array with object with decimal and string properties`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public decimal Foo { get; set; } public string Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "bar": [1, "foo"] }, { "bar": [1, 2] } ] """)>]
let ``Array with object with array property with strings mixed items`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public object[] Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "bar": [1, 2] }, { "bar": [1, "foo"] } ]""")>]
let ``Array with object with array property with strings mixed items reversed order`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public object[] Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "aps": { "alert":"tit}" } }""")>]
let ``Object with nested string`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public class ApsModel { public string Alert { get; set; } } public ApsModel Aps { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"foo": 2, "bar": "this is a test"}""")>]
let ``Two Objects`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public decimal Foo { get; set; } public string Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"foo": 2, "items": [1,2,3,4,5]}""")>]
let ``Object with nested int array`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public decimal Foo { get; set; } public decimal[] Items { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"foo": 2, "items": ["foo","bar", "doe"]}""")>]
let ``Object with nested string array`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public decimal Foo { get; set; } public string[] Items { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "Models": [ { "foo": 1 }, { "foo": 2 } ]} """)>]
let ``Object with object array`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public class ModelsModel { public decimal Foo { get; set; } } public ModelsModel[] Models { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "Foo" : null }""")>]
let ``Object with null value`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public object Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "Foo" : null, "Bar" : 1 }""")>]
let ``Object with null value and none null value`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public object Foo { get; set; } public decimal Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData(""" [ "foo", "bar", "null" ] """)>]
let ``Array with strings that are mixed with null values`` json =
    let result = CSharp.CreateFile json
    let expected = "public string[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData(""" [ 1, 2, null, 4 ] """)>]
let ``Array with numbers that are mixed with null values`` json =
    let result = CSharp.CreateFile json
    let expected = "public decimal?[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData(""" [ "foo", 2, 4 ] """)>]
let ``Array mixed with strings and numbers`` json =
    let result = CSharp.CreateFile json
    let expected = "public object[] Root { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "Foo" : "foobar", "John" : 2 }, { "Bar" : null, "Doe" : 2 } ]""")>]
let ``Array with missmatched properties`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal? John { get; set; } public object Bar { get; set; } public decimal? Doe { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "Foo" : null, "John" : null }, { "Bar" : null, "Doe" : null }, { "Foo" : "foo", "John" : "doo", "Bar" : 2, "Doe" : 4 } ]""")>]
let ``Array with without values first`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public string Foo { get; set; } public string John { get; set; } public decimal? Bar { get; set; } public decimal? Doe { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "Foo" : "foobar", "John" : 2 }, { "Bar" : null, "Doe" : 2 }, { "Foo" : null, "John" : 2 }, { "Bar" : null, "Doe" : 2 } ]""")>]
let ``Array with objects with two structures`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal? John { get; set; } public object Bar { get; set; } public decimal? Doe { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2 }, { "Foo" : 4, "Bar" : 2 } ]""")>]
let ``Array with objects with properties whose values are mixed with nullable value types`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public decimal? Foo { get; set; } public decimal Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2 }, { "Foo" : 4, "Bar" : 2 } ]""")>]
let ``Array with objects with properties whose values are mixed with nullable value types reversed`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public decimal? Foo { get; set; } public decimal Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2 }, { "Foo" : "foobar", "Bar" : 2 } ]""")>]
let ``Array with objects with properties whose values are mixed with null`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public string Foo { get; set; } public decimal Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2, "test" : 2 }, { "Foo" : "foobar", "Bar" : 2 } ]""")>]
let ``Array with objects with different amount of properties reversed`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal Bar { get; set; } public decimal? Test { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2 }, { "Foo" : "foobar", "Bar" : 2, "test" : 2 } ]""")>]
let ``Array with objects with different amount of properties`` json =
    let result = CSharp.CreateFile json
    let expected =
        "public class RootModel { public string Foo { get; set; } public decimal Bar { get; set; } public decimal? Test { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"Nested": {"Root": {"foo":2}}}""")>]
[<InlineData("""{"Nested": {"Root": []}}""")>]
[<InlineData("""{"Nested": {"Root": 1}}""")>]
let ``Member names is the same as their enclosing type`` json =
    let result = CSharp.CreateFile json
    let expected = "Member names cannot be the same as their enclosing type"
    Assert.Equal(expected, result.Either.Error.Message)

[<Theory>]
[<InlineData("""[{"foo":"bar"}, {"abc": 123}]""")>]
let ``Array with object does not leave trailing array`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public string Foo { get; set; } public decimal? Abc { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[{}, {"foo":"bar"}]""")>]
[<InlineData("""[{"foo":"bar"}, {}]""")>]
[<InlineData("""[{}, {"foo":"bar"}, {}]""")>]
[<InlineData("""[{}, {}, {"foo":"bar"}, {}]""")>]
[<InlineData("""[{}, {}, {"foo":"bar"}, {}, {}]""")>]
[<InlineData("""[{}, {"foo":"bar"}, {}, {}, {}]""")>]
[<InlineData("""[{}, {}, {}, {"foo":"bar"}, {}]""")>]
[<InlineData("""[null, {"foo":"bar"}]""")>]
[<InlineData("""[{"foo":"bar"}, null]""")>]
[<InlineData("""[null, {"foo":"bar"}, null]""")>]
[<InlineData("""[null, null, {"foo":"bar"}, null]""")>]
[<InlineData("""[null, null, {"foo":"bar"}, null, null]""")>]
[<InlineData("""[null, {"foo":"bar"}, null, null, null]""")>]
[<InlineData("""[null, null, null, {"foo":"bar"}, null]""")>]
let ``Array with empty object or null should not resolve in object array`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public string Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData(""" [ { "Foo" : "foobar", "Bar" : 2 }, { "Foo" : null, "Bar" : 2 } ] """)>]
let ``Array with objects with properties whose values are mixed with null reversed`` json =
    let result = CSharp.CreateFile json
    let expected = "public class RootModel { public string Foo { get; set; } public decimal Bar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)
