module Tests

open Generator
open Xunit
open Xunit

// TODO these tests should test the CSharp assembly directly and there should be separate tests for translating the CSharpSettings object.
[<Literal>]
let CamelCase = "Camel"

[<Literal>]
let PascalCase = "Pascal"

[<Literal>]
let NoneCase = "None"

[<Theory>]
[<InlineData("""{}""")>]
let ``Empty object is valid`` json =
    let result = Factory.CSharp json
    let expected = "public object RootModel { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[]""")>]
let ``Empty Array is valid`` json =
    let result = Factory.CSharp json
    let expected = "public object[] RootModel { get; set; }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""abc""")>]
let ``Passing invalid json results in an exception`` json =
    let result = Factory.CSharp json
    Assert.Null(result.Either.Value)
    Assert.NotNull(result.Either.Error)

[<Theory>]
[<InlineData("""1""", "int")>]
[<InlineData("""1.3""", "decimal")>]
[<InlineData(""" "abc" """, "string")>]
[<InlineData(""" "C34B804A-AB36-4060-88DC-E0138863E152" """, "System.Guid")>]
[<InlineData(""" "2020-12-01" """, "System.DateTime")>]
[<InlineData(""" true """, "bool")>]
[<InlineData(""" false """, "bool")>]
[<InlineData(""" null """, "object")>]
let ``Entry with value`` json expected =
    let result = Factory.CSharp json
    let expected = sprintf "public %s RootModel { get; set; }" expected
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("Foo", "")>]
[<InlineData("", "Foo")>]
let ``Allow classPrefix to be empty if classSuffix is set`` classPrefix classSuffix =
    let result = Factory.CSharp("""{ "FooBar" : [] }""", CSharpSettings(ClassPrefix = classPrefix, ClassSuffix = classSuffix))
    let expected = sprintf "public class %sRoot%s { public object[] FooBar { get; set; } }" classPrefix classSuffix
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData(NoneCase, "public class rootmodel { public object[] @object { get; set; } }")>]
[<InlineData(PascalCase, "public class RootModel { public object[] Object { get; set; } }")>]
let ``Handle reserved words`` casing expected =
    let result = Factory.CSharp("""{ "object" : [] }""", CSharpSettings(PropertyCasing = casing, ClassCasing = casing))
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("", "")>]
[<InlineData(null, null)>]
[<InlineData(null, "")>]
[<InlineData("", null)>]
let ``Uses default classSuffix if classPrefix and sufifx is set to empty string or null`` classPrefix classSuffix =
    let result =
        Factory.CSharp("""
    {
        "FooBar" : []
    }
    """, CSharpSettings(ClassPrefix = classPrefix, ClassSuffix = classSuffix))

    let expected = "public class RootModel { public object[] FooBar { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"InnerClass": {"FooBar": 2}}""", CamelCase, CamelCase,
             "public class rootModel { public class innerClassModel { public int fooBar { get; set; } } public innerClassModel innerClass { get; set; } }")>]
[<InlineData("""{"InnerClass": {"FooBar": 2}}""", null, CamelCase,
             "public class RootModel { public class InnerClassModel { public int fooBar { get; set; } } public InnerClassModel innerClass { get; set; } }")>]
[<InlineData("""{"InnerClass": {"FooBar": 2}}""", CamelCase, null,
             "public class rootModel { public class innerClassModel { public int FooBar { get; set; } } public innerClassModel InnerClass { get; set; } }")>]
let ``Casing Camel `` json classCasing propertyCasing expected =
    let result = Factory.CSharp(json, CSharpSettings(PropertyCasing = propertyCasing, ClassCasing = classCasing))
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"InnerClass": {"FooBar": 2}}""", CamelCase, CamelCase,
             "public class xRootModel { public class xInnerClassModel { public int fooBar { get; set; } } public xInnerClassModel innerClass { get; set; } }")>]
[<InlineData("""{"InnerClass": {"FooBar": 2}}""", null, CamelCase,
             "public class XRootModel { public class XInnerClassModel { public int fooBar { get; set; } } public XInnerClassModel innerClass { get; set; } }")>]
[<InlineData("""{"InnerClass": {"FooBar": 2}}""", CamelCase, null,
             "public class xRootModel { public class xInnerClassModel { public int FooBar { get; set; } } public xInnerClassModel InnerClass { get; set; } }")>]
[<InlineData("""{}""", CamelCase, CamelCase, "public object xRootModel { get; set; }")>]
[<InlineData("""{}""", CamelCase, null, "public object XRootModel { get; set; }")>]
[<InlineData("""{}""", null, CamelCase, "public object xRootModel { get; set; }")>]
[<InlineData("""[]""", CamelCase, CamelCase, "public object[] xRootModel { get; set; }")>]
[<InlineData("""[]""", CamelCase, null, "public object[] XRootModel { get; set; }")>]
[<InlineData("""[]""", null, CamelCase, "public object[] xRootModel { get; set; }")>]
let ``Casing Camel  works with arguments classSuffix, root and classPrefix`` json classCasing propertyCasing expected =
    let result =
        Factory.CSharp
            (json,
             CSharpSettings
                 (ClassCasing = classCasing, PropertyCasing = propertyCasing, ClassPrefix = "x", ClassSuffix = "model",
                  RootObjectName = "root"))
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"INNERCLASS": {"FOOBAR": 2}}""", NoneCase, NoneCase,
             "public class xrootmodel { public class xINNERCLASSmodel { public int FOOBAR { get; set; } } public xINNERCLASSmodel INNERCLASS { get; set; } }")>]
[<InlineData("""{"innerClass": {"fooBar": 2}}""", null, NoneCase,
             "public class XRootModel { public class XInnerClassModel { public int fooBar { get; set; } } public XInnerClassModel innerClass { get; set; } }")>]
[<InlineData("""{"innerClass": {"fooBar": 2}}""", NoneCase, null,
             "public class xrootmodel { public class xinnerClassmodel { public int FooBar { get; set; } } public xinnerClassmodel InnerClass { get; set; } }")>]
[<InlineData("""{}""", NoneCase, NoneCase, "public object xrootmodel { get; set; }")>]
[<InlineData("""{}""", NoneCase, null, "public object XRootModel { get; set; }")>]
[<InlineData("""{}""", null, NoneCase, "public object xrootmodel { get; set; }")>]
[<InlineData("""[]""", NoneCase, NoneCase, "public object[] xrootmodel { get; set; }")>]
[<InlineData("""[]""", NoneCase, null, "public object[] XRootModel { get; set; }")>]
[<InlineData("""[]""", null, NoneCase, "public object[] xrootmodel { get; set; }")>]
let ``Casing None works with arguments classSuffix, root and classPrefix`` json classCasing propertyCasing expected =
    let result =
        Factory.CSharp
            (json,
             CSharpSettings
                 (ClassCasing = classCasing, PropertyCasing = propertyCasing, ClassPrefix = "x", ClassSuffix = "model",
                  RootObjectName = "root"))
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"innerClass": {"FooBar": 2}}""", PascalCase, PascalCase)>]
[<InlineData("""{"innerClass": {"FooBar": 2}}""", null, PascalCase)>]
[<InlineData("""{"innerClass": {"FooBar": 2}}""", PascalCase, null)>]
[<InlineData("""{"innerClass": {"FooBar": 2}}""", null, null)>]
let ``Casing Pascal`` json classCasing propertyCasing =
    let expected =
        "public class RootModel { public class InnerClassModel { public int FooBar { get; set; } } public InnerClassModel InnerClass { get; set; } }"
    let result = Factory.CSharp(json, CSharpSettings(PropertyCasing = propertyCasing, ClassCasing = classCasing))
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"innerClass": {"fooBar": 2}}""", PascalCase, PascalCase,
             "public class XRootModel { public class XInnerClassModel { public int FooBar { get; set; } } public XInnerClassModel InnerClass { get; set; } }")>]
[<InlineData("""{"innerClass": {"fooBar": 2}}""", null, null,
             "public class XRootModel { public class XInnerClassModel { public int FooBar { get; set; } } public XInnerClassModel InnerClass { get; set; } }")>]
[<InlineData("""{"innerClass": {"fooBar": 2}}""", null, PascalCase,
             "public class XRootModel { public class XInnerClassModel { public int FooBar { get; set; } } public XInnerClassModel InnerClass { get; set; } }")>]
[<InlineData("""{"innerClass": {"fooBar": 2}}""", PascalCase, null,
             "public class XRootModel { public class XInnerClassModel { public int FooBar { get; set; } } public XInnerClassModel InnerClass { get; set; } }")>]
[<InlineData("""{}""", PascalCase, PascalCase, "public object XRootModel { get; set; }")>]
[<InlineData("""{}""", PascalCase, null, "public object XRootModel { get; set; }")>]
[<InlineData("""{}""", null, PascalCase, "public object XRootModel { get; set; }")>]
[<InlineData("""[]""", PascalCase, PascalCase, "public object[] XRootModel { get; set; }")>]
[<InlineData("""[]""", PascalCase, null, "public object[] XRootModel { get; set; }")>]
[<InlineData("""[]""", null, PascalCase, "public object[] XRootModel { get; set; }")>]
let ``Casing Pascal  works with arguments classSuffix, root and classPrefix`` json classCasing propertyCasing expected =
    let result =
        Factory.CSharp
            (json,
             CSharpSettings
                 (ClassCasing = classCasing, PropertyCasing = propertyCasing, ClassPrefix = "x", ClassSuffix = "model",
                  RootObjectName = "root"))
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{"INNER_CLASS": {"FOO_BAR": 2}}""", NoneCase, NoneCase,
             "public class rootmodel { public class INNER_CLASSmodel { public int FOO_BAR { get; set; } } public INNER_CLASSmodel INNER_CLASS { get; set; } }")>]
[<InlineData("""{"INNER_CLASS": {"FOO_BAR": 2}}""", null, NoneCase,
             "public class RootModel { public class InnerClassModel { public int FOO_BAR { get; set; } } public InnerClassModel INNER_CLASS { get; set; } }")>]
[<InlineData("""{"INNER_CLASS": {"FOO_BAR": 2}}""", NoneCase, null,
             "public class rootmodel { public class INNER_CLASSmodel { public int FooBar { get; set; } } public INNER_CLASSmodel InnerClass { get; set; } }")>]
let ``Casing none`` json classCasing propertyCasing expected =
    let result = Factory.CSharp(json, CSharpSettings(PropertyCasing = propertyCasing, ClassCasing = classCasing))
    Assert.Equal(expected, result.Either.Value)


[<Theory>]
[<InlineData("""[ { "tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ] }, { "tags": [ "aliqua" ], "friends": [ { "id": 0, "name": "Kristy Calhoun" } ] } ]""")>]
let ``Two array Objects next to each other`` json =
    let result = Factory.CSharp json
    let expected =
        "public class RootModel { public string[] Tags { get; set; } public class FriendsModel { public int Id { get; set; } public string Name { get; set; } } public FriendsModel[] Friends { get; set; } }"

    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ], "john" : "" }, { "tags": [ "aliqua" ], "friends": [ { "id": 0, "name": "Kristy Calhoun" } ], "doe" : "" } ]""")>]
[<InlineData("""[ { "tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ], "john" : "" }, { "tags": [ "aliqua" ], "doe" : "" } ]""")>]
[<InlineData("""[ { "tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ], "john" : "" }, { "tags": [ "aliqua" ], "friends": [ { "id": 0 } ], "doe" : "" } ]""")>]
[<InlineData("""[ { "Tags": [ "non" ], "friends": [ { "id": 0, "name": "Henrietta Tillman" } ], "john" : "" }, { "tags": [ "aliqua" ], "friends": [ { "id": 0 } ], "doe" : "" } ]""")>]
let ``Two array Objects next to each other with different properties should expand properties`` json =
    let result = Factory.CSharp json
    let expected =
        "public class RootModel { public string[] Tags { get; set; } public class FriendsModel { public int Id { get; set; } public string Name { get; set; } } public FriendsModel[] Friends { get; set; } public string John { get; set; } public string Doe { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "Foo" : [] }""")>]
let ``Object with Empty Array`` json =
    let result = Factory.CSharp json
    let expected = "public class RootModel { public object[] Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "Foo" : {} }""")>]
[<InlineData("""{ "Foo" : null }""")>]
let ``Object with Empty or null object`` json =
    let result = Factory.CSharp json
    let expected = "public class RootModel { public object Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""["2020-01-01", "3/1/2020 9:05:20 PM"]""", "System.DateTime")>]
[<InlineData("""["2020-01-01"]""", "System.DateTime")>]
[<InlineData("""["3/1/2020 9:05:20 PM"]""", "System.DateTime")>]
[<InlineData("""["85a34f88-d688-46ce-9989-89d7a8698de7", "79607c8b-74b8-4293-8e01-ca2f77bf76a7"]""", "System.Guid")>]
[<InlineData("""["85a34f88-d688-46ce-9989-89d7a8698de7"]""", "System.Guid")>]
[<InlineData("""["foo", "bar"]""", "string")>]
[<InlineData("""["foo"]""", "string")>]
[<InlineData("""[1, 10]""", "int")>]
[<InlineData("""[1]""", "int")>]
[<InlineData("""[1.3, 1.5]""", "decimal")>]
[<InlineData("""[1.2]""", "decimal")>]
[<InlineData("""[null, null]""", "object")>]
[<InlineData("""[null, {}]""", "object")>]
[<InlineData("""[null]""", "object")>]
[<InlineData("""[{}, {}]""", "object")>]
[<InlineData("""[{}]""", "object")>]
let ``Entry as with array`` json ``type`` =
    let result = Factory.CSharp json
    let expected = sprintf "public %s[] RootModel { get; set; }" ``type``
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""["2020-01-01", "3/1/2020 9:05:20 PM", null]""", "System.DateTime?")>]
[<InlineData("""["2020-01-01", null, "3/1/2020 9:05:20 PM"]""", "System.DateTime?")>]
[<InlineData("""["2020-01-01", null]""", "System.DateTime?")>]
[<InlineData("""["3/1/2020 9:05:20 PM", null]""", "System.DateTime?")>]
[<InlineData("""["85a34f88-d688-46ce-9989-89d7a8698de7", "79607c8b-74b8-4293-8e01-ca2f77bf76a7", null]""",
             "System.Guid?")>]
[<InlineData("""["85a34f88-d688-46ce-9989-89d7a8698de7", null,"79607c8b-74b8-4293-8e01-ca2f77bf76a7"]""", "System.Guid?")>]
[<InlineData("""["85a34f88-d688-46ce-9989-89d7a8698de7", null]""", "System.Guid?")>]
[<InlineData("""["foo", "bar", null]""", "string")>]
[<InlineData("""["foo", null, "bar"]""", "string")>]
[<InlineData("""["foo", null]""", "string")>]
[<InlineData("""[1, 10, null]""", "int?")>]
[<InlineData("""[1, null]""", "int?")>]
[<InlineData("""[1,null, 10]""", "int?")>]
[<InlineData("""[null,1, 10]""", "int?")>]
[<InlineData("""[1.3, 10.5, null]""", "decimal?")>]
[<InlineData("""[1.7, null]""", "decimal?")>]
[<InlineData("""[1.9,null, 10.25]""", "decimal?")>]
[<InlineData("""[null,1.35, 10.40]""", "decimal?")>]
[<InlineData("""[null ,"2020-01-01", "3/1/2020 9:05:20 PM"]""", "System.DateTime?")>]
[<InlineData("""[null ,"85a34f88-d688-46ce-9989-89d7a8698de7", "79607c8b-74b8-4293-8e01-ca2f77bf76a7"]""",
             "System.Guid?")>]
[<InlineData("""[null, {}, null]""", "object")>]
[<InlineData("""[null,"foo", "bar"]""", "string")>]
[<InlineData("""[{}, null, {}]""", "object")>]
[<InlineData("""[{}, null]""", "object")>]
[<InlineData("""[{}, {}, null]""", "object")>]
let ``Array with basetypes mixed with null`` json ``type`` =
    let result = Factory.CSharp json
    let expected = sprintf "public %s[] RootModel { get; set; }" ``type``
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""[ { "foo": 1, "bar": "Hi" }, { "foo": 2, "bar": "Hi" } ] """,
             "public int Foo { get; set; } public string Bar { get; set; }")>]
[<InlineData("""[ { "bar": [1, "foo"] }, { "bar": [1, 2] } ] """, "public object[] Bar { get; set; }")>]
[<InlineData("""[ { "bar": [1, 2] }, { "bar": [1, "foo"] } ]""", "public object[] Bar { get; set; }")>]
[<InlineData("""[ { "Foo" : "foobar", "John" : 2 }, { "Bar" : null, "Doe" : 2 } ]""",
             "public string Foo { get; set; } public int? John { get; set; } public object Bar { get; set; } public int? Doe { get; set; }")>]
[<InlineData("""[ { "Foo" : null, "John" : null }, { "Bar" : null, "Doe" : null }, { "Foo" : "foo", "John" : "doo", "Bar" : 2, "Doe" : 4 } ]""",
             "public string Foo { get; set; } public string John { get; set; } public int? Bar { get; set; } public int? Doe { get; set; }")>]
[<InlineData("""[ { "Foo" : "foobar", "John" : 2 }, { "Bar" : null, "Doe" : 2 }, { "Foo" : null, "John" : 2 }, { "Bar" : null, "Doe" : 2 } ]""",
             "public string Foo { get; set; } public int? John { get; set; } public object Bar { get; set; } public int? Doe { get; set; }")>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2 }, { "Foo" : 4, "Bar" : 2 } ]""",
             "public int? Foo { get; set; } public int Bar { get; set; }")>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2 }, { "Foo" : "foobar", "Bar" : 2 } ]""",
             "public string Foo { get; set; } public int Bar { get; set; }")>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2, "test" : 2 }, { "Foo" : "foobar", "Bar" : 2 } ]""",
             "public string Foo { get; set; } public int Bar { get; set; } public int? Test { get; set; }")>]
[<InlineData("""[ { "Foo" : null, "Bar" : 2 }, { "Foo" : "foobar", "Bar" : 2, "test" : 2 } ]""",
             "public string Foo { get; set; } public int Bar { get; set; } public int? Test { get; set; }")>]
let ``Array with objects`` json csharp =
    let result = Factory.CSharp json
    let expected = sprintf "public class RootModel { %s }" csharp
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("""{ "aps": { "alert":"foobar" } }""",
             "public class ApsModel { public string Alert { get; set; } } public ApsModel Aps { get; set; }")>]
[<InlineData("""{"Foo": 2}""", "public int Foo { get; set; }")>]
[<InlineData("""{"foo": 2, "bar": "this is a test"}""",
             "public int Foo { get; set; } public string Bar { get; set; }")>]
[<InlineData("""{"foo": 2, "items": [1,2,3,4,5]}""",
             "public int Foo { get; set; } public int[] Items { get; set; }")>]
[<InlineData("""{"foo": 2, "items": ["foo","bar", "doe"]}""",
             "public int Foo { get; set; } public string[] Items { get; set; }")>]
[<InlineData("""{ "Models": [ { "foo": 1 }, { "foo": 2 } ]} """,
             "public class ModelsModel { public int Foo { get; set; } } public ModelsModel[] Models { get; set; }")>]
[<InlineData("""{ "Foo" : null }""", "public object Foo { get; set; }")>]
[<InlineData("""{ "Foo" : null, "Bar" : 1 }""", "public object Foo { get; set; } public int Bar { get; set; }")>]
[<InlineData(""" [ { "Foo" : "foobar", "Bar" : 2 }, { "Foo" : null, "Bar" : 2 } ] """,
             "public string Foo { get; set; } public int Bar { get; set; }")>]
let ``Entry with object`` json csharp =
    let result = Factory.CSharp json
    let expected = sprintf "public class RootModel { %s }" csharp
    Assert.Equal(expected, result.Either.Value)


[<Literal>]
let invalidTypeNameErrorMessage = "Member names can only start with letters."
[<Theory>]
[<InlineData("""{"1337": {}}""", invalidTypeNameErrorMessage)>]
[<InlineData("""{"Foo": {"1337": {"bar": 2}}}""", invalidTypeNameErrorMessage)>]
let ``Type names or member names who starts with numbers result in error`` json expected =
    let result = Factory.CSharp json
    Assert.Equal(expected, result.Either.Error.Message)
    
[<Theory>]
[<InlineData("""{}""", invalidTypeNameErrorMessage)>]
[<InlineData("""{"Foo": {}}""", invalidTypeNameErrorMessage)>]
let ``Class classPrefix with number result in error`` json expected =
    let result = Factory.CSharp(json, CSharpSettings(ClassPrefix = "0"))
    Assert.Equal(expected, result.Either.Error.Message)

[<Theory>]
[<InlineData("""{"Nested": {"Root": {"foo":2}}}""",
             "public class RootModel { public class NestedModel { public class NestedRootModel { public int Foo { get; set; } } public NestedRootModel Root { get; set; } } public NestedModel Nested { get; set; } }")>]
[<InlineData("""{"Nested": {"Nested": {"Root": {"foo":2}}}}""",
             "public class RootModel { public class NestedModel { public class NestedNestedModel { public class NestedNestedRootModel { public int Foo { get; set; } } public NestedNestedRootModel Root { get; set; } } public NestedNestedModel Nested { get; set; } } public NestedModel Nested { get; set; } }")>]
[<InlineData("""{"data":{"name":"","token":"","user":{"data":{"name":"","email":""}},"quota":{"data":{"usage":25,"limit":100,"next_reset":"2020-04-01T00:00:00+02:00"}}}}""",
             "public class RootModel { public class DataModel { public string Name { get; set; } public string Token { get; set; } public class UserModel { public class UserDataModel { public string Name { get; set; } public string Email { get; set; } } public UserDataModel Data { get; set; } } public UserModel User { get; set; } public class QuotaModel { public class QuotaDataModel { public int Usage { get; set; } public int Limit { get; set; } public System.DateTime NextReset { get; set; } } public QuotaDataModel Data { get; set; } } public QuotaModel Quota { get; set; } } public DataModel Data { get; set; } }")>]
let ``Resolve class names when it's the same name as their enclosing type`` json expected =
    let result = Factory.CSharp (json)
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
    let result = Factory.CSharp json
    let expected = "public class RootModel { public string Foo { get; set; } }"
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData(""" { "orgnr": "", "name": "", "children": { "orgnr": "", "name": "", "children": { "orgnr": "", "name": "", "children": { "orgnr": "", "name": "" } } } } """,
             "public class RootModel { public string Orgnr { get; set; } public string Name { get; set; } public class ChildrenModel { public string Orgnr { get; set; } public string Name { get; set; } public class ChildrenChildrenModel { public string Orgnr { get; set; } public string Name { get; set; } public class ChildrenChildrenChildrenModel { public string Orgnr { get; set; } public string Name { get; set; } } public ChildrenChildrenChildrenModel Children { get; set; } } public ChildrenChildrenModel Children { get; set; } } public ChildrenModel Children { get; set; } }")>]
let ``Nested identical children with Object`` json expected =
    let result = Factory.CSharp json
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData(""" { "orgnr": "", "name": "", "children": [ { "orgnr": "", "name": "", "children": [ { "orgnr": "", "name": "", "children": [ { "orgnr": "", "name": "" } ] } ] } ] } """,
             "public class RootModel { public string Orgnr { get; set; } public string Name { get; set; } public class ChildrenModel { public string Orgnr { get; set; } public string Name { get; set; } public class ChildrenChildrenModel { public string Orgnr { get; set; } public string Name { get; set; } public class ChildrenChildrenChildrenModel { public string Orgnr { get; set; } public string Name { get; set; } } public ChildrenChildrenChildrenModel[] Children { get; set; } } public ChildrenChildrenModel[] Children { get; set; } } public ChildrenModel[] Children { get; set; } }")>]
let ``Nested identical children with Array`` json expected =
    let result = Factory.CSharp json
    Assert.Equal(expected, result.Either.Value)

[<Theory>]
[<InlineData("[1.50,1,2,3,4]", "decimal")>]
[<InlineData("[1,2,1.50,3,4]", "decimal")>]
[<InlineData("[1,null,1.50,3,4]", "decimal?")>]
[<InlineData("[null,3,1.50,3,4]", "decimal?")>]
[<InlineData("[1,3,1.50,3,null]", "decimal?")>]
let ``Integers transforms into decimal if a number with decimals occur`` json ``type``  =
    let result = Factory.CSharp json
    let expected = sprintf "public %s[] RootModel { get; set; }" ``type``
    Assert.Equal(expected, result.Either.Value)
