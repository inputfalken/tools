module CSharpGeneratorTests.SQL.parseClassTests

open System
open Sql.FromCSharp
open Xunit


[<Theory>]
[<InlineData("")>]
[<InlineData("x")>]
[<InlineData("foobar")>]
[<InlineData("public int { get; set; }")>]
[<InlineData("public string { get; set; }")>]
[<InlineData("public double { get; set; }")>]
let ``Invalid class syntax`` input =
    let ``exception`` = Assert.Throws<ArgumentException>(fun () -> parseClass input |> ignore)
    Assert.Equal("You must supply a CSharp class.", ``exception``.Message)

[<Fact>]
let ``Multiple classes throws not supported exception`` () =
    let ``exception`` =
        Assert.Throws<NotSupportedException>(fun () ->
            parseClass "public class Foo { public class Bar {} }" |> ignore)
    Assert.Equal("Passing multiple classes is not supported.", ``exception``.Message)


[<Theory>]
[<InlineData("public class Person {}")>]
[<InlineData("public class Person { public int Id { get; set; } }")>]
[<InlineData("public class Person { public int Id { get; set; } public string FirstName { get; set; } }")>]
let ``Single class does not throw`` input =
    let result = parseClass input
    Assert.NotNull result
