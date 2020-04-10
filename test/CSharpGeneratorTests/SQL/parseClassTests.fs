module CSharpGeneratorTests.SQL.parseClassTests

open System
open TemplateFactory.SQL
open Xunit


[<Theory>]
[<InlineData("")>]
[<InlineData("x")>]
[<InlineData("foobar")>]
[<InlineData("public int { get; set; }")>]
[<InlineData("public string { get; set; }")>]
[<InlineData("public double { get; set; }")>]
let ``Invalid class syntax`` input =
    let ``exception`` = Assert.Throws<ArgumentException>(fun () -> SQL.parseClass input |> ignore)
    Assert.Equal("You must supply a CSharp class.", ``exception``.Message)

[<Fact>]
let ``Multiple classes throws not supported exception``() =
    let ``exception`` =
        Assert.Throws<NotSupportedException>(fun () ->
            SQL.parseClass "public class Foo { public class Bar {} }" |> ignore)
    Assert.Equal("Passing multiple classes is not supported.", ``exception``.Message)


[<Fact>]
let ``Single class does not throw``() =
    let result = SQL.parseClass "public class foo {}"
    Assert.NotNull result
