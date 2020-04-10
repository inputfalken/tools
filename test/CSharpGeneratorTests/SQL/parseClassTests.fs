module CSharpGeneratorTests.SQL.parseClassTests

open System
open TemplateFactory.SQL
open Xunit


[<Fact>]
let ``Empty string throws argument exception``() =
    let ``exception`` = Assert.Throws<ArgumentException>(fun () -> SQL.parseClass "" |> ignore)
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
