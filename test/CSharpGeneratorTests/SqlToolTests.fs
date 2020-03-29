module CSharpGeneratorTests.SqlToolTests

open Xunit
open TemplateFactory.SQL


[<Fact>]
let ``Empty object is valid`` () =
    let res = Index.foo "DoStuff" "public class Foo { public int Data { get; set; } }"
    let expected = "CREATE OR ALTER PROCEDURE DoStuff (int @data) AS\nBEGIN\nEND"
    Assert.Equal(expected, res)