module CSharpGeneratorTests.DataTypeTests

open Xunit
open TemplateFactory.SQL


[<Fact>]
let Int () =
    let res = Index.foo "DoStuff" "public class Foo { public int Member { get; set; } }"
    let expected = "CREATE OR ALTER PROCEDURE DoStuff (@Member int) AS\nBEGIN\n\nEND"
    Assert.Equal(expected, res)
    
[<Fact>]
let Bool () =
    let res = Index.foo "DoStuff" "public class Foo { public bool Member { get; set; } }"
    let expected = "CREATE OR ALTER PROCEDURE DoStuff (@Member bit) AS\nBEGIN\n\nEND"
    Assert.Equal(expected, res)
    
[<Fact>]
let String () =
    let res = Index.foo "DoStuff" "public class Foo { public string Member { get; set; } }"
    let expected = "CREATE OR ALTER PROCEDURE DoStuff (@Member nvarchar(max)) AS\nBEGIN\n\nEND"
    Assert.Equal(expected, res)
    
[<Fact>]
let Guid () =
    let res = Index.foo "DoStuff" "public class Foo { public GUID Member { get; set; } }"
    let expected = "CREATE OR ALTER PROCEDURE DoStuff (@Member uniqueidentifier) AS\nBEGIN\n\nEND"
    Assert.Equal(expected, res)
    
[<Fact>]
let DateTime () =
    let res = Index.foo "DoStuff" "public class Foo { public DateTime Member { get; set; } }"
    let expected = "CREATE OR ALTER PROCEDURE DoStuff (@Member datetime) AS\nBEGIN\n\nEND"
    Assert.Equal(expected, res)
