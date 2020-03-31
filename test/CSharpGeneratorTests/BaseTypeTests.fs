module CSharpGeneratorTests.DataTypeTests

open Xunit
open TemplateFactory.SQL


[<Fact>]
let Int () =
    let res = SQL.mapBaseTypes "int"
    Assert.Equal("int", res)
    
[<Fact>]
let Bool () =
    let res = SQL.mapBaseTypes "bool"
    Assert.Equal("bit", res)
    
[<Fact>]
let String () =
    let res = SQL.mapBaseTypes "string"
    Assert.Equal("nvarchar(max)", res)
    
[<Fact>]
let Guid () =
    let res = SQL.mapBaseTypes "GUID"
    Assert.Equal("uniqueidentifier", res)
    
[<Fact>]
let DateTime () =
    let res = SQL.mapBaseTypes "DateTime"
    Assert.Equal("datetime", res)
