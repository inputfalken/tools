module CSharpGeneratorTests.DataTypeTests

open Xunit
open TemplateFactory.SQL.SQL


[<Fact>]
let Int() =
    let res = SqlDataType.Int.ToString()
    Assert.Equal("int", res)

[<Fact>]
let Bool() =
    let res = SqlDataType.Bit.ToString()
    Assert.Equal("bit", res)

[<Theory>]
[<InlineDataAttribute(10)>]
[<InlineDataAttribute(20)>]
[<InlineDataAttribute(200)>]
let String length =
    let res = (SqlDataType.Nvarchar <| NVarCharArgument.Number length).ToString()
    let expected = sprintf "nvarchar(%d)" length
    Assert.Equal(expected, res)

[<Fact>]
let StringMax() =
    let res = (SqlDataType.Nvarchar <| NVarCharArgument.Max).ToString()
    let expected = sprintf "nvarchar(max)"
    Assert.Equal(expected, res)

[<Fact>]
let Guid() =
    let res = SqlDataType.UniqueIdentifier.ToString()
    Assert.Equal("uniqueidentifier", res)

[<Fact>]
let DateTime() =
    let res = SqlDataType.DateTime.ToString()
    Assert.Equal("datetime", res)
