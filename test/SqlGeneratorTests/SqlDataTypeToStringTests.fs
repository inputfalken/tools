module SqlGeneratorTests.DataTypeTests

open Xunit
open Sql.Index


[<Fact>]
let Float () =
    let res = SqlDataType.Float.ToString()
    Assert.Equal("float", res)

[<Fact>]
let Int () =
    let res = SqlDataType.Int.ToString()
    Assert.Equal("int", res)

[<Fact>]
let Bool () =
    let res = SqlDataType.Bit.ToString()
    Assert.Equal("bit", res)

[<Theory>]
[<InlineData(10)>]
[<InlineData(20)>]
[<InlineData(200)>]
let String length =
    let res = (SqlDataType.Nvarchar <| charArgument.Number length).ToString()
    let expected = sprintf "nvarchar(%d)" length
    Assert.Equal(expected, res)

[<Fact>]
let StringMax () =
    let res = (SqlDataType.Nvarchar <| charArgument.Max).ToString()
    let expected = sprintf "nvarchar(max)"
    Assert.Equal(expected, res)

[<Fact>]
let Guid () =
    let res = SqlDataType.UniqueIdentifier.ToString()
    Assert.Equal("uniqueidentifier", res)

[<Fact>]
let DateTime () =
    let res = SqlDataType.DateTime.ToString()
    Assert.Equal("datetime", res)
