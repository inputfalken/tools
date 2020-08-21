module SqlGeneratorTests.DataTypeTests

open Xunit
open Sql.Generator


[<Fact>]
let Float () =
    let res = Sql.Types.Float.ToString()
    Assert.Equal("float", res)

[<Fact>]
let Int () =
    let res = Sql.Types.Int.ToString()
    Assert.Equal("int", res)

[<Fact>]
let Bool () =
    let res = Sql.Types.Bit.ToString()
    Assert.Equal("bit", res)

[<Theory>]
[<InlineData(10)>]
[<InlineData(20)>]
[<InlineData(200)>]
let String length =
    let res = (Sql.Types.Nvarchar <| Sql.Types.Number length).ToString()
    let expected = sprintf "nvarchar(%d)" length
    Assert.Equal(expected, res)

[<Fact>]
let StringMax () =
    let res = (Sql.Types.Nvarchar <| Sql.Types.Max).ToString()
    let expected = sprintf "nvarchar(max)"
    Assert.Equal(expected, res)

[<Fact>]
let Guid () =
    let res = Sql.Types.UniqueIdentifier.ToString()
    Assert.Equal("uniqueidentifier", res)

[<Fact>]
let DateTime () =
    let res = Sql.Types.DateTime.ToString()
    Assert.Equal("datetime", res)
