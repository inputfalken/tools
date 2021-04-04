module SqlGeneratorTests.DataTypeTests

open Xunit
open Sql.Generator
open Languages.SQL


[<Fact>]
let Float () =
    let res = Float.ToString()
    Assert.Equal("float", res)

[<Fact>]
let Int () =
    let res = Int.ToString()
    Assert.Equal("int", res)

[<Fact>]
let Bool () =
    let res = Bit.ToString()
    Assert.Equal("bit", res)

[<Theory>]
[<InlineData(10)>]
[<InlineData(20)>]
[<InlineData(200)>]
let String length =
    let res = (Nvarchar <| Number length).ToString()
    let expected = sprintf "nvarchar(%d)" length
    Assert.Equal(expected, res)

[<Fact>]
let StringMax () =
    let res = (Nvarchar <| Max).ToString()
    let expected = sprintf "nvarchar(max)"
    Assert.Equal(expected, res)

[<Fact>]
let Guid () =
    let res = UniqueIdentifier.ToString()
    Assert.Equal("uniqueidentifier", res)

[<Fact>]
let DateTime () =
    let res = DateTime.ToString()
    Assert.Equal("datetime", res)
