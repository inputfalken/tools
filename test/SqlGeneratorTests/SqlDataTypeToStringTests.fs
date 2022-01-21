module SqlGeneratorTests.DataTypeTests

open Xunit
open Languages.SQL


[<Fact>]
let Float () =
    let res = Float.ToString()
    Assert.Equal("FLOAT", res)

[<Fact>]
let Int () =
    let res = Int.ToString()
    Assert.Equal("INT", res)

[<Fact>]
let Bool () =
    let res = Bit.ToString()
    Assert.Equal("BIT", res)

[<Theory>]
[<InlineData(10)>]
[<InlineData(20)>]
[<InlineData(200)>]
let String length =
    let res = (Nvarchar <| Number length).ToString()
    let expected = sprintf "NVARCHAR(%d)" length
    Assert.Equal(expected, res)

[<Fact>]
let StringMax () =
    let res = (Nvarchar <| Max).ToString()
    let expected = sprintf "NVARCHAR(MAX)"
    Assert.Equal(expected, res)

[<Fact>]
let Guid () =
    let res = UniqueIdentifier.ToString()
    Assert.Equal("UNIQUEIDENTIFIER", res)

[<Fact>]
let DateTime () =
    let res = DateTime.ToString()
    Assert.Equal("DATETIME", res)
    
[<Fact>]
let Date () =
    let res = Date.ToString()
    Assert.Equal("DATE", res)
