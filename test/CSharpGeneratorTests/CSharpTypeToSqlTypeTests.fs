module CSharpGeneratorTests.SQL.CSharpTypeToSqlTypeTests

open System
open Xunit
open Languages.SQL
open Sql.FromCSharp

[<Theory>]
[<InlineData("foobar")>]
[<InlineData("foobar3")>]
[<InlineData("hello world")>]
[<InlineData("unhandled type")>]
let ``Unhandled types`` str =
    let ``exception`` =
        Assert.Throws<NotImplementedException>(fun () ->
            str
            |> toSqlType
            |> ignore)

    let expected = sprintf "Type '%s' is not supported." str
    Assert.Equal(expected, ``exception``.Message)


[<Theory>]
[<InlineData("float?")>]
let ``Float?`` str =
    let res = str |> toSqlType
    Assert.Equal(Float, res)

[<Theory>]
[<InlineData("System.Double?")>]
[<InlineData("double?")>]
let ``Double?`` str =
    let res = str |> toSqlType
    Assert.Equal(Float, res)

[<Theory>]
[<InlineData("Guid?")>]
[<InlineData("System.Guid?")>]
let ``Guid?`` str =
    let res = str |> toSqlType
    Assert.Equal(UniqueIdentifier, res)

[<Theory>]
[<InlineData("int?")>]
[<InlineData("Int32?")>]
[<InlineData("System.Int32?")>]
let ``Int?`` str =
    let res = str |> toSqlType
    Assert.Equal(Int, res)

[<Theory>]
[<InlineData("bool?")>]
[<InlineData("System.Boolean?")>]
let ``Bool?`` str =
    let res = str |> toSqlType
    Assert.Equal(Bit, res)

[<Theory>]
[<InlineData("DateTime?")>]
[<InlineData("System.DateTime?")>]
let ``DateTime?`` str =
    let res = str |> toSqlType
    Assert.Equal(DateTime, res)

[<Theory>]
[<InlineData("float")>]
let Float str =
    let res = str |> toSqlType
    Assert.Equal(Float, res)

[<Theory>]
[<InlineData("double")>]
[<InlineData("System.Double")>]
[<InlineData("Double")>]
let Double str =
    let res = str |> toSqlType
    Assert.Equal(Languages.SQL.Float, res)

[<Theory>]
[<InlineData("Guid")>]
[<InlineData("System.Guid")>]
let Guid str =
    let res = str |> toSqlType
    Assert.Equal(UniqueIdentifier, res)

[<Theory>]
[<InlineData("int")>]
[<InlineData("System.Int32")>]
[<InlineData("Int32")>]
let Int str =
    let res = str |> toSqlType
    Assert.Equal(Int, res)

[<Theory>]
[<InlineData("bool")>]
[<InlineData("Boolean")>]
[<InlineData("System.Boolean")>]
let Bool str =
    let res = str |> toSqlType
    Assert.Equal(Bit, res)

[<Theory>]
[<InlineData("DateTime")>]
[<InlineData("System.DateTime")>]
let DateTime str =
    let res = str |> toSqlType
    Assert.Equal(DateTime, res)

[<Theory>]
[<InlineData("string")>]
[<InlineData("System.String")>]
let String str =
    let res =
        match str |> toSqlType with
        | Nvarchar x -> x
        | _ -> raise (Exception())

    Assert.Equal(CharArgument.Max, res)
