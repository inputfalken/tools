module CSharpGeneratorTests.SQL.CSharpTypeToSqlTypeTests

open Xunit
open TemplateFactory.SQL.SQL


[<Theory>]
[<InlineData("guid")>]
[<InlineData("GUID")>]
[<InlineData("GuId")>]
let Guid str =
    let res = str |> SqlDataType.toSqlType
    Assert.Equal(SqlDataType.UniqueIdentifier, res)

[<Theory>]
[<InlineData("int")>]
[<InlineData("INT")>]
[<InlineData("InT")>]
[<InlineData("int32")>]
[<InlineData("INT32")>]
[<InlineData("InT32")>]
let Int str =
    let res = str |> SqlDataType.toSqlType
    Assert.Equal(SqlDataType.Int, res)

[<Theory>]
[<InlineData("bool")>]
[<InlineData("BOOL")>]
[<InlineData("bOoL")>]
[<InlineData("boolean")>]
[<InlineData("BOOLEAN")>]
[<InlineData("bOoLeAn")>]
let Bool str =
    let res = str |> SqlDataType.toSqlType
    Assert.Equal(SqlDataType.Bit, res)

[<Theory>]
[<InlineData("datetime")>]
[<InlineData("DATETIME")>]
[<InlineData("dAteTime")>]
let DateTime str =
    let res = str |> SqlDataType.toSqlType
    Assert.Equal(SqlDataType.DateTime, res)

[<Theory>]
[<InlineData("string")>]
[<InlineData("STRING")>]
[<InlineData("stRinG")>]
let String str =
    let res =
        match str |> SqlDataType.toSqlType with
        | Nvarchar x -> x
        | _ -> raise (System.Exception())

    Assert.Equal(NVarCharArgument.Max, res)
