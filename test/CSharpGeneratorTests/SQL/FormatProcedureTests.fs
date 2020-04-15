module CSharpGeneratorTests.SQL.FormatProcedure

open Xunit
open TemplateFactory.SQL
open TemplateFactory.SQL.SQL



[<Fact>]
let ```One parameter``() =
    let argument =
        [ { Type = SqlDataType.Int
            Name = "id" } ]
        |> ProcedureParameter.Parameters

    let res = formatProcedure "ExecuteOnId" argument
    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int) AS\nBEGIN\n\nEND", res)

[<Fact>]
let ```User defined one parameter``() =

    let argument =
        { Parameters =
              [ { Type = SqlDataType.Int
                  Name = "id" } ]
          Name = "PersonType" }
        |> ProcedureParameter.UserDefinedTableType

    let res = SQL.formatProcedure "ExecuteOnId" argument
    let expected =
        """DROP PROCEDURE IF EXISTS ExecuteOnId
GO

IF type_id('PersonType') IS NOT NULL DROP TYPE PersonType
GO

CREATE TYPE PersonType AS TABLE (@id int)
GO

CREATE OR ALTER PROCEDURE ExecuteOnId (@PersonType PersonType) AS
BEGIN

END"""

    Assert.Equal(expected, res, false, true, true)

[<Fact>]
let ```User defined two parameter``() =

    let argument =
        { Parameters =
              [ { Type = SqlDataType.Int
                  Name = "id" }
                { Type = (SqlDataType.Nvarchar NVarCharArgument.Max)
                  Name = "FirstName" } ]
          Name = "PersonType" }
        |> ProcedureParameter.UserDefinedTableType

    let res = SQL.formatProcedure "ExecuteOnId" argument
    let expected =
        """DROP PROCEDURE IF EXISTS ExecuteOnId
GO

IF type_id('PersonType') IS NOT NULL DROP TYPE PersonType
GO

CREATE TYPE PersonType AS TABLE (@id int, @FirstName nvarchar(max))
GO

CREATE OR ALTER PROCEDURE ExecuteOnId (@PersonType PersonType) AS
BEGIN

END"""

    Assert.Equal(expected, res, false, true, true)

[<Fact>]
let ```User defined three parameter``() =

    let argument =
        { Parameters =
              [ { Type = SqlDataType.Int
                  Name = "id" }
                { Type = (SqlDataType.Nvarchar NVarCharArgument.Max)
                  Name = "FirstName" }
                { Type = SqlDataType.DateTime
                  Name = "TimeStamp" } ]
          Name = "PersonType" }
        |> ProcedureParameter.UserDefinedTableType

    let res = SQL.formatProcedure "ExecuteOnId" argument
    let expected =
        """DROP PROCEDURE IF EXISTS ExecuteOnId
GO

IF type_id('PersonType') IS NOT NULL DROP TYPE PersonType
GO

CREATE TYPE PersonType AS TABLE (@id int, @FirstName nvarchar(max), @TimeStamp datetime)
GO

CREATE OR ALTER PROCEDURE ExecuteOnId (@PersonType PersonType) AS
BEGIN

END"""

    Assert.Equal(expected, res, false, true, true)

[<Fact>]
let ```Two parameters``() =
    let parameter =
        [ { Type = Int
            Name = "id" }
          { Type = Bit
            Name = "isDeleted" } ]
        |> ProcedureParameter.Parameters

    let res = formatProcedure "ExecuteOnId" parameter

    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int, @isDeleted bit) AS\nBEGIN\n\nEND", res)

[<Fact>]
let ```Three parameters``() =
    let parameter =
        [ { Type = Int
            Name = "id" }
          { Type = Bit
            Name = "isDeleted" }
          { Type = DateTime
            Name = "updatedAt" } ]
        |> ProcedureParameter.Parameters

    let res = formatProcedure "ExecuteOnId" parameter
    Assert.Equal
        ("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int, @isDeleted bit, @updatedAt datetime) AS\nBEGIN\n\nEND", res)
