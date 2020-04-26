module CSharpGeneratorTests.SQL.FormatProcedure

open Xunit
open TemplateFactory.SQL
open TemplateFactory.SQL.SQL

[<Fact>]
let ```One datatype parameter`` () =
    let argument =
        [ { Type = SqlDataType.Int
            Name = "id" } ]
        |> List.map ProcedureParameter.DataType

    let res = formatProcedure "ExecuteOnId" argument
    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int) AS\nBEGIN\n\nEND", res, false, true, true)

[<Fact>]
let ```Two datatype parameters`` () =
    let parameter =
        [ { Type = Int
            Name = "id" }
          { Type = Bit
            Name = "isDeleted" } ]
        |> List.map ProcedureParameter.DataType

    let res = formatProcedure "ExecuteOnId" parameter

    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int, @isDeleted bit) AS\nBEGIN\n\nEND", res, false, true, true)

[<Fact>]
let ```Three datatype parameters`` () =
    let parameter =
        [ { Type = Int
            Name = "id" }
          { Type = Bit
            Name = "isDeleted" }
          { Type = DateTime
            Name = "updatedAt" } ]
        |> List.map ProcedureParameter.DataType

    let res = formatProcedure "ExecuteOnId" parameter
    Assert.Equal
        ("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int, @isDeleted bit, @updatedAt datetime) AS\nBEGIN\n\nEND", res, false, true, true)

[<Fact>]
let ```Two User defined tables with one datatype parameter`` () =

    let argument =
        { Parameters =
              [ { Type = SqlDataType.Int
                  Name = "id" } ]
          Name = "PersonType" }
        |> ProcedureParameter.UserDefinedTableType
    let argument2 =
        { Parameters =
              [ { Type = SqlDataType.NvarcharMax
                  Name = "Address" } ]
          Name = "AddressType" }
        |> ProcedureParameter.UserDefinedTableType

    let res = SQL.formatProcedure "ExecuteOnId" [ argument; argument2 ]
    let expected =
        """DROP PROCEDURE IF EXISTS ExecuteOnId
GO

IF type_id('PersonType') IS NOT NULL DROP TYPE PersonType
GO

IF type_id('AddressType') IS NOT NULL DROP TYPE AddressType
GO

CREATE TYPE PersonType AS TABLE (id int)
GO

CREATE TYPE AddressType AS TABLE (Address nvarchar(max))
GO

CREATE OR ALTER PROCEDURE ExecuteOnId (@PersonType PersonType READONLY, @AddressType AddressType READONLY) AS
BEGIN

END"""

    Assert.Equal(expected, res, false, true, true)
[<Fact>]
let ```User defined table with one datatype parameter`` () =

    let argument =
        { Parameters =
              [ { Type = SqlDataType.Int
                  Name = "id" } ]
          Name = "PersonType" }
        |> ProcedureParameter.UserDefinedTableType

    let res = SQL.formatProcedure "ExecuteOnId" [ argument ]
    let expected =
        """DROP PROCEDURE IF EXISTS ExecuteOnId
GO

IF type_id('PersonType') IS NOT NULL DROP TYPE PersonType
GO

CREATE TYPE PersonType AS TABLE (id int)
GO

CREATE OR ALTER PROCEDURE ExecuteOnId (@PersonType PersonType READONLY) AS
BEGIN

END"""

    Assert.Equal(expected, res, false, true, true)

[<Fact>]
let ```User defined table with two datatype parameters`` () =

    let argument =
        { Parameters =
              [ { Type = SqlDataType.Int
                  Name = "id" }
                { Type = (SqlDataType.Nvarchar NVarCharArgument.Max)
                  Name = "FirstName" } ]
          Name = "PersonType" }
        |> ProcedureParameter.UserDefinedTableType

    let res = SQL.formatProcedure "ExecuteOnId" [ argument ]
    let expected =
        """DROP PROCEDURE IF EXISTS ExecuteOnId
GO

IF type_id('PersonType') IS NOT NULL DROP TYPE PersonType
GO

CREATE TYPE PersonType AS TABLE (id int, FirstName nvarchar(max))
GO

CREATE OR ALTER PROCEDURE ExecuteOnId (@PersonType PersonType READONLY) AS
BEGIN

END"""

    Assert.Equal(expected, res, false, true, true)

[<Fact>]
let ```User defined table with three datatype parameters`` () =

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

    let res = SQL.formatProcedure "ExecuteOnId" [ argument ]
    let expected =
        """DROP PROCEDURE IF EXISTS ExecuteOnId
GO

IF type_id('PersonType') IS NOT NULL DROP TYPE PersonType
GO

CREATE TYPE PersonType AS TABLE (id int, FirstName nvarchar(max), TimeStamp datetime)
GO

CREATE OR ALTER PROCEDURE ExecuteOnId (@PersonType PersonType READONLY) AS
BEGIN

END"""
    Assert.Equal(expected, res, false, true, true)

[<Fact>]
let ```One datatype parameter mixed with a user defined table with one datatype parameter`` () =
    let id =
        { Type = SqlDataType.Int
          Name = "Id" }

    let firstName =
        { Type = SqlDataType.NvarcharNumber 20
          Name = "FirstName" }

    let argument =
        [ id ] |> List.map ProcedureParameter.DataType

    let argument =
        argument @ [ ProcedureParameter.UserDefinedTableType
                         { Parameters = [ id; firstName ]
                           Name = "Users" } ]

    let res = formatProcedure "ExecuteOnId" argument
    let expected =
        """DROP PROCEDURE IF EXISTS ExecuteOnId
GO

IF type_id('Users') IS NOT NULL DROP TYPE Users
GO

CREATE TYPE Users AS TABLE (Id int, FirstName nvarchar(20))
GO

CREATE OR ALTER PROCEDURE ExecuteOnId (@Id int, @Users Users READONLY) AS
BEGIN

END"""
    Assert.Equal(expected, res, false, true, true)
