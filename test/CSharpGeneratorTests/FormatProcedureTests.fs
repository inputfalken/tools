module CSharpGeneratorTests.FormatProcedure

open Xunit
open TemplateFactory.SQL
open TemplateFactory.SQL.SQL



[<Fact>]
let ```One parameter``() =
    let res =
        SQL.formatProcedure "ExecuteOnId"
            [ { Type = "int"
                Name = "id" } ]
    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int) AS\nBEGIN\n\nEND", res)

[<Fact>]
let ```Two parameters``() =
    let res =
        SQL.formatProcedure "ExecuteOnId"
            [ { Type = "int"
                Name = "id" }
              { Type = "bit"
                Name = "isDeleted" } ]
    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int, @isDeleted bit) AS\nBEGIN\n\nEND", res)

[<Fact>]
let ```Three parameters``() =
    let res =
        SQL.formatProcedure "ExecuteOnId"
            [ { Type = "int"
                Name = "id" }
              { Type = "bit"
                Name = "isDeleted" }
              { Type = "datetime"
                Name = "updatedAt" } ]
    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int, @isDeleted bit, @updatedAt datetime) AS\nBEGIN\n\nEND", res)
