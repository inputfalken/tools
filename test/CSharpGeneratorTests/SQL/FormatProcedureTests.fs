module CSharpGeneratorTests.SQL.FormatProcedure

open Xunit
open TemplateFactory.SQL
open TemplateFactory.SQL.SQL



[<Fact>]
let ```One parameter``() =
    let res =
        SQL.formatProcedure "ExecuteOnId"
            [ { Type =  Int
                Name = "id" } ]
    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int) AS\nBEGIN\n\nEND", res)

[<Fact>]
let ```Two parameters``() =
    let res =
        SQL.formatProcedure "ExecuteOnId"
            [ { Type = Int
                Name = "id" }
              { Type = Bit
                Name = "isDeleted" } ]
    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int, @isDeleted bit) AS\nBEGIN\n\nEND", res)

[<Fact>]
let ```Three parameters``() =
    let res =
        SQL.formatProcedure "ExecuteOnId"
            [ { Type = Int
                Name = "id" }
              { Type =  Bit
                Name = "isDeleted" }
              { Type = DateTime
                Name = "updatedAt" } ]
    Assert.Equal("CREATE OR ALTER PROCEDURE ExecuteOnId (@id int, @isDeleted bit, @updatedAt datetime) AS\nBEGIN\n\nEND", res)
