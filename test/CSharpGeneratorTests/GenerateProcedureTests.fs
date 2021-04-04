module CSharpGeneratorTests.SQL.GenerateProcedureTests

open Sql.FromCSharp
open Xunit


[<Theory>]
[<InlineData("public class Person {}", "CREATE OR ALTER PROCEDURE Person () AS\nBEGIN\n\nEND")>]
[<InlineData("public class Person { public int Id { get; set; } }",
             "CREATE OR ALTER PROCEDURE Person (@Id int) AS\nBEGIN\n\nEND")>]
[<InlineData("public class Person { public int Id { get; set; } public string FirstName { get; set; } }",
             "CREATE OR ALTER PROCEDURE Person (@Id int, @FirstName nvarchar(max)) AS\nBEGIN\n\nEND")>]
let Class input expected =
    let result = generateStoredProcedureFromCSharp input { GenerationType = GenerationType.None }
    Assert.Equal(expected, result, false, true, true)
