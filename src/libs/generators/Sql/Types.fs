module Sql.Types

type CharArgument =
    | Max
    // We get type safety but negative numbers and 0 could still be passed which is not allowed.
    | Number of int

type DateTime2Argument =
    | Precision of int
    static member Default = DateTime2Argument.Precision 7

type DataType =
    | DateTime
    | DateTime2 of DateTime2Argument
    | Varchar of CharArgument
    | Int
    | Float
    | Bit
    | UniqueIdentifier
    | Nvarchar of CharArgument
    static member NvarcharMax = CharArgument.Max |> Nvarchar
    static member NvarcharNumber number = CharArgument.Number number |> Nvarchar

    override x.ToString() =
        match x with
        | DateTime -> "datetime"
        | DateTime2 x ->
            match x with
            | Precision y -> sprintf "datetime2(%d)" y
        | Varchar x ->
            match x with
            | Max -> "varchar(max)"
            | Number x -> sprintf "varchar(%d)" x
        | Int -> "int"
        | Bit -> "bit"
        | UniqueIdentifier -> "uniqueidentifier"
        | Float -> "float"
        | Nvarchar x ->
            match x with
            | Max -> "nvarchar(max)"
            | Number x -> sprintf "nvarchar(%d)" x

type Parameter =
    { Type: DataType
      Name: string
      Nullable: bool }

type UserDefined =
    { Parameters: Parameter list
      Name: string }

type ProcedureParameter =
    | DataType of Parameter
    | UserDefinedTableType of UserDefined
