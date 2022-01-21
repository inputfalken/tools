module Languages.SQL

type CharArgument =
    | Max
    // We get type safety but negative numbers and 0 could still be passed which is not allowed.
    | Number of int

type DateTime2Argument =
    | Precision of int
    static member Default = DateTime2Argument.Precision 7

type DataType =
    | Date
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
        | Date -> "DATE"
        | DateTime -> "DATETIME"
        | DateTime2 x ->
            match x with
            | Precision y -> sprintf "DATETIME2(%d)" y
        | Varchar x ->
            match x with
            | Max -> "VARCHAR(MAX)"
            | Number x -> sprintf "VARCHAR(%d)" x
        | Int -> "INT"
        | Bit -> "BIT"
        | UniqueIdentifier -> "UNIQUEIDENTIFIER"
        | Float -> "FLOAT"
        | Nvarchar x ->
            match x with
            | Max -> "NVARCHAR(MAX)"
            | Number x -> sprintf "NVARCHAR(%d)" x

type Parameter = { Type: DataType; Name: string }

type UserDefined =
    { Parameters: Parameter list
      Name: string }

type ProcedureParameter =
    | DataType of Parameter
    | UserDefinedTableType of UserDefined
