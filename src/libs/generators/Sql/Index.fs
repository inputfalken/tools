module Sql.Index

open System
open Common.StringJoin

type GenerationType =
    | UserDefinedTableType
    | None

type Settings =
    { GenerationType: GenerationType }


let newline = Environment.NewLine
let doubleNewLine = newline + newline

type NVarCharArgument =
    | Max
    // We get type safety but negative numbers and 0 could still be passed which is not allowed.
    | Number of int

type SqlDataType =
    | DateTime
    | Int
    | Float
    | Bit
    | UniqueIdentifier
    | Nvarchar of NVarCharArgument

    static member NvarcharMax = NVarCharArgument.Max |> Nvarchar
    static member NvarcharNumber number = NVarCharArgument.Number number |> Nvarchar

    override x.ToString() =
        match x with
        | DateTime -> "datetime"
        | Int -> "int"
        | Bit -> "bit"
        | UniqueIdentifier -> "uniqueidentifier"
        | Float -> "float"
        | Nvarchar x ->
            match x with
            | Max -> "nvarchar(max)"
            | Number x -> sprintf "nvarchar(%d)" x

type Parameter =
    { Type: SqlDataType
      Name: string }

type UserDefined =
    { Parameters: Parameter list
      Name: string }

type ProcedureParameter =
    | DataType of Parameter
    | UserDefinedTableType of UserDefined

let public formatProcedure name (arg: ProcedureParameter list): string =

    let userDefinedTypes =
        arg
        |> List.choose (fun x ->
            match x with
            | UserDefinedTableType x -> option.Some x
            | DataType _ -> option.None)

    let stringifiedParams =
        arg
        |> List.map (fun x ->
            match x with
            | DataType x ->
                let name =
                    match x.Name with
                    | x when x.[0] = '@' -> x
                    | x -> joinStrings [ "@"; x ]
                [ name
                  x.Type.ToString() ]
                |> joinStringsWithSpaceSeparation
            | UserDefinedTableType userDefined ->

                let param =
                    joinStringsWithSpaceSeparation
                        [ (joinStrings [ "@"; userDefined.Name ])
                          userDefined.Name
                          "READONLY" ]

                param)

    let userDefinedTypeProvided = not userDefinedTypes.IsEmpty

    let dropProcedure =
        if userDefinedTypeProvided
        then sprintf "DROP PROCEDURE IF EXISTS %s%sGO%s" name newline doubleNewLine
        else System.String.Empty

    let userDefinedCreate =
        if userDefinedTypeProvided then
            (userDefinedTypes
             |> List.map (fun x ->
                 sprintf "CREATE TYPE %s AS TABLE (%s)%sGO" x.Name
                     (x.Parameters
                      |> List.map (fun x -> sprintf "%s %s" x.Name (x.Type.ToString()))
                      |> joinStringsWithCommaSpaceSeparation) newline)
             |> String.concat doubleNewLine)
            + doubleNewLine
        else
            System.String.Empty

    let userDefinedDrop =
        if userDefinedTypeProvided then
            (userDefinedTypes
             |> List.map (fun x -> sprintf "IF type_id('%s') IS NOT NULL DROP TYPE %s%sGO" x.Name x.Name newline)
             |> String.concat doubleNewLine)
            + doubleNewLine
        else
            System.String.Empty


    let createOrAlterProcedure x =
        sprintf "CREATE OR ALTER PROCEDURE %s (%s) AS%sBEGIN%sEND" name x newline doubleNewLine

    [ dropProcedure
      userDefinedDrop
      userDefinedCreate
      (stringifiedParams
       |> joinStringsWithCommaSpaceSeparation
       |> createOrAlterProcedure) ]
    |> joinStrings
