module Sql.Generator

open System
open Common.StringJoin
open Languages.SQL

let newline = Environment.NewLine
let doubleNewLine = newline + newline

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

                [ name; x.Type.ToString() ]
                |> joinStringsWithSpaceSeparation
            | UserDefinedTableType userDefined ->

                let param =
                    joinStringsWithSpaceSeparation [ (joinStrings [ "@"; userDefined.Name ])
                                                     userDefined.Name
                                                     "READONLY" ]

                param)

    let userDefinedTypeProvided = not userDefinedTypes.IsEmpty

    let dropProcedure =
        if userDefinedTypeProvided
        then sprintf "DROP PROCEDURE IF EXISTS %s%sGO%s" name newline doubleNewLine
        else String.Empty

    let userDefinedCreate =
        if userDefinedTypeProvided then
            (userDefinedTypes
             |> List.map (fun x ->
                 sprintf
                     "CREATE TYPE %s AS TABLE (%s)%sGO"
                     x.Name
                     (x.Parameters
                      |> List.map (fun x -> sprintf "%s %s" x.Name (x.Type.ToString()))
                      |> joinStringsWithCommaSpaceSeparation)
                     newline)
             |> String.concat doubleNewLine)
            + doubleNewLine
        else
            String.Empty

    let userDefinedDrop =
        if userDefinedTypeProvided then
            (userDefinedTypes
             |> List.map (fun x -> sprintf "IF type_id('%s') IS NOT NULL DROP TYPE %s%sGO" x.Name x.Name newline)
             |> String.concat doubleNewLine)
            + doubleNewLine
        else
            String.Empty


    let createOrAlterProcedure x =
        sprintf "CREATE OR ALTER PROCEDURE %s (%s) AS%sBEGIN%sEND" name x newline doubleNewLine

    [ dropProcedure
      userDefinedDrop
      userDefinedCreate
      (stringifiedParams
       |> joinStringsWithCommaSpaceSeparation
       |> createOrAlterProcedure) ]
    |> joinStrings
