namespace Generator

open System
open System.Collections.Generic
open System.Linq
        

type SqlDataApiEnum =
    | Bit = 1
    | Int = 2
    | String = 3

type public SqlSettingsApiModel() =
    member val GenerateUserDefinedTable = false with get, set

type public SqlProcedureDataTypeParameterApiModel(dataType: SqlDataApiEnum, name: string) =

    member val DataType = dataType
    member val Name = name

type public SqlProcedureUserTypeParameterApiModel(dataTypes: IEnumerable<SqlProcedureDataTypeParameterApiModel>, name: string) =
    member val DataTypes = dataTypes
    member val Name = name

type SqlProcedureApiModel(dataTypes: IEnumerable<SqlProcedureDataTypeParameterApiModel>, userDefinedTypes: IEnumerable<SqlProcedureUserTypeParameterApiModel>, name: string) =
    member val DataTypes = dataTypes
    member val UserDefinedTypes = userDefinedTypes
    member val Name = name
    static member DataTypes2  = Enum.GetValues(typeof<SqlDataApiEnum>) |> Enumerable.Cast<SqlDataApiEnum> |> Enumerable.ToArray
