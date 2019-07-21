namespace JsonParser
open System

type public Key = string
type public NameSpace = string
type public Value =
            | Double of double
            | Decimal of decimal
            | String of string
            | DateTime of DateTime
            | Boolean of Boolean
            | Array of Value seq
            | Guid of Guid
            | Null
            | Object of Property seq
and public Property = Key * Value
