namespace JsonParser

open System

type public Key = string

type public Value =
    | Double of double
    | Decimal of decimal
    | String of string
    | DateTime of DateTime
    | Boolean of Boolean
    | Array of Value []
    | Guid of Guid
    | Null
    | Object of Record []

and public Record =
    { Key: Key
      Value: Value }
