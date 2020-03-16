namespace CSharp.Types
open Common.Casing
open Common.StringJoin
open System

type Settings =
    { ClassCasing: Casing
      PropertyCasing: Casing
      ClassPrefix: string
      ClassSuffix: string }

type internal TypeInfo =
    { Name: string
      Namespace: string
      Alias: string option
      Nullable: bool }
    member this.Stringified = this.Alias |> Option.defaultValue ([ this.Namespace; "."; this.Name ] |> joinStrings)
    member this.AsNullable =
        if this.Nullable then
            this
        else
            let nullableConcat x =
                [ x; "?" ] |> joinStrings
            { Namespace = this.Namespace
              Name = nullableConcat this.Name
              Alias = this.Alias |> Option.map nullableConcat
              Nullable = true }

type internal ValueTypePair<'T> =
    { Value: 'T
      Type: TypeInfo }
    member pair.AsNullable =
        { Value = pair.Value
          Type = pair.Type.AsNullable }

and internal ValueType =
    | Integer of ValueTypePair<int>
    | Guid of ValueTypePair<Guid>
    | Boolean of ValueTypePair<bool>
    | Datetime of ValueTypePair<DateTime>
    | Decimal of ValueTypePair<decimal>
    | Double of ValueTypePair<double>

    member valueType.AsNullable =
        match valueType with
        | Integer x -> x.AsNullable |> ValueType.Integer
        | Guid x -> x.AsNullable |> ValueType.Guid
        | Boolean x -> x.AsNullable |> ValueType.Boolean
        | Datetime x -> x.AsNullable |> ValueType.Datetime
        | Decimal x -> x.AsNullable |> ValueType.Decimal
        | Double x -> x.AsNullable |> ValueType.Double

    member this.TypeInfo =
        match this with
        | Integer x -> x.Type
        | Guid x -> x.Type
        | Boolean x -> x.Type
        | Datetime x -> x.Type
        | Decimal x -> x.Type
        | Double x -> x.Type


and internal ReferenceType =
    | String of ValueTypePair<string>
    | Object of TypeInfo
    member this.TypeInfo =
        match this with
        | String x -> x.Type
        | Object x -> x

and internal BaseType =
    | ReferenceType of ReferenceType
    | ValueType of ValueType

    member this.TypeInfo =
        match this with
        | ReferenceType x -> x.TypeInfo
        | ValueType x -> x.TypeInfo
    static member Guid x =
        { Type =
              { Namespace = "System"
                Name = "Guid"
                Alias = option.None
                Nullable = false }
          Value = x }
        |> ValueType.Guid
        |> BaseType.ValueType

    static member Double x =
        { Type =
              { Namespace = "System"
                Name = "Double"
                Alias = option.Some "double"
                Nullable = false }
          Value = x }
        |> ValueType.Double
        |> BaseType.ValueType
        
    static member Integer x =
        { Type =
              { Namespace = "System"
                Name = "Int32"
                Alias = option.Some "int"
                Nullable = false }
          Value = x }
        |> ValueType.Integer
        |> BaseType.ValueType

    static member Boolean x =
        { Type =
              { Namespace = "System"
                Name = "Boolean"
                Alias = option.Some "bool"
                Nullable = false }
          Value = x }
        |> ValueType.Boolean
        |> BaseType.ValueType

    static member DateTime x =
        { Type =
              { Namespace = "System"
                Name = "DateTime"
                Alias = option.None
                Nullable = false }
          Value = x }
        |> ValueType.Datetime
        |> BaseType.ValueType

    static member Decimal x =
        { Type =
              { Namespace = "System"
                Name = "Decimal"
                Alias = option.Some "decimal"
                Nullable = false }
          Value = x }
        |> ValueType.Decimal
        |> BaseType.ValueType

    static member Object =
        { Name = "Object"
          Namespace = "System"
          Alias = option.Some "object"
          Nullable = false }
        |> ReferenceType.Object
        |> BaseType.ReferenceType

    static member String x =
        { Type =
              { Namespace = "System"
                Name = "String"
                Alias = option.Some "string"
                Nullable = false }
          Value = x }
        |> ReferenceType.String
        |> BaseType.ReferenceType
        
and internal Property =
    { Name: string
      Type: CSType Option }

and internal CSType =
    | BaseType of BaseType
    | GeneratedType of Property[]
    | ArrayType of CSType
