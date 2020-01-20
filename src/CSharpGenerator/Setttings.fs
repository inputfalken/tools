namespace CSharpGenerator.Arguments

    open Common.Casing

    type public Settings() =
            member val Casing = "" with get, set
            member val NameSpace = "" with get, set
            member val ClassPrefix = "" with get, set
            member val ClassSuffix = "" with get, set
            member val RootObjectName = "" with get, set
            static member public Cases = Casing.Cases
            static member Pascal = Settings.Cases.[0]
            static member Camel = Settings.Cases.[1]
            static member None = Settings.Cases.[2]
