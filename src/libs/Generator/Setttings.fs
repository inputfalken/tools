namespace Generator

    type public Settings() =
            member val PropertyCasing = "" with get, set
            member val TypeCasing = "" with get, set
            member val NameSpace = "" with get, set
            member val ClassPrefix = "" with get, set
            member val ClassSuffix = "" with get, set
            member val RootObjectName = "" with get, set
