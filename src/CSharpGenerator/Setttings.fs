namespace CSharpGenerator.Arguments
    type public Settings() =
            member val Casing = "" with get, set
            member val NameSpace = "" with get, set
            member val ClassPrefix = "" with get, set
            member val ClassSuffix = "" with get, set
            member val RootObjectName = "" with get, set

    type public CSharpResult() =
            member val Error : System.Exception = null with get, set
            member val Value : string = null with get, set
