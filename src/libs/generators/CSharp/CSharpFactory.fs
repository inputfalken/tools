namespace CSharp.Factory

open System
open Common.CaseInsensitiveString
open Common.StringJoin
open CSharp.Types
open CSharp.Formatters.Formatters
open Common.Casing

module internal CSharpFactory =
    let internal UnresolvedBaseType = BaseType.Object |> CSType.BaseType

    let private getFormatter =
        function
        | ArrayType _ -> arrayProperty
        | _ -> property

    let private applyPrefixSuffix key (settings: Settings) (casing: Casing) =
        match settings.LetterRule with
        | Prefix x -> [ x; key ]
        | Suffix x -> [ key; x]
        | ``Prefix and Suffix`` (x, y) -> [ x; key; y]
        |> casing.applyMultiple

    let private createClassName (classSet: CIString Set) property className settings =
        if property
           |> CI
           |> classSet.Contains
        then settings.ClassCasing.applyMultiple [ className; property ]
        else property
        |> Option.Some

    let validateName (name: String) =
        if not (Char.IsLetter name.[0]) then
            raise (System.ArgumentException("Member names can only start with letters."))

    let rec private GeneratedType members key (classSet: CIString Set) settings propertyFormatter className =
        let className = className |> Option.defaultValue key
        let classSet = classSet.Add <| CI className

        let classContent =
            members
            |> Array.map (fun property ->
                match property.Type |> Option.defaultValue UnresolvedBaseType with
                | GeneratedType x ->
                    let uniqueClassName = createClassName classSet property.Name className settings
                    GeneratedType x property.Name classSet settings propertyFormatter uniqueClassName
                | ArrayType x ->
                    match x with
                    | GeneratedType x ->
                        let uniqueClassName = createClassName classSet property.Name className settings
                        GeneratedType x property.Name classSet settings arrayProperty uniqueClassName
                    | x -> CSharpFactoryPrivate x property.Name classSet settings arrayProperty
                | x -> CSharpFactoryPrivate x property.Name classSet settings (getFormatter x))
            |> joinStringsWithSpaceSeparation

        let formattedClassName = applyPrefixSuffix className settings settings.ClassCasing

        // Ugly side effect, maybe use Result in order in order to be explicit that things could go wrong.
        validateName formattedClassName

        let ``class`` = ``class`` formattedClassName classContent
        if classSet.Count = 1 then
            ``class``
        else
            let formattedPropertyName = key |> settings.PropertyCasing.apply
            let property = propertyFormatter formattedClassName formattedPropertyName
            [ ``class``; property ] |> joinStringsWithSpaceSeparation

    and private CSharpFactoryPrivate ``type`` key classSet settings propertyFormatter =
        match ``type`` with
        | GeneratedType members -> GeneratedType members key classSet settings propertyFormatter Option.None
        | ArrayType ``type`` -> CSharpFactoryPrivate ``type`` key classSet settings arrayProperty
        | BaseType x ->

            let formattedPropertyName =
                if classSet.IsEmpty then applyPrefixSuffix key settings settings.PropertyCasing
                else key |> settings.PropertyCasing.apply

            // Ugly side effect, maybe use Result in order in order to be explicit that things could go wrong.
            validateName formattedPropertyName

            propertyFormatter x.TypeInfo.Stringified formattedPropertyName

    let internal CSharpFactory ``type`` settings =
        CSharpFactoryPrivate ``type`` settings.RootName Set.empty settings (getFormatter ``type``)
