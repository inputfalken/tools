namespace CSharp.Factory

open System
open Common.CaseInsensitiveString
open Common.StringJoin
open CSharp.Types
open CSharp.Formatters.Formatters

module internal CSharpFactory =
    let internal UnresolvedBaseType = BaseType.Object |> CSType.BaseType

    let private getFormatter =
        function
        | ArrayType _ -> arrayProperty
        | _ -> property

    let validateName (name: String) =
        if not (Char.IsLetter name.[0]) then
            raise (System.ArgumentException("Member names can only start with letters."))

    let rec private GeneratedType members key (classSet: CIString Set) settings propertyFormatter className =
        let className = className |> Option.defaultValue key
        let classSet = classSet.Add <| CI className

        let classContent =
            members
            |> Array.map (fun property ->
                let className =
                    if property.Name
                       |> CI
                       |> classSet.Contains
                    then settings.ClassCasing.applyMultiple [ className; property.Name ]
                    else property.Name
                    |> Option.Some
                match property.Type |> Option.defaultValue UnresolvedBaseType with
                | GeneratedType x ->
                    GeneratedType x property.Name classSet settings propertyFormatter className
                | ArrayType x ->
                    let formatter = arrayProperty
                    match x with
                    | GeneratedType x -> GeneratedType x property.Name classSet settings formatter className
                    | x -> CSharpFactoryPrivate x property.Name classSet settings formatter
                | x -> CSharpFactoryPrivate x property.Name classSet settings (getFormatter x))
            |> joinStringsWithSpaceSeparation

        let formattedClassName = settings.ClassCasing.applyMultiple [ settings.ClassPrefix; className; settings.ClassSuffix ]

        // Ugly side effect, maybe use Result in order in order to be explicit that things could go wrong.
        validateName formattedClassName

        let ``class`` = ``class`` formattedClassName classContent
        if classSet.Count = 1 then
            ``class``
        else
            let formattedPropertyName = key |> settings.PropertyCasing.apply
            let property = propertyFormatter formattedClassName formattedPropertyName
            let res = [ ``class``; property ] |> joinStringsWithSpaceSeparation
            res

    and private CSharpFactoryPrivate ``type`` key classSet settings propertyFormatter =
        match ``type`` with
        | GeneratedType members -> GeneratedType members key classSet settings propertyFormatter Option.None
        | ArrayType ``type`` -> CSharpFactoryPrivate ``type`` key classSet settings arrayProperty
        | BaseType x ->
            let formattedPropertyName =
                if classSet.IsEmpty then
                    [ settings.ClassPrefix; key; settings.ClassSuffix ] |> settings.PropertyCasing.applyMultiple
                else key |> settings.PropertyCasing.apply

            validateName formattedPropertyName
            propertyFormatter x.TypeInfo.Stringified formattedPropertyName

    let internal CSharpFactory ``type`` root settings =
        CSharpFactoryPrivate ``type`` root Set.empty settings (getFormatter ``type``)
