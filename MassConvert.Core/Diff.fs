namespace MassConvert.Core

open FunToolbox.FileSystem

open Scanner

/// Finds out what to do by comparing filename stems.
module Diff = 

    type 'e setdiff = {
        remove: 'e list
        update: 'e list
        add: 'e list
        }

    /// Diff two sets and express the result in remove / update / add steps to get from left to right.
    let private setDiff (left: 'e seq) (right: 'e seq) = 
        let leftSet = left |> Set.ofSeq
        let rightSet = right |> Set.ofSeq
        let remove = leftSet - rightSet
        let add = rightSet - leftSet
        let update = Set.intersect leftSet rightSet

        {
            remove = remove |> Set.toList
            update =  update |> Set.toList
            add = add |> Set.toList
        }
        
    /// A map of filename names without extension to their extensions
    type StemExtMap = FileExtMap of Map<string, string>
        with
        member this.value = let (FileExtMap value) = this in value
        member this.stemNames = this.value |> Map.toSeq |> Seq.map fst |> Seq.toList
        member this.filename stem = stem + this.value.[stem] |> FileName.parse
        member this.extension stem = this.value.[stem] |> FileExtension.parse
        static member ofFileNames(files: FileName list) =
            files
            |> List.map (fun fn -> fn.nameWithoutExtension, fn.extension)
            |> Map.ofList
            |> FileExtMap

    type ScanResultDiff = { 
            sourceRoot: Path
            destinationRoot: Path
            relative: RelativePath

            sourceFiles: StemExtMap
            destinationFiles: StemExtMap

            filesStemDiff : string setdiff
            directoriesDiff: DirectoryName setdiff

            nested: (DirectoryName * ScanResultDiff) list
        }        

    // computes a diff that describes a transformation from source to destination
    let rec scanResults (source: ScanResult) (destination: ScanResult) : ScanResultDiff = 
        if source.relative <> destination.relative then
            failwithf "can't compute diff, source and destination relative paths do not match:\nsource:%s\ndestination:%s" source.relative.string destination.relative.string

        let sourceStemExtMap = source.files |> StemExtMap.ofFileNames
        let destinationStemExtMap = destination.files |> StemExtMap.ofFileNames

        let files = setDiff sourceStemExtMap.stemNames destinationStemExtMap.stemNames

        let sourceDirectories = source.nested |> List.map fst
        let destinationDirectories = destination.nested |> List.map fst

        let directories = setDiff sourceDirectories destinationDirectories
        
        let sourceMap = source.nested |> Map.ofList
        let destinationMap = destination.nested |> Map.ofList

        // for the nested directoies that needs an update, we recurse
        let nestedUpdated = 
            directories.update
            |> List.map (fun dn -> dn, scanResults sourceMap.[dn] destinationMap.[dn])

        // for the ones that exist in source but not in destination, we create 
        // empty diffs against the source
        let nestedRemoved = 
            directories.remove
            |> List.map (
                fun dn ->
                    let emptyDestination = ScanResult.empty destination.root (destination.relative.extend dn)
                    dn, scanResults sourceMap.[dn] emptyDestination)

        // for the ones the exist in destination but not in source, we create
        // empty diffs against the destination
        let nestedAdded = 
            directories.add
            |> List.map (
                fun dn ->
                    let emptySource = ScanResult.empty source.root (source.relative.extend dn)
                    dn, scanResults emptySource destinationMap.[dn])

        {
            sourceRoot = source.root
            destinationRoot = destination.root
            relative = source.relative

            sourceFiles = sourceStemExtMap
            destinationFiles = destinationStemExtMap
            filesStemDiff = files
            directoriesDiff = directories
            nested = [nestedRemoved; nestedUpdated; nestedAdded] |> List.flatten
        }
        

