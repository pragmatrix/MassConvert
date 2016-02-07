namespace MassConvert.Core

open FunToolbox.FileSystem

open Scanner

module Diff = 

    type 'e setdiff = {
        remove: 'e list
        update: 'e list
        add: 'e list
        }

    let private setDiff (left: 'e seq) (right: 'e seq) = 
        let leftSet = left |> Set.ofSeq
        let rightSet = right |> Set.ofSeq
        let removed = leftSet - rightSet
        let added = rightSet - leftSet
        let same = Set.intersect leftSet rightSet

        {
            remove = removed |> Set.toList
            update =  same |> Set.toList
            add = added |> Set.toList
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
            source: Path
            destination: Path

            sourceFiles: StemExtMap
            destinationFiles: StemExtMap

            filesStemDiff : string setdiff
            directoriesDiff: DirectoryName setdiff
        }        


    let diffScanResults (source: DirectoryScanResult) (destination: DirectoryScanResult) = 
        let sourceStemExtMap = source.files |> StemExtMap.ofFileNames
        let destinationStemExtMap = destination.files |> StemExtMap.ofFileNames

        let files = setDiff sourceStemExtMap.stemNames destinationStemExtMap.stemNames
        let directories = setDiff source.directories destination.directories
        {
            source = source.path
            destination = destination.path
            sourceFiles = sourceStemExtMap
            destinationFiles = destinationStemExtMap
            filesStemDiff = files
            directoriesDiff = directories
        }
        

