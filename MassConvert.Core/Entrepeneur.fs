namespace MassConvert.Core

open System
open System.IO

open FunToolbox.FileSystem

open Scanner
open Diff

module Entrepeneur = 

    type Job = 
        | MayRemoveDestinationDirectory of DirectoryName
        | MayRemoveDestinationFile of FileName
        | ConvertFile of stem: string * srcExt: FileExtension * dstExt: FileExtension

    type JobEnvironment = {
        source: Path
        destination: Path
    }

    type JobList = {
        environment: JobEnvironment
        jobs: Job list
        nested: DirectoryName list
    } with
        member this.mkSourcePath stem (ext: FileExtension) =
            this.environment.source |> Path.extend (stem + ext.value)
        member this.mkDestinationPath stem (ext: FileExtension) = 
            this.environment.destination |> Path.extend (stem + ext.value)

    [<CR(ModuleSuffix)>]
    module JobList = 
        let fromDiff (diff: ScanResultDiff) = 

            // we want to get rid of stuff not required first.

            let removeDirs = 
                diff.directoriesDiff.remove
                |> List.map MayRemoveDestinationDirectory

            let removeFiles = 
                diff.filesStemDiff.remove
                |> List.map diff.destinationFiles.filename
                |> List.map MayRemoveDestinationFile
                
            // then we do update files the files in the current directory

            let updateFiles =
                diff.filesStemDiff.add @ diff.filesStemDiff.update
                |> List.map (fun stem -> ConvertFile(stem, diff.sourceFiles.extension stem, diff.destinationFiles.extension stem))
                
            let jobs = 
                [removeDirs; removeFiles; updateFiles]
                |> List.flatten

            {
                environment = 
                    {
                        source = diff.source
                        destination = diff.destination
                    }
                jobs = jobs
                nested = diff.directoriesDiff.add @ diff.directoriesDiff.update
            }

        let fromScanResults (source: DirectoryScanResult) (destination: DirectoryScanResult) : JobList = 
            Diff.diffScanResults source destination
            |> fromDiff

        let removeJobsIfFilesHaveTheSameDate (list: JobList) = 
            let jobFilter =
                function 
                | ConvertFile (stem, srcExt, dstExt) ->
                    let src = list.mkSourcePath stem srcExt
                    let dst = list.mkDestinationPath stem dstExt
                    File.GetLastWriteTimeUtc(src.value)
                    <>
                    File.GetLastWriteTimeUtc(dst.value)
                | _ -> true
            
            { list with jobs = list.jobs |> List.filter jobFilter }

        let forDirectory (configuration: Configuration) (source: Path) (destination: Path) : JobList = 
            let sourceContents = Scanner.scanDirectory source configuration.sourceFilePattern
            let destinationContents = Scanner.scanDirectory destination (configuration.destinationFileExtension |> GlobPattern.forExtension)
            fromScanResults sourceContents destinationContents
            |> removeJobsIfFilesHaveTheSameDate

    type JobTree = {
        list: JobList
        nested: (DirectoryName * JobTree) list
    } with
        member this.allTopDown = 
            let rec topDown this (fragments: Fragments) = 
                [
                    yield fragments, this.list.jobs
                    for (name, tree) in this.nested do
                        yield! topDown tree (fragments.push name)
                ]

            topDown this Fragments.root

    [<CR(ModuleSuffix)>]
    module JobTree =

        let rec forDirectory (configuration: Configuration) (source: Path) (destination: Path) : JobTree =
            let list = JobList.forDirectory configuration source destination
                
            let nested =
                list.nested
                |> List.map (fun dn -> dn, forDirectory configuration (source |> Path.extend dn.value) (destination |> Path.extend dn.value))
                
            {
                list = list
                nested = nested
            }





        
        
         

    

