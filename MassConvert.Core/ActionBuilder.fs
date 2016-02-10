namespace MassConvert.Core

open FunToolbox.FileSystem

open JobBuilder

/// Converts jobs to actions
module ActionBuilder = 

    type Action = 
        | TryRemoveFile of Path        
        | TryRemoveDir of Path
        | ConvertCommand of inputFile: Path * tmpFile: Path * outputFile: Path * commandLine: string
        with 
        member this.string = 
            match this with
            | TryRemoveFile p -> sprintf "- %s" p.value
            | TryRemoveDir p -> sprintf "- %s (dir)" p.value
            | ConvertCommand (_, _, _, cmd) -> sprintf "> " + cmd

    [<Literal>]
    let TempFileExtension = ".mc.tmp"

    let private quotePath (path: string) = 
        if path.Contains("\"") || path.Contains("\\") then
            path |> failwithf "%s: failed to quote this path, it already contains a quote or backslashes"
        "\"" + path + "\""

    let private args (inputFile: Path) (tmpFile: Path) = 
        // note: we want the paths to be quoted by default, so that space characters
        // don't lead to nasty surprises.
        [
            "inputFile", inputFile.value |> quotePath
            "outputFile", tmpFile.value |> quotePath
            "rawInputFile", inputFile.value
            "rawOutputFile", tmpFile.value
        ]
        |> Template.Arguments.ofList

    let fromJobs (configuration: Configuration) (jobs: Job list) = 
        
        let srcRoot = configuration.source.path
        let dstRoot = configuration.destination.path
        let dstExt = configuration.destination.extension
        let commandLineTemplate = configuration.command.template

        let job2Action job = 
            match job with
            | PurgeDirectory (path, dn) -> 
                let path = path.extend dn
                TryRemoveDir (dstRoot |> Path.extend path.string)
            | PurgeFile (path, fn) -> 
                let path = path.extend fn
                TryRemoveFile (dstRoot |> Path.extend path.string)
            | ConvertFile (path, srcFile) -> 
                let inputFile = 
                    let srcPath = path.extend srcFile
                    srcRoot |> Path.extend srcPath.string
    
                let srcStem = srcFile.nameWithoutExtension

                let tmpFile = 
                    let tmpFile = FileName (srcStem, dstExt.string + TempFileExtension)
                    let tmpPath = path.extend tmpFile
                    dstRoot |> Path.extend tmpPath.string

                let outputFile = 
                    let dstFile = FileName (srcStem, dstExt.string)
                    let dstPath = path.extend dstFile
                    dstRoot |> Path.extend dstPath.string

                let commandLine = 
                    let args = args inputFile tmpFile
                    args.format commandLineTemplate
        
                ConvertCommand (inputFile, tmpFile, outputFile, commandLine)

        jobs
        |> List.map job2Action

