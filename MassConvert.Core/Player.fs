namespace MassConvert.Core

open System.IO

open FunToolbox.FileSystem

open Director

/// Runs actions.
module Player = 
    open System.Diagnostics

    type ActionResult = 
        | ActionError of string * exn
        | ActionSuccess

    let private act context (f: unit -> unit) = 
        try
            f()
            ActionSuccess
        with e ->
            ActionError (context(), e)
    
    let private quotePath (path: string) = 
        if path.Contains("\"") || path.Contains("\\") then
            path |> failwithf "%s: failed to quote this path, it already contains a quote or backslashes"
        "\"" + path + "\""

    let private args (inputFile: Path) (outputFile: Path) = 
        // note: we want the paths to be quoted by default, so that space characters
        // don't lead to nasty surprises.
        [
            "inputFile", inputFile.value |> quotePath
            "outputFile", outputFile.value |> quotePath
            "rawInputFile", inputFile.value
            "rawOutputFile", outputFile.value
        ]
        |> Template.Arguments.ofList

    let private splitCommandLine (commandLine: string) = 
        let commandLine = commandLine.Trim()
        match commandLine with
        | "" -> failwithf "command line is empty"
        | cmdLine when cmdLine.[0] = '"' ->
            match cmdLine.IndexOf('"', 1) with
            | -1 -> cmdLine |> failwithf "%s: can't find the closing quote in command line"
            | i -> cmdLine.Substring(1, i-1), cmdLine.Substring(i+1)
        | cmdLine ->
        match cmdLine.IndexOf(' ') with
        | -1 -> cmdLine |> failwithf "%s: no actual arguments in the command line, what about {inputFile} and {outputFile}?"
        | i -> cmdLine.Substring(0, i), cmdLine.Substring(i+1).Trim()

    let private runCommand (commandLine: string) = 
        let si = ProcessStartInfo()      
        si.UseShellExecute <- false
        let filename, args = splitCommandLine commandLine
        si.FileName <- filename
        si.Arguments <- args
        match Process.Start(si) with
        | null -> failwith "failed to start process (null)"
        | p ->
        use proc = p
        proc.WaitForExit()
        if proc.ExitCode <> 0 then
            failwithf "command failed with exit code %d" proc.ExitCode
        else
        ()


    let fileSystemPlayer (commandTemplate: string) (action: Action) : ActionResult = 
        match action with
        | TryRemoveDir p -> 
            fun () -> Directory.Delete(p.value)
            |> act (fun () -> sprintf "rmdir %s" p.value)
        | TryRemoveFile p -> 
            fun () -> File.Delete(p.value)
            |> act (fun () -> sprintf "rmfile %s" p.value)
        | ConvertCommand (inputFile, outputFile) ->
            // if template formatting fails, we want a huge crash!
            let args = args inputFile outputFile
            let commandLine = args.format commandTemplate
            // also if we can't create the directory for the output file 
            outputFile |> Path.ensureDirectoryOfPathExists 
            fun () ->
                runCommand commandLine
            |> act (fun() -> "> " + commandLine)

    let dryPlayer (commandTemplate: string) (action: Action) : string = 
        match action with
        | TryRemoveDir p -> "- " + p.value + " (dir)"
        | TryRemoveFile p -> "- " + p.value
        | ConvertCommand (inputFile, outputFile) ->
            let args = args inputFile outputFile
            "> " + args.format commandTemplate

    // note: we play lazy :)
    let play (player: Action -> 'r) (actions: Action list) : 'r seq = 
        actions
        |> Seq.map player

        

                
        
    
    