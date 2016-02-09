namespace MassConvert.Core

open System
open System.IO
open System.Diagnostics

open FunToolbox.FileSystem

open Director

/// Runs actions.
module Player = 

    type ActionResult = 
        | ActionError of string * exn
        | ActionSuccess

    let private act context (f: unit -> unit) = 
        try
            f()
            ActionSuccess
        with e ->
            ActionError (context(), e)

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

    let private tryToRemoveCorruptOutputFile (outputFile: Path) =
        try
            // first try to delete it!
            File.Delete(outputFile.value)
        with _ ->
        // if deleting did not work, try to move it away!
        try
            File.Move(outputFile.value, (outputFile |> Path.extend ".corrupt").value)
        with _ ->
            ()

    let fileSystemPlayer (action: Action) : ActionResult = 
        let ctx() = action.string
        fun () ->
            match action with
            | TryRemoveDir p -> 
                Directory.Delete(p.value)
            | TryRemoveFile p -> 
                File.Delete(p.value)
            | ConvertCommand (inputFile, outputFile, commandLine) ->
                let inputFileDate = File.GetLastWriteTimeUtc(inputFile.value)
                if inputFileDate > DateTime.UtcNow then
                    failwith "input file's modification date is in the future"
                outputFile |> Path.ensureDirectoryOfPathExists
                try
                    runCommand commandLine
                with _ ->
                    let outputFileDate = File.GetLastWriteTimeUtc(outputFile.value)
                    if outputFileDate > inputFileDate then
                        tryToRemoveCorruptOutputFile(outputFile)
                    reraise()
                let outputFileDate = File.GetLastWriteTimeUtc(outputFile.value)
                if outputFileDate <= inputFileDate then
                    failwith "output file was not touched by the command"
        |> act ctx

    // note: we play lazy :)
    let play (player: Action -> 'r) (actions: Action list) : 'r seq = 
        actions
        |> Seq.map player

        

                
        
    
    