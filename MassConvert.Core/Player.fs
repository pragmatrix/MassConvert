namespace MassConvert.Core

open System.IO

open FunToolbox.FileSystem

open Director

/// Runs actions.
module Player = 

    type ActionResult = 
        | ActionError of exn
        | ActionSuccess

    let private act (f: unit -> unit) = 
        try
            f()
            ActionSuccess
        with e ->
            ActionError e
    
    let private args (inputFile: Path) (outputFile: Path) = 
        ["inputFile", inputFile.value; "outputFile", outputFile.value]
        |> Template.Arguments.ofList

    let private runCommand (commandLine: string) = 
        ()        

    let fileSystemPlayer (commandTemplate: string) (action: Action) : ActionResult = 
        fun () ->
            match action with
            | TryRemoveDir p -> Directory.Delete(p.value)
            | TryRemoveFile p -> File.Delete(p.value)
            | ConvertCommand (inputFile, outputFile) ->
                let args = args inputFile outputFile
                let commandLine = args.format commandTemplate
                runCommand commandLine
        |> act

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

        

                
        
    
    