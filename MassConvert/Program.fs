﻿namespace MassConvert

open System
open System.IO
open System.Collections.Generic
open System.Diagnostics

open FunToolbox.FileSystem

open Argu
open SharpYaml.Serialization

open MassConvert.Core

open JobCreator
open Player

type Arguments = 
    | [<Mandatory>] Config of string
    | Convert
    | Brave
    with 
        interface IArgParserTemplate with
            member s.Usage = 
                match s with
                | Config _ -> "specify a configuration file"
                | Convert -> "run the conversion, don't do a dry-run"
                | Brave -> "be brave, don't halt on errors"

type Mode = 
    | DryRun
    | Convert

type Character =
    | Coward
    | Brave

type StartupArguments = {
    configFile: Path
    mode: Mode
    character: Character
} with
    static member fromCommandLine currentDir argv = 
        let parser = ArgumentParser.Create<Arguments>()
        let result = parser.Parse(argv)
        let configFile = result.GetResult(<@ Config @>) |> (fun p -> currentDir |> Path.extend p)
        let convert = result.Contains(<@ Arguments.Convert @>)
        let brave = result.Contains(<@ Arguments.Brave @>)

        let mode = if convert then Mode.Convert else Mode.DryRun
        let character = if brave then Brave else Coward
        {
            configFile = configFile
            mode = mode
            character = character
        }


module Main =

    let readConfiguration (args: StartupArguments) : Configuration =
        let ser = Serializer()
        let file = File.ReadAllText(args.configFile.value)
        let configDir = args.configFile |> Path.parent
        let values = ser.Deserialize<Dictionary<obj, obj>>(file)
        Configuration.fromDictionary configDir values

    let protectedMain argv = 
        let currentDir = Path.currentDirectory()
        let startupArgs = StartupArguments.fromCommandLine currentDir argv
        let config = startupArgs |> readConfiguration
        let actions = 
            JobTree.forDirectory config config.source.path config.destination.path
            |> Director.composeFromJobTree
            |> Director.compositionToActions

        match startupArgs.mode with
        | Mode.DryRun ->
            let player = Player.dryPlayer config.command.template
            let results = actions |> Player.play player
            results |> Seq.iter (printfn "%s")
        | Mode.Convert ->
            let player = Player.fileSystemPlayer config.command.template
            for result in actions |> Player.play player do
                match result with
                | ActionError (context, e) ->
                    printfn "Error: %s\n  %s" e.Message context
                    match startupArgs.character with
                    | Coward -> failwith "I'm a coward and stop at the first error, make me brave with --brave."
                    | Brave -> ()
                | ActionSuccess -> ()

    [<EntryPoint>]
    let main argv = 
        try
            protectedMain argv
            0
        with e ->
            Console.WriteLine("failed:\n" + e.Message)
            Debug.WriteLine("failed:\n" + (string e))
            1


