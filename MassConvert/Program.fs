namespace MassConvert

open System
open System.IO
open System.Collections.Generic

open FunToolbox.FileSystem

open Argu
open SharpYaml.Serialization

open MassConvert.Core

open JobCreator

type Arguments = 
    | [<Mandatory>] Config of string
    | Convert
    with 
        interface IArgParserTemplate with
            member s.Usage = 
                match s with
                | Config _ -> "specify a configuration file"
                | Convert -> "run the conversion, don't do a dry-run"


type Mode = 
    | DryRun = 0
    | Convert = 1

type StartupArguments = {
    configFile: Path
    mode: Mode
} with
    static member fromCommandLine currentDir argv = 
        let parser = ArgumentParser.Create<Arguments>()
        let result = parser.Parse(argv)
        let configFile = result.GetResult(<@ Config @>) |> (fun p -> currentDir |> Path.extend p)
        let convert = result.Contains(<@ Convert @>)
        let mode = if convert then Mode.Convert else Mode.DryRun
        {
            configFile = configFile
            mode = mode
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
        let composition = 
            JobTree.forDirectory config config.source.path config.destination.path
            |> Director.composeFromJobTree

        match startupArgs.mode with
        | Mode.DryRun ->
            composition.print()
            |> Seq.iter (printf "%s")
        | Mode.Convert ->
            failwithf "--convert unsupported right now."
        | _ -> failwith "internal error"

    [<EntryPoint>]
    let main argv = 
        try
            protectedMain argv
            0
        with e ->
            Console.WriteLine("MassConvert crashed:\n" + (e |> string))
            1


