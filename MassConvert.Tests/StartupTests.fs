namespace MassConvert.Tests

open FunToolbox.FileSystem

open NUnit.Framework
open FsUnit

open MassConvert
open MassConvert.Core

[<AutoOpen>]
module Helper =
    let startupArgumentsFromCommandLine cl = 
        StartupArguments.fromCommandLine (Path.currentDirectory()) cl

[<TestFixture>]
type CommandLineArgumentsTests() =

    [<Test>]
    member this.parsesArguments() = 
        let args =
            [| "--config"; "c:/configFile"; "--convert" |]
            |> startupArgumentsFromCommandLine

        args.configFile |> should equal (Path "c:/configFile")
        args.mode |> should equal Mode.Convert

    [<Test>]
    member this.acceptsRelativeConfigFilePath() = 
        let args =
            [| "--config"; "configFile" |]
            |> startupArgumentsFromCommandLine

        args.configFile |> Path.name |> should equal "configFile"
    
    [<Test>]
    member this.modeIsDryRunWhenConvertIsNotSpecified() =
        let args =
            [| "--config"; "c:/config" |]
            |> startupArgumentsFromCommandLine

        args.mode |> should equal Mode.DryRun
        
[<TestFixture>]
type ConfigFileTests() = 
    [<Test>]
    member this.readsConfigFile() = 
        let args =
            [| "--config"; "config.yaml" |]
            |> startupArgumentsFromCommandLine

        let config = 
            args
            |> Main.readConfiguration

        config.source.path |> Path.name |> should equal "sourcePath"
        config.destination.path |> Path.name |> should equal "destinationPath"
        config.source.pattern |> should equal ("*.wav" |> GlobPattern.parse)
        config.destination.extension |> should equal (".sln" |> FileExtension.parse)
        config.destination.purge |> should equal PurgeNone

        
                
        