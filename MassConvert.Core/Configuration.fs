namespace MassConvert.Core

open System
open System.IO
open System.Collections.Generic

open FunToolbox.FileSystem

type ConvertedFiles = 
    | TryDeleteWhenTheyAreMissingFromSource
    | KeepAll

type CreatedDirectories = 
    | TryDeleteWhenTheyAreMissingFromSource
    | KeepAll

[<AutoOpen>]
module Helper =
    let parse key (parser: 'v -> 'x) (dict: Dictionary<obj, obj>) =
        let found, value = dict.TryGetValue(key)
        if not found then
            failwithf "key '%s' not found" key
        try
            match value with
            | :? 'v as value -> value |> parser
            | _ -> failwithf "key '%s' is in the wrong format, expect %s" key (typeof<'v>.Name)
        with e ->
            failwithf "key '%s' can not be parsed: %s" key e.Message

    let section (key: string) (dict: Dictionary<obj, obj>) = 
        let found, value = dict.TryGetValue(key)
        if not found then
            failwithf "section %s not found" key
        match value with
        | :? Dictionary<obj, obj> as d ->
            d
        | _ -> failwithf "section '%s' is of an unexpected type: %s" key (value.GetType().Name)

type PurgeMode =
    | PurgeRemoved
    | PurgeNone
    with
    static member parse str = 
        match str with
        | "Removed" -> PurgeRemoved
        | "None" -> PurgeNone
        | _ -> str |> failwithf "unsupported purge mode: %s, valid values are 'None' and 'Removed'"

type SourceConfig = {
    path: Path
    pattern: GlobPattern
}

type DestinationConfig = {
    path: Path
    extension: FileExtension
    purge: PurgeMode
}

type CommandConfig = {
    template: string
}

type Configuration = {
    source: SourceConfig
    destination: DestinationConfig
    command: CommandConfig
} with
    static member fromDictionary (baseDir: Path) (dict: Dictionary<obj, obj>) = 
        let parseDir dir = baseDir |> Path.extend dir
        let source = dict |> section "source"
        let destination = dict |> section "destination"
        let command = dict |> section "command"
        {
            source = 
                {
                    path = source |> parse "path" parseDir
                    pattern = source |> parse "pattern" GlobPattern.parse
                }
                
            destination = 
                {
                    path = destination |> parse "path" parseDir
                    extension = destination |> parse "extension" FileExtension.parse
                    purge = destination |> parse "purge" PurgeMode.parse
                }

            command = 
                {
                    template = command |> parse "template" id
                }                
        }
