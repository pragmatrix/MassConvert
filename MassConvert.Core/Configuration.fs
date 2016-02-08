namespace MassConvert.Core

open System
open System.IO

open System.Collections.Generic

open FunToolbox.FileSystem

type FileExtension = FileExtension of string
    with
    /// Returns the extension as a string (including the leading dot)
    member this.value = let (FileExtension ext) = this in ext
    /// Parses a file extension. Expects the string to start with a dot and at least one character after that
    static member parse (ext: string) = 
        if ext.Length < 2 || ext.[0] <> '.' then
            failwith "%s: no extension, expect '.' at the beginning and at least one character after that"        
        FileExtension ext

type FileName = FileName of stem: string * ext: string
    with
    member this.value = let (FileName (stem, ext)) = this in (stem, ext)
    member this.nameWithoutExtension = this.value |> fst
    member this.extension = this.value |> snd
    member this.string = this.nameWithoutExtension + this.extension
    static member parse str = 
        let stem = Path.GetFileNameWithoutExtension(str)
        let ext = Path.GetExtension(str)
        FileName (stem, ext)

type DirectoryName = DirectoryName of string
    with 
    member this.value = let (DirectoryName name) = this in name
    static member parse str =   
        DirectoryName str

type Fragments = Fragments of DirectoryName list
    with
    member this.value = let (Fragments fragments) = this in fragments
    member this.string = this.value |> List.rev |> List.map(fun dn -> dn.value) |> (fun strs -> String.Join("/", strs))
    member this.push dn = 
        dn :: this.value
        |> Fragments
    static member root = Fragments []
    member this.path root = 
        root
        |> Path.extend this.string

type GlobPattern = GlobPattern of string 
    with 
    member this.value = let (GlobPattern value) = this in value
    static member parse str = 
        GlobPattern str
    static member all = "*" |> GlobPattern.parse
    static member forExtension (ext: FileExtension) = "*" + ext.value |> GlobPattern.parse

    // more:
    // https://github.com/mganss/Glob.cs

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
                    path = destination|> parse "path" parseDir
                    extension = destination |> parse "extension" FileExtension.parse
                    purge = destination |> parse "purge" PurgeMode.parse
                }

            command = 
                {
                    template = command |> parse "template" id
                }                
        }
