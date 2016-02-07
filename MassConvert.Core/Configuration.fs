namespace MassConvert.Core

open System
open System.IO

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

type Configuration = {
    source: Path
    destination: Path

    sourceFilePattern: GlobPattern
    destinationFileExtension: FileExtension

    commandLine: string

    convertedFiles: ConvertedFiles
    createdDirectories: CreatedDirectories
}
