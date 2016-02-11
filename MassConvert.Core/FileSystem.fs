namespace MassConvert.Core

open System
open System.IO

open FunToolbox.FileSystem

type FileExtension = FileExtension of string
    with
    member private this.value = let (FileExtension ext) = this in ext
    /// Returns the extension as a string (including the leading dot)
    member this.string = this.value
    /// Parses a file extension. Expects the string to start with a dot and at least one character after that
    static member parse (ext: string) = 
        if ext.Length < 2 || ext.[0] <> '.' then
            ext |> failwithf "%s: no extension, expect '.' at the beginning and at least one character after that"        
        FileExtension ext

type FileName = FileName of stem: string * ext: string
    with
    member private this.value = let (FileName (stem, ext)) = this in (stem, ext)
    member this.nameWithoutExtension = this.value |> fst
    member this.extension = this.value |> snd
    member this.string = this.nameWithoutExtension + this.extension
    static member parse str = 
        let stem = Path.GetFileNameWithoutExtension(str)
        let ext = Path.GetExtension(str)
        if stem = "" && ext = "" then
            failwith "filename can not be empty"
        FileName (stem, ext)

type DirectoryName = DirectoryName of string
    with 
    member private this.value = let (DirectoryName name) = this in name
    member this.string = this.value
    static member parse str =
        match str with
        | "" -> failwith "directory name can not be empty"
        | str -> DirectoryName str

/// Describes a relative path represented as a list of path parts. For performance reasons, the parts are
/// stored in reverse order.
type RelativePath = RelativePath of string list
    with 
    member private this.value = let (RelativePath value) = this in value
    member this.parts = this.value |> List.rev
    member this.string = this.parts |> (fun strs -> String.Join("/", strs))
    member this.isEmpty = this.value = []
    member this.mkAbsolute root = root |> Path.extend this.string
    member this.extend (path: RelativePath) = 
        RelativePath (path.value @ this.value)
    member this.extend (dir: DirectoryName) =
        RelativePath (dir.string :: this.value)
    member this.extend (file: FileName) = 
        RelativePath (file.string :: this.value)
    member this.parent = 
        match this.value with
        | [] -> failwith "empty relative path has no parent"
        | _::parent -> parent |> RelativePath
    static member empty = RelativePath []

    static member parse str =
        let rec ofString soFar str =
            let dir, name = Path.GetDirectoryName(str), Path.GetFileName(str)
            match dir with
            | null -> str |> failwithf "%s: expected to be a relative path"
            | "" -> name::soFar |> List.rev
            | dir -> ofString (name::soFar) dir

        ofString [] str |> RelativePath         

type GlobPattern = GlobPattern of string 
    with 
    member private this.value = let (GlobPattern value) = this in value
    member this.string = this.value
    static member parse str = GlobPattern str
    static member all = "*" |> GlobPattern.parse
    static member forExtension (ext: FileExtension) = "*" + ext.string |> GlobPattern.parse

    // more:
    // https://github.com/mganss/Glob.cs

module FileSystem = 

    /// Permissive file system functions that try to swallow errors when it makes sense.
    module Permissive =

        let getFiles(path: Path, pattern: GlobPattern) =
            try
                Directory.GetFiles(path.value, pattern.string)
                |> Array.map (Path.parse >> Path.name >> FileName.parse)
                |> Array.toList

            with
            | :? UnauthorizedAccessException ->
                // tbd: log
                []

        let getDirectories(path: Path) = 
            try
                Directory.GetDirectories(path.value)
                |> Array.map (Path.parse >> Path.name >> DirectoryName.parse)
                |> Array.toList
            with
            | :? UnauthorizedAccessException ->
                // tbd: log
                []

