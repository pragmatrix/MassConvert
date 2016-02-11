namespace MassConvert.Core

open System
open System.IO
open FunToolbox.FileSystem

/// Scans directories for files and subdirectories.
module Scanner = 

    type ScanResult = {
        root: Path
        relative: RelativePath
        files: FileName list
        nested: (DirectoryName * ScanResult) list
    } with
        static member empty root relative = {
                root = root
                relative = relative
                files = []
                nested = []
            }   

    type ScanDirector = 
        /// A directed scan scans the names specified and everything below them. Scanning stops
        /// as soon the first name can not be located.
        | DirectedScan of string list
        with
        member this.shouldScanDirectories() = true

        member this.shouldEnter (name: DirectoryName) =
            match this with
            | DirectedScan [] -> true
            | DirectedScan (head::_) when head = name.string -> true
            | _ -> false 

        member this.enter (name: DirectoryName) = 
            match this with
            | DirectedScan [] -> this
            | DirectedScan (head::rest) when head = name.string -> DirectedScan rest
            | _ -> failwithf "internal error"

        static member ScanAll = 
            DirectedScan []

    /// Returns the files and directories that are contained in the given directory. The files are filtered by 
    /// the glob pattern.
    let rec scanDirectory (root: Path) (relative: RelativePath) (pattern: GlobPattern) (director: ScanDirector) = 
        let path = relative.mkAbsolute root

        let files = FileSystem.Permissive.getFiles(path, pattern)

        let nested = 
            // tbd: we could specifically scan for one nested directory if DirectedScan is not empty.
            if not <| director.shouldScanDirectories() 
            then []
            else
            let names = FileSystem.Permissive.getDirectories(path)
            let scanDirectory (name: DirectoryName) = 
                name, scanDirectory root (relative.extend name) pattern (director.enter name)
            
            names
            |> List.filter director.shouldEnter
            |> List.map scanDirectory
            
        {
            root = root
            relative = relative
            files = files
            nested = nested
        }

    /// Find a relative directory below rootLeft and rootRight that _is_ or contains the element specified by
    /// the path.
    /// The element can point to a directory, a file or may not existing at all. If roota / rootb are not 
    /// existing directories, this method fails.
    /// Returns the relative path that points to the common root directory.
    let rec findCommonRelativePath (rootLeft: Path) (rootRight: Path) (path: RelativePath) : RelativePath =
        let left = path.mkAbsolute rootLeft
        let right = path.mkAbsolute rootRight
        if Directory.Exists(left.value) && Directory.Exists(right.value) 
        then path
        else
        match path.isEmpty with
        | true -> failwithf "can not find a common root, at least one of the root paths does not exist:\nleft: %s\nright: %s" rootLeft.value rootRight.value
        | false ->
        findCommonRelativePath rootLeft rootRight path.parent

    
        
        
    
