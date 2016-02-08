namespace MassConvert.Core

open System.IO
open FunToolbox.FileSystem

/// Scans directories for files and sub directories.
module Scanner = 

    type DirectoryScanResult = {
        path: Path
        files: FileName list
        directories: DirectoryName list
    } with
        static member empty path = {
                path = path
                files = []
                directories = []
            }   

    /// Returns the files and directories that are contained in the given directory. The files are filtered by 
    /// the glob pattern
    let scanDirectory (path: Path) (pattern: GlobPattern) = 
        let files = 
            Directory.GetFiles(path.value, pattern.value)
            |> Array.map (Path.parse >> Path.name >> FileName.parse)
            |> Array.toList
        let directories = 
            Directory.GetDirectories(path.value)
            |> Array.map (Path.parse >> Path.name >> DirectoryName.parse)
            |> Array.toList

        {
            path = path
            files = files
            directories = directories
        }

    
