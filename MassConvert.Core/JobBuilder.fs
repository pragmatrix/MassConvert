namespace MassConvert.Core

open System.IO

open FunToolbox.FileSystem

open Scanner
open Diff

module JobBuilder = 

    type Job = 
        | PurgeDirectory of RelativePath * DirectoryName
        | PurgeFile of RelativePath * FileName
        | ConvertFile of RelativePath * FileName
        with 
        member this.string =
            match this with
            | PurgeDirectory (p, dn) -> "purge   " + (p.extend dn).string + " (dir)"
            | PurgeFile (p, fn) -> "purge   " + (p.extend fn).string
            | ConvertFile (p, fn) -> "convert " + (p.extend fn).string
                
        member this.isRequired (mode: PurgeMode) = 
            match this with
            | PurgeDirectory _
            | PurgeFile _ -> mode = PurgeRemoved
            | _ -> true        
    
    type ScanResultDiff with
        /// returns all the files that can be purged from the destination (bottom up)
        member this.getFilesThatCanBePurged() = 
            [
                yield!
                    this.nested
                    |> List.map (fun (_, d) -> d.getFilesThatCanBePurged())
                    |> List.flatten

                yield!
                    this.filesStemDiff.remove
                    |> List.map 
                        (fun stem -> this.relative, this.sourceFiles.filename stem)
            ]

        /// returns all the directories that can be purged from the destination (bottom up)
        member this.getDirectoriesThatCanBePurged() = 
            [
                yield!
                    this.nested
                    |> List.map (fun (_, d) -> d.getDirectoriesThatCanBePurged())
                    |> List.flatten

                yield!
                    this.directoriesDiff.remove
                    |> List.map (fun dn -> this.relative, dn)
            ]

        /// returns all potential file conversion jobs (top down)
        member this.getFileConversions() = 
            [            
                yield!
                    this.filesStemDiff.add @ this.filesStemDiff.update 
                    |> List.map (
                        fun stem ->
                            this.relative,
                            this.destinationFiles.filename stem
                        )           

                yield! 
                    this.nested
                    |> List.map (fun (_, d) -> d.getFileConversions())
                    |> List.flatten
            ]
            
    
    /// Builds a list of jobs that are optimized to only relate to the given element path. If
    /// the element path is empty, a complete list of jobs is returned.
    /// The element path may point to a file, a directory or may not exist at all. The element is
    /// used to limit the directory scan in case changes were only detected regarding that element.

    let forRelativePath (configuration: Configuration) (path: RelativePath) : Job list = 
        let srcRoot = configuration.source.path
        let dstRoot = configuration.destination.path
        let dstExt = configuration.destination.extension
        let common = findCommonRelativePath srcRoot dstRoot path

        let scanDirector = DirectedScan (path.parts |> List.skip (common.parts.Length))
        let sourceScan = 
            let srcPattern = configuration.source.pattern
            Scanner.scanDirectory srcRoot common srcPattern scanDirector
        let destinationScan = 
            let dstPattern = configuration.destination.extension |> GlobPattern.forExtension
            Scanner.scanDirectory dstRoot common dstPattern scanDirector

        // note the diff is inversed, our jobs actually describe a transformation
        // from the destination file hierarchy to the source
        let diff = 
            Diff.scanResults destinationScan sourceScan

        let purgeJobs = 
            match configuration.destination.purge with
            | PurgeNone -> []
            | PurgeRemoved ->
            [
                yield! 
                    diff.getFilesThatCanBePurged()
                    |> List.map PurgeFile
                yield! diff.getDirectoriesThatCanBePurged()
                    |> List.map PurgeDirectory
            ]


        let convertJobs = 
            let needsConversion (path: RelativePath, srcFile: FileName) = 
                let srcPath = path.mkAbsolute srcRoot
                let dstPath = path.mkAbsolute dstRoot
                let stem = srcFile.nameWithoutExtension
                let dstFile = FileName (stem, dstExt.string)
                let src = srcPath |> Path.extend srcFile.string
                let dst = dstPath |> Path.extend dstFile.string
                // GetLastWriteTimeUtc(): 
                // https://msdn.microsoft.com/en-us/library/system.io.file.getlastwritetimeutc(v=vs.110).aspx
                // If the file described in the path parameter does not exist, this method returns 12:00 midnight, January 1, 1601 A.D. (C.E.) Coordinated Universal Time (UTC).
                // which is fine for us.
                File.GetLastWriteTimeUtc(src.value) > File.GetLastWriteTimeUtc(dst.value)

            diff.getFileConversions()
            |> List.filter needsConversion
            |> List.map ConvertFile

        purgeJobs @ convertJobs


                    
        
        
            
        
