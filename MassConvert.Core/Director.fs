namespace MassConvert.Core

open FunToolbox.FileSystem

open JobCreator

/// Orders jobs and converts them to actions
module Director =
    
    let shouldProcessBottomUp job = 
        match job with
        | PurgeDirectory _
        | PurgeFile _ -> true
        | _ -> false

    let shouldProcessTopDown job = not (shouldProcessBottomUp job)

    let filterJobs f (names, jobs) = names, jobs |> List.filter f
    let isNotEmpty (_, jobs : _ list) = jobs.IsEmpty |> not

    type Composition = {
        environment: JobEnvironment
        jobs: (Fragments * Job list) list
    } with
        member this.print() : string seq = 
            seq {
                for (fragments, jobList) in this.jobs do
                    yield fragments.string + ":"
                    for job in jobList do
                        yield "  " + job.string
            }

    let composeFromJobTree (tree: JobTree) = 
        let env = tree.list.environment
        let all = tree.allTopDown
        let bottomUp = 
            all 
            |> List.rev 
            |> List.map (filterJobs shouldProcessBottomUp) 
            |> List.filter isNotEmpty
        let topDown = 
            all 
            |> List.map (filterJobs shouldProcessTopDown) 
            |> List.filter isNotEmpty
        {
            environment = env
            jobs = bottomUp @ topDown
        }
    
    type Action = 
        | TryRemoveFile of Path        
        | TryRemoveDir of Path
        | ConvertCommand of inputFile: Path * outputFile: Path

    let compositionToActions (composition: Composition) = 
        let environment = composition.environment

        [
            for (fragments, jobs) in composition.jobs do
                let src = fragments.path environment.source
                let dst = fragments.path environment.destination
                for job in jobs do
                    yield
                        match job with
                        | PurgeDirectory dn -> 
                            TryRemoveDir (dst |> Path.extend dn.value)
                        | PurgeFile fn -> 
                            TryRemoveFile (dst |> Path.extend fn.string)
                        | ConvertFile (stem, srcExt, dstExt) -> 
                            ConvertCommand (src |> Path.extend (stem + srcExt.value), dst |> Path.extend (stem + dstExt.value))
        ]
            
          
    