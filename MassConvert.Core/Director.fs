namespace MassConvert.Core

open System
open Entrepeneur

open FunToolbox.FileSystem

module Director =
    
    let shouldProcessBottomUp job = 
        match job with
        | MayRemoveDestinationDirectory _
        | MayRemoveDestinationFile _ -> true
        | _ -> false

    let shouldProcessTopDown job = not (shouldProcessBottomUp job)

    let filterJobs f (names, jobs) = names, jobs |> List.filter f

    type Composition = {
        environment: JobEnvironment
        jobs: (Fragments * Job list) list
    }

    let composeFromJobTree (tree: JobTree) = 
        let env = tree.list.environment
        let all = tree.allTopDown
        let bottomUp = all |> List.rev |> List.map (filterJobs shouldProcessBottomUp)
        let topDown = all |> List.map (filterJobs shouldProcessTopDown)
        {
            environment = env
            jobs = bottomUp @ topDown
        }
    


    