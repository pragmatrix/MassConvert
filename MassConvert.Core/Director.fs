namespace MassConvert.Core

open JobCreator

module Director =
    
    let shouldProcessBottomUp job = 
        match job with
        | PurgeDirectory _
        | PurgeFile _ -> true
        | _ -> false

    let shouldProcessTopDown job = not (shouldProcessBottomUp job)

    let filterJobs f (names, jobs) = names, jobs |> List.filter f
    let isNotEmpty (names, jobs : _ list) = jobs.IsEmpty |> not

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
    


    