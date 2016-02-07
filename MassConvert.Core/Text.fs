namespace MassConvert.Core

// note: this code is duplicated from the IVR.Tools project.

open System.Collections.Generic

open SmartFormat
open SmartFormat.Core.Extensions

module Template =

    let private formatter = 
        // Create a formatter that is as conservative as possible!
        let formatter = SmartFormatter(Core.Settings.ErrorAction.ThrowError)
        formatter.AddExtensions(
            [|
                // we don't want to bind template argument names to names
                // derived from reflection
                // Extensions.ReflectionSource(formatter) :> ISource
                Extensions.DictionarySource(formatter) :> ISource
            |])
        formatter.AddExtensions(
            [|
                new Extensions.DefaultFormatter() :> IFormatter
            |])
        formatter

    type Arguments = ArgumentList of (string * string) list
        with
        member this.value = let (ArgumentList a) = this in a

    [<CR(ModuleSuffix)>]
    module Arguments =
        let empty = ArgumentList []
        let ofList l = ArgumentList l
        let add name value (args: Arguments) = 
            (name, value) :: args.value
            |> ArgumentList
        let merge (ArgumentList argb) (ArgumentList argsa) = 
            argb @ argsa
            |> ArgumentList

    let private format (template: string) (args: Arguments) = 

        let args =
            match args with
            | ArgumentList lst ->
                let dict = Dictionary<_, _>()
                // don't add duplicates, first one wins.
                for e in lst do
                    let key = fst e
                    if dict.ContainsKey key |> not then
                        dict.Add(key, snd e)
                dict :> obj

        formatter.Format(template, args)        

    type Arguments with
        member this.format str = 
            format str this