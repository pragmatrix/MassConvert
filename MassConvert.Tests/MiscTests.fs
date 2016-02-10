namespace MassConvert.Tests

open NUnit.Framework
open FsUnit

open MassConvert.Core

[<TestFixture>]
type MiscTests() =
    [<Test>]
    member this.relativePathIsConstructedProperly() = 
        let input = "a/b/c"
        let path = 
            input
            |> RelativePath.parse
        
        path.parts |> should equal ["a"; "b"; "c"]
        path.string |> should equal input
        