open System.IO
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo

let main () =
    async {
        let projectDirectory = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, ".."))

        let projectPath =
            Path.Combine(projectDirectory.FullName, "Fason.TestLib/Fason.TestLib.fsproj")
            |> Path.GetFullPath

        let toolsPath = Init.init projectDirectory None

        let defaultLoader = WorkspaceLoader.Create(toolsPath, [])

        let projectOptions = defaultLoader.LoadProjects([ projectPath ]) |> Seq.toArray
        
        if projectOptions.Length = 0 then
            failwith "Failed to load project"
        else
            let options = FCS.mapToFSharpProjectOptions projectOptions[0] []

            let checker = FSharpChecker.Create(keepAssemblyContents = true)
            let! result = checker.ParseAndCheckProject options

            printfn $"Check results: %A{result}"
    }
    
main () |> Async.RunSynchronously