open System.IO
open FSharp.Compiler.CodeAnalysis
open Fason
open Ionide.ProjInfo

let main () =
    async {
        let projectPath =
            Path.Combine(__SOURCE_DIRECTORY__, "../Fason.TestLib/Fason.TestLib.fsproj")
            |> Path.GetFullPath

        match Paths.dotnetRoot.Value with
        | None -> failwith "Failed to find dotnet root"
        | Some dotnetRoot ->
            // Setup the SDK to use manually, based on the version this project is compiled against.
            // We do this because otherwise Ionide will use the latest SDK, which can cause compatibility issues.
            let sdks = SdkDiscovery.sdks dotnetRoot

            let sdkToUse =
                sdks
                |> Array.filter (fun sdk -> sdk.Version.Major = System.Environment.Version.Major)
                |> Array.maxBy _.Version

            Init.setupForSdkVersion sdkToUse.Path dotnetRoot
            let toolsPath = Types.ToolsPath(Path.Combine(sdkToUse.Path.FullName, "MSBuild.dll"))

            // Try to load the target project.
            let defaultLoader = WorkspaceLoader.Create(toolsPath, [])
            let projectOptions = defaultLoader.LoadProjects([ projectPath ]) |> Seq.toArray

            if projectOptions.Length = 0 then
                failwith "Failed to load project"
            else
                let options = FCS.mapToFSharpProjectOptions projectOptions[0] []

                let checker = FSharpChecker.Create(keepAssemblyContents = true)
                let! result = checker.ParseAndCheckProject options

                // Collect all entities that have the FasonSerializable attribute.
                // We'll then recursively go through them and their children to collect all
                // types that we need to generated (de)serializers for.
                for entity in result.AssemblySignature.Entities do
                    let isFasonSerializable =
                        entity.Attributes
                        |> Seq.exists (fun attr ->
                            attr.AttributeType.TryGetFullName() = Some typeof<FasonSerializableAttribute>.FullName)

                    if isFasonSerializable then
                        entity |> TypeCollector.collectFrom

                let serializableTypes = TypeCollector.getSerializableTypes ()
                printfn $"Collected {serializableTypes.Length} serializable types."

                let code = JsonEncoderCodegen.generate serializableTypes
                printfn $"Generated code:\n{code}"
    }

[<EntryPoint>]
let realMain argv =
    main () |> Async.RunSynchronously
    0
