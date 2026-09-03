open System.IO
open FSharp.Compiler.CodeAnalysis
open Fason
open Ionide.ProjInfo

let usage =
    """Usage: fason <project.fsproj> [options]

Generates JSON encoders and decoders for the types of the project that carry [<FasonSerializable>].

Options:
  --namespace <name>   Namespace of the generated code (default: <ProjectName>.Serialization)
  --output <path>      File to write, or a directory that Fason.Generated.fs gets written in
                       (default: Fason.Generated.fs next to the project)
  --configuration <c>  Build configuration to load the project with (default: Debug).
                       Referenced projects must already be built in this configuration.
  --hermes             Emit code optimized for the Hermes JS engine
  --version            Print the tool version"""

let version =
    let attr =
        System.Reflection.Assembly
            .GetExecutingAssembly()
            .GetCustomAttributes(typeof<System.Reflection.AssemblyInformationalVersionAttribute>, false)
        |> Seq.tryHead

    match attr with
    | Some(:? System.Reflection.AssemblyInformationalVersionAttribute as a) -> a.InformationalVersion.Split('+')[0]
    | _ -> "unknown"

let main (argv: string array) =
    async {
        let projectPath = Path.GetFullPath argv[0]

        JsonEncoderCodegen.hermes <- argv |> Array.contains "--hermes"

        let option (name: string) =
            match argv |> Array.tryFindIndex ((=) name) with
            | Some i when i + 1 < argv.Length -> Some argv[i + 1]
            | _ -> None

        let ns =
            option "--namespace"
            |> Option.defaultValue (Path.GetFileNameWithoutExtension projectPath + ".Serialization")

        let outputPath =
            match option "--output" with
            | Some path when not (Directory.Exists path) && Path.HasExtension path -> Path.GetFullPath path
            | Some directory -> Path.GetFullPath(Path.Combine(directory, "Fason.Generated.fs"))
            | None -> Path.Combine(Path.GetDirectoryName projectPath, "Fason.Generated.fs")

        match Paths.dotnetRoot.Value with
        | None -> failwith "Failed to find dotnet root. Is dotnet in your PATH?"
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

            printfn $"Loading {projectPath} with SDK {sdkToUse.Version}"

            let globalProperties =
                match option "--configuration" with
                | Some configuration -> [ "Configuration", configuration ]
                | None -> []

            let defaultLoader = WorkspaceLoader.Create(toolsPath, globalProperties)

            let projectOptions =
                defaultLoader.LoadProjects([ projectPath ], [], BinaryLogGeneration.Off)
                |> Seq.toArray

            if projectOptions.Length = 0 then
                failwith "Failed to load project"
            else
                let options = FCS.mapToFSharpProjectOptions projectOptions[0] []

                let checker = FSharpChecker.Create(keepAssemblyContents = true)
                let! result = checker.ParseAndCheckProject options

                for diagnostic in result.Diagnostics do
                    eprintfn $"{diagnostic}"

                for entity in result.AssemblySignature.Entities do
                    TypeCollector.collectFrom entity

                let serializableTypes = TypeCollector.getSerializableTypes ()
                printfn $"Collected {serializableTypes.Length} serializable types."

                let code = JsonEncoderCodegen.generate (serializableTypes, ns)

                Directory.CreateDirectory(Path.GetDirectoryName outputPath) |> ignore
                File.WriteAllText(outputPath, code)
                printfn $"Generated code written to {outputPath}."
    }

[<EntryPoint>]
let realMain argv =
    if argv |> Array.contains "--version" then
        printfn $"{version}"
        0
    elif argv.Length = 0 || argv[0].StartsWith "-" then
        eprintfn $"{usage}"
        1
    else
        main argv |> Async.RunSynchronously
        0
