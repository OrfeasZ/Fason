open System
open System.Collections.Concurrent
open System.IO
open System.Threading
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Fason
open Ionide.ProjInfo

let usage =
    """Usage: fason <project.fsproj>... [options]

Generates JSON encoders and decoders for the types of each project that carry [<FasonSerializable>].

Options:
  --namespace <name>   Namespace of the generated code (default: <ProjectName>.Serialization).
                       Only allowed with a single project.
  --output <path>      File to write, or a directory that Fason.Generated.fs gets written in
                       (default: Fason.Generated.fs next to the project). Only allowed with a single project.
  --configuration <c>  Build configuration to load the projects with (default: Debug).
                       Referenced projects must already be built in this configuration.
  --hermes             Emit code optimized for the Hermes JS engine
  --watch              Keep running and regenerate when a source file of any loaded project changes
  --version            Print the tool version

The FasonNamespace and FasonOutput MSBuild properties of a project set the same things per project.
A relative FasonOutput is resolved against the project directory."""

let version =
    let attr =
        System.Reflection.Assembly
            .GetExecutingAssembly()
            .GetCustomAttributes(typeof<System.Reflection.AssemblyInformationalVersionAttribute>, false)
        |> Seq.tryHead

    match attr with
    | Some(:? System.Reflection.AssemblyInformationalVersionAttribute as a) -> a.InformationalVersion.Split('+')[0]
    | _ -> "unknown"

let attributeName = "FasonSerializable"

type Target =
    { projectPath: string
      ns: string
      outputPath: string
      options: FSharpProjectOptions }

/// The last source file that mentions the attribute. Every type a serializable type refers to
/// must come before it, so checking up to this file is enough.
let lastAnnotatedFile (sourceFiles: string array) =
    sourceFiles
    |> Array.filter (fun f -> File.Exists f && (File.ReadAllText f).Contains attributeName)
    |> Array.tryLast

let writeIfChanged (path: string) (content: string) =
    if File.Exists path && File.ReadAllText path = content then
        "unchanged"
    else
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        File.WriteAllText(path, content)
        $"written to {path}"

let generate (checker: FSharpChecker) (target: Target) =
    match lastAnnotatedFile target.options.SourceFiles with
    | None -> printfn $"{target.projectPath}: no file mentions {attributeName}, nothing generated."
    | Some file ->
        let started = Diagnostics.Stopwatch.StartNew()
        let source = SourceText.ofString (File.ReadAllText file)

        let _, answer =
            checker.ParseAndCheckFileInProject(file, 0, source, target.options)
            |> Async.RunSynchronously

        match answer with
        | FSharpCheckFileAnswer.Aborted -> eprintfn $"{target.projectPath}: type check aborted."
        | FSharpCheckFileAnswer.Succeeded result ->
            for diagnostic in result.Diagnostics do
                eprintfn $"{diagnostic}"

            TypeCollector.reset ()

            for entity in result.PartialAssemblySignature.Entities do
                TypeCollector.collectFrom entity

            let serializableTypes = TypeCollector.getSerializableTypes ()
            let code = JsonEncoderCodegen.generate (serializableTypes, target.ns)
            let status = writeIfChanged target.outputPath code

            printfn
                $"{target.projectPath}: {serializableTypes.Length} types, {started.ElapsedMilliseconds} ms, {status}."

let main (argv: string array) =
    JsonEncoderCodegen.hermes <- argv |> Array.contains "--hermes"

    let option (name: string) =
        match argv |> Array.tryFindIndex ((=) name) with
        | Some i when i + 1 < argv.Length -> Some argv[i + 1]
        | _ -> None

    let optionValues =
        [ "--namespace"; "--output"; "--configuration" ] |> List.choose option |> set

    let projectPaths =
        argv
        |> Array.filter (fun a -> not (a.StartsWith "-") && not (optionValues.Contains a))
        |> Array.map Path.GetFullPath
        |> Array.toList

    if
        projectPaths.Length > 1
        && (option "--namespace" |> Option.isSome || option "--output" |> Option.isSome)
    then
        failwith "--namespace and --output can only be used with a single project"

    let targetOf (loaded: Types.ProjectOptions list) (project: Types.ProjectOptions) =
        let projectPath = Path.GetFullPath project.ProjectFileName
        let projectDir = Path.GetDirectoryName projectPath

        let property name =
            project.CustomProperties
            |> List.tryFind (fun p -> p.Name = name)
            |> Option.map _.Value
            |> Option.filter (String.IsNullOrWhiteSpace >> not)

        let outputPath =
            match option "--output" |> Option.map Path.GetFullPath with
            | Some path -> path
            | None ->
                property "FasonOutput"
                |> Option.map (fun p -> Path.GetFullPath(Path.Combine(projectDir, p)))
                |> Option.defaultValue projectDir

        { projectPath = projectPath
          ns =
            option "--namespace"
            |> Option.orElse (property "FasonNamespace")
            |> Option.defaultValue (Path.GetFileNameWithoutExtension projectPath + ".Serialization")
          outputPath =
            if Directory.Exists outputPath || not (Path.HasExtension outputPath) then
                Path.Combine(outputPath, "Fason.Generated.fs")
            else
                outputPath
          options = FCS.mapToFSharpProjectOptions project loaded }

    match Paths.dotnetRoot.Value with
    | None -> failwith "Failed to find dotnet root. Is dotnet in your PATH?"
    | Some dotnetRoot ->
        // Setup the SDK to use manually, based on the version this project is compiled against.
        // We do this because otherwise Ionide will use the latest SDK, which can cause compatibility issues.
        let sdks = SdkDiscovery.sdks dotnetRoot

        let sdkToUse =
            sdks
            |> Array.filter (fun sdk -> sdk.Version.Major = Environment.Version.Major)
            |> Array.maxBy _.Version

        Init.setupForSdkVersion sdkToUse.Path dotnetRoot
        let toolsPath = Types.ToolsPath(Path.Combine(sdkToUse.Path.FullName, "MSBuild.dll"))

        let globalProperties =
            match option "--configuration" with
            | Some configuration -> [ "Configuration", configuration ]
            | None -> []

        let loader = WorkspaceLoader.Create(toolsPath, globalProperties)

        let load () =
            printfn $"Loading {projectPaths.Length} project(s) with SDK {sdkToUse.Version}"

            let loaded =
                loader.LoadProjects(projectPaths, [ "FasonOutput"; "FasonNamespace" ], BinaryLogGeneration.Off)
                |> Seq.toList

            let targets =
                [ for path in projectPaths ->
                      match loaded |> List.tryFind (fun p -> Path.GetFullPath p.ProjectFileName = path) with
                      | Some project -> targetOf loaded project
                      | None -> failwith $"Failed to load {path}" ]

            loaded, targets

        let loaded, targets = load ()
        let checker = FSharpChecker.Create(projectCacheSize = loaded.Length)

        let generateAll targets =
            for target in targets do
                try
                    generate checker target
                with ex ->
                    eprintfn $"{target.projectPath}: {ex.Message}"

        generateAll targets

        if argv |> Array.contains "--watch" then
            let watchedFiles (loaded: Types.ProjectOptions list) =
                [ for p in loaded do
                      yield p.ProjectFileName
                      yield! p.SourceFiles ]
                |> List.map Path.GetFullPath
                |> set

            let changes = new BlockingCollection<string>()

            let watchers =
                [ for directory in loaded |> List.map (_.ProjectFileName >> Path.GetDirectoryName) |> List.distinct ->
                      let watcher = new FileSystemWatcher(directory, IncludeSubdirectories = true)
                      watcher.Changed.Add(fun e -> changes.Add e.FullPath)
                      watcher.Created.Add(fun e -> changes.Add e.FullPath)
                      watcher.Deleted.Add(fun e -> changes.Add e.FullPath)
                      watcher.Renamed.Add(fun e -> changes.Add e.FullPath)
                      watcher.EnableRaisingEvents <- true
                      watcher ]

            printfn "Watching for changes. Press Ctrl+C to stop."

            try
                let mutable watched = watchedFiles loaded
                let mutable targets = targets

                while true do
                    let batch = ResizeArray [ changes.Take() ]

                    // Editors write in several steps. Let them finish before reading.
                    Thread.Sleep 200
                    let mutable path = ""

                    while changes.TryTake(&path) do
                        batch.Add path

                    let changed =
                        batch |> Seq.map Path.GetFullPath |> Seq.filter watched.Contains |> Seq.toList

                    if changed |> List.exists (fun p -> p.EndsWith ".fsproj") then
                        let loaded, reloaded = load ()
                        watched <- watchedFiles loaded
                        targets <- reloaded

                    if not changed.IsEmpty then
                        generateAll targets
            finally
                for watcher in watchers do
                    watcher.Dispose()

[<EntryPoint>]
let realMain argv =
    if argv |> Array.contains "--version" then
        printfn $"{version}"
        0
    elif argv.Length = 0 || argv[0].StartsWith "-" then
        eprintfn $"{usage}"
        1
    else
        main argv
        0
