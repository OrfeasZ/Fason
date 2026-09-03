# Fason

An F# code generator that generates optimized (and Thoth-compatible) JSON encoders and decoders for F# types, without
using reflection. Only supports F# types and .NET primitives (e.g. records, tuples, DUs, collections). Compatible with
Fable.

## Usage

First, add `Fason` as a dependency to your project via NuGet. You need to also install the Fason code generation tool by
running `dotnet tool install Fason.Tool`.

Then, annotate any types you want JSON encoders / decoders to be generated for with the `FasonSerializable` attribute.
For example:

```fsharp
open Fason

[<FasonSerializable>]
type MyRecord = {
    hello: string
    world: int
}
```

To generate the encoders / decoders, run `dotnet fason MyProject.fsproj`, where `MyProject.fsproj` is the path to your
F# project. This will create a `Fason.Generated.fs` file next to your project file. Include that file in your project,
and then use the generated `Json.serialize`, `Json.deserialize<'T>`, `Json.serializeObj`, and `Json.deserializeObj` to
convert your values between JSON and back. The `Obj`-suffixed methods take an `obj` and its `Type` instead of a concrete
object, for cases where it might not be known at runtime.

## Options

The Fason tool has the following options:

```shell
dotnet fason MyProject.fsproj [--namespace <name>] [--output <path>] [--configuration <name>] [--hermes] [--version]
```

| Option                   | Description                                                                                                       |
|--------------------------|-------------------------------------------------------------------------------------------------------------------|
| `--namespace <name>`     | Allows specifying the namespace the generated F# code will be put under.                                          |
| `--output <path>`        | Specify the path the generated code will be placed under. Can be a directory or a full path with a filename.      |
| `--configuration <name>` | Build configuration the project is loaded with (default: Debug). Referenced projects must already be built in it. |
| `--hermes`               | Optimize the generated code for usage with the Hermes JS engine (probably also helps with other JS interpreters). |
| `--version`              | Print the tool version and exit.                                                                                  |

## Benchmarks

Below are some benchmarks compared to Thoth's auto coders on different runtimes, averaged across 5 runs with many
iterations. Those are against several documents of varying complexities and sizes (most are from sensitive production
data and are not included in this repo). Keep in mind that this is just runtime encoding / decoding performance. Since
this tool generates code, you're basically trading build time for this. In a real-world project I've been testing this
on, the generated file added ~2s to the build time. Generating the code itself took an additional ~10s.

### .NET 10

| Document | Serialize | Thoth   | Deserialize | Thoth   |
|----------|-----------|---------|-------------|---------|
| 84 B     | 0.22 µs   | 15 µs   | 0.36 µs     | 15 µs   |
| 10 KB    | 8.8 µs    | 2.8 ms  | 14 µs       | 8.0 ms  |
| 100 KB   | 72 µs     | 23 ms   | 151 µs      | 67 ms   |
| 633 KB   | 0.85 ms   | 127 ms  | 1.08 ms     | 318 ms  |
| 5.7 MB   | 8.0 ms    | 1203 ms | 15.7 ms     | 2355 ms |

### Fable + Node 22

| Document | Serialize | Thoth   | Deserialize | Thoth   |
|----------|-----------|---------|-------------|---------|
| 84 B     | 0.50 µs   | 1.10 µs | 0.50 µs     | 2.04 µs |
| 10 KB    | 25 µs     | 180 µs  | 30 µs       | 539 µs  |
| 100 KB   | 475 µs    | 1.30 ms | 403 µs      | 3.55 ms |
| 633 KB   | 1.18 ms   | 5.19 ms | 2.04 ms     | 12.8 ms |
| 5.7 MB   | 36.7 ms   | 75.2 ms | 32.9 ms     | 176 ms  |

### Fable + Hermes

| Document | Serialize | Thoth   | Deserialize | Thoth   |
|----------|-----------|---------|-------------|---------|
| 84 B     | 1.12 µs   | 5.18 µs | 0.72 µs     | 8.18 µs |
| 10 KB    | 67 µs     | 1.06 ms | 90 µs       | 2.09 ms |
| 100 KB   | 744 µs    | 5.08 ms | 957 µs      | 16.0 ms |
| 633 KB   | 3.18 ms   | 28.2 ms | 6.92 ms     | 66.2 ms |
| 5.7 MB   | 50.1 ms   | 257 ms  | 69.8 ms     | 829 ms  |
