# Fason

An F# code generator that generates optimized (and Thoth-compatible) JSON encoders and decoders for F# types, without
using reflection. Supports F# types (records, unions, tuples, collections, etc.) and .NET primitives and core types.
See [Supported types](#supported-types) below for the full list. Compatible with Fable.

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
F# project. This will create a `Fason.Generated.fs` file next to your project file. You can pass several projects at
once by separating them with spaces.

Include the generated file in your project and call the generated `Codecs.Register()` once at startup, before any
JSON conversion. Then use `Json.serialize`, `Json.deserialize<'T>`, `Json.serializeObj`, and `Json.deserializeObj`
from the `Fason` namespace to convert your values between JSON and back. The `Obj`-suffixed functions take a `Type` and
an `obj` instead of a concrete object, for cases where it might not be known at runtime.

```fsharp
open Fason

MyProject.Serialization.Codecs.Register()

let json = Json.serialize { hello = "hi"; world = 1 }
let value = Json.deserialize<MyRecord> json
let jsonObj = Json.serializeObj typeof<MyRecord> (box value)
```

## Attributes

Fason looks for three attributes, all in the `Fason` namespace:

| Attribute           | Description                                                                                                                                                                                                                      |
|---------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `FasonSerializable` | Marks a type to generate encoders / decoders for, together with every type it depends on. On a module, it applies to everything inside. On an interface, codecs are generated for the parameter and return types of its members. |
| `FasonUnwrap`       | Generates encoders / decoders for the type arguments of a type instead of itself. `Task<'T>`, `ValueTask<'T>` and `Async<'T>` are treated this way by default.                                                                   |
| `FasonIgnore`       | Marks a type that is skipped (no encoders / decoders are generated for it). Types that depend on it get none either.                                                                                                             |

For example, this makes Fason generate codecs for `Request`, `Response` and `ServerStatus`, but not for `Session`,
`Audit` or `Deferred<'T>`:

```fsharp
namespace MyNamespace

open System.Threading.Tasks
open Fason

type ServerStatus = { ok: bool }

[<FasonSerializable>]
module Api =
    type Request = { id: int }
    type Response = { ok: bool }

    [<FasonIgnore>]
    type Session = { token: string }

    type Audit = { session: Session; at: System.DateTime }

    [<FasonUnwrap>]
    type Deferred<'T>(compute: unit -> 'T) =
        member _.Value = compute ()

    type IApi =
        abstract member Send: request: Request -> Task<Response>
        abstract member Status: unit -> Deferred<ServerStatus>
```

## Supported types

Fason supports generating encoders / decoders for these types, and for any combination of them:

| Kind             | Types                                                                                                                                               | JSON                                                                                                      |
|------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------|
| Numbers          | `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`, `single`, `double`, `decimal`                                             | A number. `int64`, `uint64` and `decimal` are quoted, so JavaScript doesn't lose precision.               |
| Other primitives | `bool`, `char`, `string`, `unit`                                                                                                                    | `unit` is `null`.                                                                                         |
| Core types       | `Guid`, `DateTime`, `DateTimeOffset`, `DateOnly`, `TimeOnly`, `TimeSpan`                                                                            | Encoded as strings. Dates and times in ISO 8601, `TimeSpan` its default format.                           |
| Records          | Records, anonymous records, generic records.                                                                                                        | Fields that are `None` or `ValueNone` are omitted.                                                        |
| Unions           | Discriminated unions, generic unions, `Result<'T, 'TError>`                                                                                         | The case name as a string, or an array of the case name string followed by its fields.                    |
| Enums            | Enums over any of the supported integer types                                                                                                       | The underlying number. Reading also accepts the name of the value as a string.                            |
| Tuples           | Tuples of any arity                                                                                                                                 | Encoded as an array.                                                                                      |
| Collections      | `'T array`, `'T list`, `'T seq`, `Set<'T>`, `Map<'K, 'V>`                                                                                           | Encoded as an array. Maps with `string` or `Guid` keys are objects, other maps are arrays of pair arrays. |
| Options          | `'T option`, `'T voption`                                                                                                                           | The value, or `null`.                                                                                     |
| Units of measure | Any supported type with a unit of measure, for example `float<kg>` or `string<userId>`. For string UoMs Fason expects `FSharp.UMX` to be available. | The same as the type without the UoM.                                                                     |
| Interfaces       | Interfaces marked `FasonSerializable` (or anywhere down the chain of something marked as such).                                                     | No codecs for the interface itself, only for the argument and return types of its member functions.       |

## Options

The Fason tool has the following options:

```shell
dotnet fason MyProject.fsproj... [--namespace <name>] [--output <path>] [--configuration <name>] [--hermes] [--watch] [--version]
```

| Option                   | Description                                                                                                                       |
|--------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| `--namespace <name>`     | Allows specifying the namespace the generated F# code will be put under. Single project only.                                     |
| `--output <path>`        | Specify the path the generated code will be placed under. Can be a directory or a full path with a filename. Single project only. |
| `--configuration <name>` | Build configuration the project is loaded with (default: Debug). Referenced projects must already be built in it.                 |
| `--hermes`               | Optimize the generated code for usage with the Hermes JS engine (probably also helps with other JS interpreters).                 |
| `--watch`                | Keep running and regenerate whenever a source file of a loaded project changes.                                                   |
| `--version`              | Print the tool version and exit.                                                                                                  |

Instead of `--namespace` and `--output`, a project can set the `FasonNamespace` and `FasonOutput` MSBuild properties.
A relative `FasonOutput` is resolved against the project directory. This is the way to configure several projects
in one run.

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
