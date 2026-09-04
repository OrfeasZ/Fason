module Fason.Log

open System

let private red = 31
let private green = 32
let private yellow = 33

// Don't show colors when redirecting the console output (e.g. to a file, CI, etc.)
let private outputColors = not Console.IsOutputRedirected
let private errorColors = not Console.IsErrorRedirected

let private colored (enabled: bool) (color: int) (line: string) =
    if enabled then $"\027[{color}m{line}\027[0m" else line

let info (line: string) =
    printfn $"{colored outputColors green line}"

let error (line: string) =
    eprintfn $"{colored errorColors red line}"

let warning (line: string) =
    eprintfn $"{colored errorColors yellow line}"
