module Simendsjo.EmacsDotNet.ExposedLibrary

open System

// Ignore "warning FS0988: Main module of program is empty: nothing will happen when it is run"
// It's intentionally empty
#nowarn "0988"

// You don't need to include the library.
// You can supply your own type to avoid being bound by the license
// 
// open Simendsjo.EmacsDotNet.Lisp
type ExposeToEmacs() = inherit Attribute()
type ExposeToEmacsAsTopLevelKebabCase() = inherit ExposeToEmacs()

// Start with
//  dotnet run -p Simendsjo.EmacsDotNet.Console -- --expose-assembly ./Simendsjo.EmacsDotNet.ExposedLibrary/bin/Debug/net5.0/Simendsjo.EmacsDotNet.ExposedLibrary.dll true

// Run with
//  (dotnet hello-world "your name here")
[<ExposeToEmacsAsTopLevelKebabCase>]
let helloWorld (name : string) = $"Hello world, %s{name}!"
