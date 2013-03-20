#I "../packages/FSharp.Formatting.1.0.13/lib/net40"
#load "../packages/FSharp.Formatting.1.0.13/literate/literate.fsx"
open FSharp.Literate
open System.IO

let source = __SOURCE_DIRECTORY__
let template = Path.Combine(source, "../packages/FSharp.Formatting.1.0.13/literate/templates/template-file.html")
let output = Path.Combine(source, "../packages/FSharp.Formatting.1.0.13/literate/Script.html")

let script = Path.Combine(source, "Post1.fsx")
Literate.ProcessScriptFile(script, template, output)