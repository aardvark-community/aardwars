#r "nuget: Fake.Core.ReleaseNotes"
#r "nuget: Fake.DotNet.Cli"
open System.Configuration
open Fake.Core
open Fake.DotNet


let notes = ReleaseNotes.load "RELEASE_NOTES.md"
let version = notes.NugetVersion


"src/Aardwars/tool/aardwars.fsproj" |> DotNet.pack (fun o ->
    { o with
        Configuration = DotNet.BuildConfiguration.Release
        OutputPath = Some "bin/pack"
        MSBuildParams = { o.MSBuildParams with Properties = ["Version", version; "ReleaseNotes", String.concat "; " notes.Notes] } 
    }
)