open System.Threading
open System.Reflection
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Rendering.Text
open Aardwars

open FShade


[<EntryPoint>]
let main (args : string[]) =

    let texturesPath, mapPath = Elm.MapAssets.getFromEmbeddedResources() 

    let server, port =
        if args.Length > 0 then
            args.[0], int args.[1]
        else
            Elm.NetworkGroup.server Elm.App.port
            "localhost", Elm.App.port

    Aardvark.Init()
    let app = new OpenGlApplication(true, DebugLevel.None)

    let fld = typeof<OpenGlApplication>.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance) |> Array.find (fun f -> f.Name.Contains "windowConfig")
    let cfg = fld.GetValue(app) :?> Aardvark.Glfw.WindowConfig
    fld.SetValue(app, { cfg with vsync = false })

    let win = app.CreateGameWindow(4)
    let client = Elm.NetworkGroup.client server port

    do 
        let initial = Elm.Game.intitial client texturesPath mapPath
        let app = Elm.App.create initial (Update.update client) (Elm.Game.view client)
        Elm.App.run win app
        exit 0
    0