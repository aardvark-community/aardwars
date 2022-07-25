namespace Aardwars

open System.Threading
open System.Reflection
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Rendering.Text

open FShade

module MainGame = 
    let startGame server port = 
        if server="localhost" then Elm.NetworkGroup.server Elm.App.port
        let texturesPath, mapPath = Elm.MapAssets.getFromEmbeddedResources()     
        Aardvark.Init()
        let app = new OpenGlApplication(true, DebugLevel.None)

        let fld = typeof<OpenGlApplication>.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance) |> Array.find (fun f -> f.Name.Contains "windowConfig")
        let cfg = fld.GetValue(app) :?> Aardvark.Glfw.WindowConfig
        fld.SetValue(app, { cfg with vsync = true })

        let win = app.CreateGameWindow(4)
        let client = Elm.NetworkGroup.client server port

        do 
            let initial = Elm.Game.intitial client texturesPath mapPath
            let app = Elm.App.create initial (Update.update client) (Elm.Game.view client)
            Elm.App.run win app
            win.Title <- "Aardwars rocks "
            exit 0

        
[<AutoOpen>]
module Main = 
    [<EntryPoint>]
    let main (args : string[]) =

        let server, port =
            if args.Length > 0 then
                args.[0], int args.[1]
            else
                "localhost", Elm.App.port

        MainGame.startGame server port

        0