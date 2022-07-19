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

    let server, port =
        if args.Length > 0 then
            args.[0], int args.[1]
        else
            Elm.NetworkGroup.server 7331
            "localhost", 7331

    Aardvark.Init()
    let app = new OpenGlApplication()
    let win = app.CreateGameWindow()

    let client = Elm.NetworkGroup.client server port

    do 
        let app = Elm.App.create Elm.Game.intitial (Update.update client) (Elm.Game.view client)
        Elm.App.run win app
        exit 0
    0