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
let main (_args : string[]) =

    //Minecraft.test ()

    Aardvark.Init()
    let app = new OpenGlApplication()
    let win = app.CreateGameWindow()


    do 
        let app = Elm.App.create Elm.Game.intitial Update.update Elm.Game.view
        Elm.App.run win app
        exit 0
    0