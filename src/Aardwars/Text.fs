namespace Aardwars

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Text

module Text = 

    let weaponTextSg (win : IRenderWindow) (text : aval<string>) =
    
        let textProj =
            win.Sizes |> AVal.map (fun s ->
                Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
            )
                        
        let font = FontSquirrel.Paete_Round.Bold

        let playerStats =

            let shape = text |> AVal.map (fun t -> font.Layout(C4b.White, t))

            let trafo = 
                (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
                   
                    let scale = 35.0 / float s.Y * 2.0
                    let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    
                    let minX = -float s.X / float s.Y // left
                    let maxX = (float s.X / float s.Y) - bounds.Max.X // right
                    let rangeX = maxX - minX

                    let minY =  1.0 - bounds.Max.Y // top
                    let maxY = -1.0 + bounds.Max.Y // bottom
                    let rangeY = maxY - minY
                 
                    let x = minX + (0.25 * rangeX) 
                    let y = maxY 

                    Trafo3d.Scale(scale) *
                    Trafo3d.Translation(x, y, -1.0)
                )
                
            Sg.shape shape
            |> Sg.trafo trafo

        playerStats
        |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
        |> Sg.projTrafo textProj
        |> Sg.pass Elm.Passes.pass2
        |> Sg.depthTest' DepthTest.None
        |> Sg.blendMode' BlendMode.Blend


    let velocityTextSg (win : IRenderWindow) (velocity : aval<string>) (showText : aval<bool>) =
    
        let textProj =
            win.Sizes |> AVal.map (fun s ->
                Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
            )
                        
        let font = FontSquirrel.Anonymous_Pro.Regular

        let velocityCounter =
            let shape = velocity |> AVal.map (fun t -> font.Layout(C4b.White, t))

            let trafo = 
                (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
                   
                    let scale = 18.0 / float s.Y * 2.0
                    let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    let minX = -float s.X / float s.Y
                    let x = minX - bounds.Min.X + 0.02
                    let y = 1.0 - bounds.Max.Y - 0.02

                    Trafo3d.Scale(scale) *
                    Trafo3d.Translation(x, y, -1.0)
                )

            Sg.shape shape
            |> Sg.trafo trafo

        velocityCounter
        |> Sg.onOff showText
        |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
        |> Sg.projTrafo textProj
        |> Sg.pass Elm.Passes.pass2
        |> Sg.depthTest' DepthTest.None
        |> Sg.blendMode' BlendMode.Blend
