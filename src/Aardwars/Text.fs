namespace Aardvars

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Text

module Text = 

    let statusTextSg (win : IRenderWindow) (t : aval<string>) (showText : aval<bool>) =
    
        let textProj =
            win.Sizes |> AVal.map (fun s ->
                Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
            )
                        
        let font = FontSquirrel.Anonymous_Pro.Regular
        let healthCounter =
            let shape = 
                t |> AVal.map (fun t -> font.Layout(C4b.White, "health"))
            let trafo = 
                (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
                    let scale = 18.0 / float s.Y * 2.0
                    let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    let maxX = float s.X / float s.Y
                    let x = maxX - bounds.Max.X - 0.02
                    let y = 0.0 - bounds.Max.Y - 0.02
                    Trafo3d.Scale(scale) *
                    Trafo3d.Translation(x, y, -1.0)
                )
            Sg.shape shape
            |> Sg.trafo trafo
            
        let velocityCounter =
            let shape = 
                t |> AVal.map (fun t -> font.Layout(C4b.White, t))

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

        Sg.ofList [velocityCounter;healthCounter]
        |> Sg.onOff showText
        |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
        |> Sg.projTrafo textProj
        |> Sg.depthTest' DepthTest.None
        |> Sg.blendMode' BlendMode.Blend
