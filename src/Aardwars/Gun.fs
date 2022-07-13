namespace Aardwars

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Text
open Aardvark.SceneGraph.IO
open System
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open FShade
module Gun =
    let scene (win : IRenderWindow) =
        let sigg =   
            win.Runtime.CreateFramebufferSignature([
                DefaultSemantic.Colors, TextureFormat.Rgba8; 
                DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
                ]
            )    
        let gunProjection =
            (win.Sizes |> AVal.map (fun s -> Frustum.perspective 110.0 0.0001 20.0 (float s.X / float s.Y) |> Frustum.projTrafo))
        let task = 
            Import.importGun()
                |> Sg.transform 
                    (
                        Trafo3d.Scale(1.0,1.0,-1.0) *
                        Trafo3d.Scale(0.5) *
                        Trafo3d.Translation(1.0,-1.0,-1.0)
                    )
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                    do! 
                        (fun (v : Effects.Vertex) -> 
                            fragment {
                                return V4d(v.c.X ** 0.5, v.c.Y ** 0.5, v.c.Z ** 0.5, 1.0)
                            }
                        )
                    do! DefaultSurfaces.simpleLighting
                }
                |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
                |> Sg.projTrafo gunProjection
                //|> Sg.depthTest' DepthTest.None
                |> Sg.cullMode' CullMode.None
                |> Sg.fillMode' FillMode.Fill
                |> Sg.compile win.Runtime sigg
                
        let c : ClearValues =
            clear {
                color (C4b(0uy,0uy,0uy,0uy))
                depth 1.0
                stencil 0
            }
        let t = RenderTask.renderToColorWithClear win.Sizes c task 
        
        let gunSg =
            Sg.fullScreenQuad
            |> Sg.diffuseTexture t
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
            }
            |> Sg.blendMode' BlendMode.Blend
            |> Sg.depthTest' DepthTest.None
            |> Sg.pass (RenderPass.after "§iasfj" RenderPassOrder.Arbitrary RenderPass.main)

        let crosshairSg =
            let shape =
                let t = 3.0
                let r = 30.0
                let w = 10.0
                let t2 = 4.0
                let color = C4b.Red 
                ShapeList.ofListWithRenderStyle RenderStyle.NoBoundary [
                    ConcreteShape.circle color t (Circle2d(V2d.Zero, r))
                    ConcreteShape.fillRectangle color (Box2d(r, -t/2.0, r + w, t/2.0))
                    ConcreteShape.fillRectangle color (Box2d(-r-w, -t/2.0, -r, t/2.0))
                    ConcreteShape.fillRectangle color (Box2d(-t/2.0, r, t/2.0, r + w))
                    ConcreteShape.fillRectangle color (Box2d(-t/2.0, -r - w, t/2.0, -r))
                    ConcreteShape.fillRectangle color (Box2d(-t2/2.0,-r,t2/2.0,r))
                    ConcreteShape.fillRectangle color (Box2d(-r,-t2/2.0,r,t2/2.0))
                ]
            Sg.shape (
                AVal.constant (
                    shape    
                )
            )
            |> Sg.viewTrafo' Trafo3d.Identity
            |> Sg.projTrafo (win.Sizes |> AVal.map (fun s -> Trafo3d.Scale(1.0 / float s.X, 1.0 / float s.Y, 1.0)))
        Sg.ofList [gunSg; crosshairSg]