namespace Aardwars
open Adaptify
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open FSharp.Data.Adaptive
open Aardvark.Application
open Elm
open System.Reflection
open Aardwars
open Aardvark.Rendering.Text

open Aardwars.Gun



module Skybox =
    type private Marker = Marker 

    module Shader =
        open FShade

        let reverseTrafo (v : Effects.Vertex) =
            vertex {
                let wp = uniform.ViewProjTrafoInv * v.pos
                return { v with wp = wp / wp.W }
            }
    
        let hi = 70.0 / 255.0
        let lo = 30.0 / 255.0
        let qf = (V4d(hi,lo,lo,1.0))
        let qb = (V4d(lo,hi,hi,1.0))
        let ql = (V4d(lo,hi,lo,1.0))
        let qr = (V4d(hi,lo,hi,1.0))
        let qu = (V4d(lo,lo,hi,1.0))
        let qd = (V4d(hi,hi,lo,1.0))

        let box (v : Effects.Vertex) =
            fragment {
                let c = uniform.CameraLocation
                let f = v.wp.XYZ
                let dir = Vec.normalize (f - c)
                
                let absDir = V3d(abs dir.X, abs dir.Y, abs dir.Z)

                if absDir.X > absDir.Y && absDir.X > absDir.Z then 
                    if dir.X > 0.0 then return qf
                    else return qb
                elif absDir.Y > absDir.X && absDir.Y > absDir.Z then
                    if dir.Y > 0.0 then return ql
                    else return qr
                else
                    if dir.Z > 0.0 then return qu
                    else return qd

            }

        let env =
            samplerCube {
                texture uniform?EnvMap
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
                addressW WrapMode.Wrap
                filter Filter.MinMagMipLinear
            }

        let envMap (v : Effects.Vertex) =
            fragment {
                
                let vp = uniform.ProjTrafoInv * V4d(v.pos.X, v.pos.Y, -1.0, 1.0)
                let vp = vp.XYZ / vp.W

                let dir = (uniform.ViewTrafoInv * V4d(vp, 0.0)).XYZ |> Vec.normalize


                //let wp = uniform.ViewProjTrafoInv * V4d(v.pos.X, v.pos.Y, -1.0, 1.0)

                //let f = 1.0 / (uniform.ViewProjTrafoInv.M33 - uniform.ViewProjTrafoInv.M32)
                
                //let dir = 
                //    f * uniform.ViewProjTrafoInv.C0.XYZ * v.pos.X + 
                //    f * uniform.ViewProjTrafoInv.C1.XYZ * v.pos.Y +
                //    f * uniform.ViewProjTrafoInv.C2.XYZ * -1.0 +

                //    f * uniform.ViewProjTrafoInv.C3.XYZ +
                //    (uniform.ViewProjTrafoInv.C2.XYZ) / (-uniform.ViewProjTrafoInv.M32)

                //let dir = Vec.normalize dir

                //let c = uniform.CameraLocation
                //let f = v.wp.XYZ
                //let dir = Vec.normalize (f - c)
                return env.Sample(dir)
            }

    let skybox (name : string) =
        AVal.custom (fun _ ->
            let env =
                let trafo t (img : PixImage) = img.Transformed t
                let load (name : string) =
                    use s = typeof<Marker>.Assembly.GetManifestResourceStream("Aardwars.CubeMap." + name)
                    PixImage.Load(s)
                    //PixImage.Create(s, PixLoadOptions.Default)
                
                PixImageCube [|
                    PixImageMipMap(
                        load (name.Replace("$", "rt"))
                        |> trafo ImageTrafo.Rot90
                    )
                    PixImageMipMap(
                        load (name.Replace("$", "lf"))
                        |> trafo ImageTrafo.Rot270
                    )
                
                    PixImageMipMap(
                        load (name.Replace("$", "bk"))
                    )
                    PixImageMipMap(
                        load (name.Replace("$", "ft"))
                        |> trafo ImageTrafo.Rot180
                    )
                
                    PixImageMipMap(
                        load (name.Replace("$", "up"))
                        |> trafo ImageTrafo.Rot90
                    )
                    PixImageMipMap(
                        load (name.Replace("$", "dn"))
                        |> trafo ImageTrafo.Rot90
                    )
                |]

            PixTextureCube(env, TextureParams.mipmapped) :> ITexture
        )

    let scene =
        let tex = skybox "miramar_$.png"
        Sg.farPlaneQuad
        |> Sg.uniform "EnvMap" tex
        |> Sg.shader {
            do! Shader.reverseTrafo
            do! Shader.envMap
        }
