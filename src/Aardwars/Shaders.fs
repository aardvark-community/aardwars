namespace Aardwars

open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Base
open Aardwars
open Aardvark.Rendering.Text

module Shader =
    open FShade

    type UniformScope with
        member x.Offset : V4d[] = uniform?StorageBuffer?Offset
        member x.Scale : V4d[] = uniform?StorageBuffer?Scale

    type Vertex =
        {
            [<Position>]
            pos : V4d

            [<TexCoord>]
            tc : V2d

            [<Semantic("VertexPos")>]
            vert : V4d
            
            [<Normal>]
            n : V3d
            
            [<Semantic("Offset")>]
            off : V4d

            [<Semantic("Scale")>]
            scale : V4d
            
            [<Semantic("TextureId"); Interpolation(InterpolationMode.Flat)>]
            texId : int
            
            [<Semantic("RenderStyle"); Interpolation(InterpolationMode.Flat)>]
            renderStyle : int
        }
        
    let sammy =
        sampler2dArray {
            texture uniform?Atlas
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let boxy (v : Vertex) =
        vertex {
            let bb1 = Bitwise.FloatBitsToUInt v.off.W
            let top = bb1 >>> 16
            let bottom = bb1 &&& 0xFFFFu
            let bb2 = Bitwise.FloatBitsToUInt v.scale.W
            let renderStyle = int (bb2 >>> 16)
            let side = bb2 &&& 0xFFFFu

            let mutable vp = v.pos.XYZ
            if renderStyle = 1 then
                vp <- vp - v.n * 0.5

            let wp = uniform.ModelTrafo * V4d(v.off.XYZ + v.scale.XYZ * vp, 1.0) 
            let pp = uniform.ViewProjTrafo * wp

            let n = uniform.NormalMatrix * v.n

            let mutable pos = pp
            if renderStyle = 1 then
                if abs v.n.Z > 0.01 then pos <- V4d(1000.0, 1000.0, -1000.0, 1.0)

            let tc =
                if abs v.n.Z > 0.01 then V2d(v.pos.X, v.pos.Y)
                elif abs v.n.Y > 0.0 then V2d(v.pos.X, v.pos.Z)
                else V2d(v.pos.Y, v.pos.Z)
                
            let texId =
                if v.n.Z > 0.01 then int top
                elif v.n.Z < 0.01 then int bottom
                else int side

            return { 
                v with 
                    vert = v.pos
                    pos = pos
                    texId = texId
                    renderStyle = renderStyle
                    tc = tc
                    n = n
            }
        }
        
    let texy (v : Vertex) =
        fragment {
            let color = sammy.Sample(v.tc, v.texId)
            
            if color.W < 0.05 then discard()

            return color
        }
    [<GLSLIntrinsic("gl_FragDepth")>]
    let depth() : float = onlyInShaderCode "depth"

    let fogColor = C4d.LightPink.ToV4d()
    let foggy (v : Effects.Vertex) =
        fragment {
            // get world position
            let depth = 0.5 * (v.pos.Z / v.pos.W) + 0.5
            let d = lerp 0.1 1000.0 depth + 10.0
            //let p = v.wp.XYZ
            
            let va = 2.0 * max 0.0 (depth ** 64.0 - 0.5)
            //// Find the depth of the fragment
            //let d = Vec.distance p uniform.CameraLocation - 10.0


            // v = 0 -> 0.0
            // v = 1 -> 1.0



            // apply
            let f = va //exp (v * 0.0625) |> clamp 0.0 1.0
            return lerp v.c fogColor f
        }
    type UniformScope with
        member x.VollgasWhite : float32 = uniform?VollgasWhite
    let vollgasWhite (v : Effects.Vertex) =
        fragment {
            if uniform.VollgasWhite > 0.5f then 
                return V4d(1.0,1.0,1.0,1.0)
            else 
                return v.c
        }
        
    type UniformScope with 
        member x.GunTex : float32 = uniform?GunTex
        member x.PlayerTex : float32 = uniform?PlayerTex
    let lgSammy = 
        sampler2d {
            texture uniform?laserguntex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let sgSammy = 
        sampler2d {
            texture uniform?shotguntex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let snSammy = 
        sampler2d {
            texture uniform?snipertex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let rgSammy = 
        sampler2d {
            texture uniform?rainbowguntex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let rlSammy = 
        sampler2d {
            texture uniform?rocketlaunchertex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let guntexy (v : Effects.Vertex) =
        fragment {
            let t = uniform.GunTex
            let col = 
                match t with 
                | 0.0f -> lgSammy.Sample(v.tc)
                | 1.0f -> sgSammy.Sample(v.tc)
                | 2.0f -> snSammy.Sample(v.tc)
                | 3.0f -> rgSammy.Sample(v.tc)
                | 4.0f -> rlSammy.Sample(v.tc)
                | _ -> V4d.IOOO
            return col
        }

    let blackSammy = 
        sampler2d {
            texture uniform?blackPlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let blueSammy = 
        sampler2d {
            texture uniform?bluePlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let greenSammy = 
        sampler2d {
            texture uniform?greenPlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }          
    let orangeSammy = 
        sampler2d {
            texture uniform?orangePlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }            
    let pinkSammy = 
        sampler2d {
            texture uniform?pinkPlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }            
    let purpleSammy = 
        sampler2d {
            texture uniform?purplePlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }            
    let redSammy = 
        sampler2d {
            texture uniform?redPlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }            
    let whiteSammy = 
        sampler2d {
            texture uniform?whitePlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }            
    let yellowSammy = 
        sampler2d {
            texture uniform?yellowPlayerTex
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
    let playertexy (v : Effects.Vertex) =
        fragment {
            let t = uniform.PlayerTex
            let col = 
                match t with 
                | 0.0f -> blackSammy.Sample(v.tc)
                | 1.0f -> blueSammy.Sample(v.tc)
                | 2.0f -> greenSammy.Sample(v.tc)
                | 3.0f -> orangeSammy.Sample(v.tc)
                | 4.0f -> pinkSammy.Sample(v.tc)
                | 5.0f -> purpleSammy.Sample(v.tc)
                | 6.0f -> redSammy.Sample(v.tc)
                | 7.0f -> whiteSammy.Sample(v.tc)
                | 8.0f -> yellowSammy.Sample(v.tc)
                | _ -> V4d.IOOO
            return col
        }
        