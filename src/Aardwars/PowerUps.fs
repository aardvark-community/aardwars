namespace Aardwars
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Text
open System
open Aardvark.SceneGraph.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open FShade

type Medikit =
    {
        heal    : float -> float -> float
        rarity  : float
    }

module PowerUps = 
    let healToMax (maxHp : float) (_ : float) : float =
        maxHp
    let heal (maxHp : float) (currentHp : float) : float =
        let newHp = currentHp + 50.0
        match newHp <= maxHp with
        | true -> newHp
        | false -> maxHp
   
    let doubleMaxHp (maxHp : float) (_ : float): float =
        maxHp * 2.0

    let MedikitToMax =
        {
            heal = healToMax
            rarity = 0.5
        }
    
    let MedikitDoubleMax =
        {
            heal = doubleMaxHp
            rarity = 0.1
        }

    let MedikitHeal =
        {
            heal = heal
            rarity = 0.9
        }

    let scene =

        let position world =
            world.Bounds.Min.XYO + world.Bounds.RangeZ.Center * V3d.OOI + V3d.IIO + V3d(112,100,-68.9)

        let rotationTrafo = Trafo3d.RotationYInDegrees(-60.0) * Trafo3d.RotationXInDegrees(60.0)

        //let sg world = 
            //Import.importObj("MediPack")
            //|> Sg.scale 0.4
            //|> Sg.transform rotationTrafo
            //|> Sg.translation' (position world)
            //|> Sg.shader {
            //    do! DefaultSurfaces.trafo
            //    do! DefaultSurfaces.diffuseTexture
            //    do! DefaultSurfaces.simpleLighting
            //}
            //|> Sg.cullMode' CullMode.None
            //|> Sg.fillMode' FillMode.Fill
            //|> Sg.compile win.Runtime sigg

        Sg.empty
        


