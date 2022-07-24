namespace Aardwars

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Text

module Text = 
    let scoreboard (win : IRenderWindow) (tabDown : aval<bool>) (myFrags : aval<int>) (myDeaths : aval<int>) (myColor : aval<string>) (myName : aval<string>) (others : amap<string,OtherPlayerInfo>) =
        let textProj =
            win.Sizes |> AVal.map (fun s ->
                Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
            )
                        
        let font = FontSquirrel.Hack.Regular

        let playerStats =
            let text = 
                AVal.custom (fun tok -> 
                    let os = others.Content.GetValue(tok)
                    let tabDown = tabDown.GetValue tok
                    let myfrags = myFrags.GetValue(tok)
                    let myName = myName.GetValue tok
                    let myDeaths = myDeaths.GetValue tok
                    let myColor = myColor.GetValue tok
                    let res = 
                        os 
                        |> HashMap.map (fun n o -> o.frags,o.deaths,o.color)
                        |> HashMap.add myName (myfrags,myDeaths,myColor)
                        |> Seq.sortByDescending (fun (_,(f,_,_)) -> f)
                        |> Seq.toArray
                    let longest = res |> Array.map (fst >> String.length) |> Array.max
                    let playerLines =
                        res |> Array.map (fun (n,(f,d,c)) -> 
                            let n = n+System.String(' ',longest-n.Length)
                            if tabDown then 
                                sprintf "%s %d %d %s" n f d c
                            else 
                                sprintf "%s %d" n f
                        ) |> String.concat "\n"
                    let playerLines = 
                        if tabDown then "   (Name - Frags - Deaths)\n" + playerLines
                        else playerLines
                    let ips =
                        [
                            yield! Elm.App.myIps
                            yield sprintf "Port: %d" Elm.App.port
                        ] |> String.concat "\n"
                    
                    if tabDown then [playerLines;ips] |> String.concat "\n"
                    else playerLines
                )
            let shape = text |> AVal.map (fun t -> font.Layout(C4b.White, t))

            let trafo = 
                (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
                   
                    let scale = 20.0 / float s.Y * 2.0
                    let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    
                    let minX = -float s.X / float s.Y // left
                    let maxX = (float s.X / float s.Y) - bounds.Max.X // right
                    let rangeX = maxX - minX

                    let minY =  1.0 - bounds.Max.Y // top
                    let maxY = -1.0 + bounds.Max.Y // bottom
                    let rangeY = maxY - minY
                 
                    let x = minX 
                    let y = minY - scale 

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

    let killfeed (win : IRenderWindow) (time : aval<float>) (feed : aval<list<float*string>>) =
        let textProj =
            win.Sizes |> AVal.map (fun s ->
                Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
            )
                        
        let font = FontSquirrel.Hack.Regular

        (feed) |> AVal.map (fun lines -> 
            lines |> List.mapi (fun i (startTime,line) -> 
                let alpha = time |> AVal.map (fun time -> clamp 0.0 1.0 (1.0-(time-startTime)/6.0))
                let shape = line |> AVal.constant |> AVal.map (fun t -> font.Layout(C4f(1.0f,1.0f,1.0f,1.0f).ToC4b(), t))
                let trafo = 
                    (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
                   
                        let scale = 15.0 / float s.Y * 2.0
                        let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    
                        let minX = float s.X / float s.Y // left
                        let maxX = (float s.X / float s.Y) - bounds.Max.X // right
                        let rangeX = maxX - minX

                        let minY =  1.0 - bounds.Max.Y // top
                        let maxY = -1.0 + bounds.Max.Y // bottom
                        let rangeY = maxY - minY
                 
                        let x = maxX
                        let y = minY - (float killfeedLength) * scale + (float i * scale)

                        Trafo3d.Scale(scale) *
                        Trafo3d.Translation(x, y, -1.0)
                    )
                Sg.shape shape
                |> Sg.trafo trafo
                |> Sg.uniform "Alpha" alpha
            ) |> Sg.ofList
        )
        |> Sg.dynamic
        |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
        |> Sg.projTrafo textProj
        |> Sg.afterEffect [
            toEffect <| Elm.Shader.sgAlpha
        ]
        |> Sg.pass Elm.Passes.pass2
        |> Sg.depthTest' DepthTest.None
        |> Sg.blendMode' BlendMode.Blend

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
                   
                    let scale = 45.0 / float s.Y * 2.0
                    let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    
                    let minX = -float s.X / float s.Y // left
                    let maxX = (float s.X / float s.Y) - bounds.Max.X // right
                    let rangeX = maxX - minX

                    let minY =  1.0 - bounds.Max.Y // top
                    let maxY = -1.0 + bounds.Max.Y // bottom
                    let rangeY = maxY - minY
                 
                    let x = minX + 0.3
                    let y = maxY + 0.12

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

        
    let font = FontSquirrel.Paete_Round.Bold
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
                    let scale = 10.0 / float s.Y * 2.0
                    let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    let minX = -float s.X / float s.Y
                    let x = minX - bounds.Min.X + 0.005
                    let y = 1.0 - bounds.Max.Y - 0.005

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

    let overlaySg (enabled : aval<bool>) =
        Sg.fullScreenQuad
        |> Sg.shader {
            do! DefaultSurfaces.constantColor C4f.Black
            do! Elm.Shader.sgAlpha
        }
        |> Sg.uniform' "Alpha" 0.4
        |> Sg.pass Elm.Passes.pass4
        |> Sg.viewTrafo' Trafo3d.Identity
        |> Sg.projTrafo' Trafo3d.Identity
        |> Sg.onOff enabled
        |> Sg.depthTest' DepthTest.None
        |> Sg.blendMode' BlendMode.Blend
        
    let deathScreenSg (win : IRenderWindow) (enabled : aval<bool>) (deathReason : aval<string*WeaponType>) (timeLeftUntilRespawn : aval<Option<float>>) =
        let overlay = overlaySg enabled
        let text = 
            deathReason |> AVal.map (fun (name,wep) -> 
                sprintf "You got fragged\nFragger: %s\nGun: %s" name (wep |> WeaponType.toString)
            )
        let textProj =
            win.Sizes |> AVal.map (fun s ->
                Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
            )
        let thing =
            let shape = text |> AVal.map (fun t -> font.Layout(C4b.White, t))
            let trafo = 
                (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
                    let scale = 60.0 / float s.Y * 2.0
                    let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    
                    let minX = -float s.X / float s.Y // left
                    let maxX = (float s.X / float s.Y) - bounds.Max.X // right
                    let rangeX = maxX - minX

                    let minY =  1.0 - bounds.Max.Y // top
                    let maxY = -1.0 + bounds.Max.Y // bottom
                    let rangeY = maxY - minY
                 
                    let x = minX + 0.5
                    let y = maxY + 1.2

                    Trafo3d.Scale(scale) *
                    Trafo3d.Translation(x, y, -1.0)
                )
                
            Sg.shape shape
            |> Sg.trafo trafo

        let textSg = 
            thing
            |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
            |> Sg.projTrafo textProj
            |> Sg.pass Elm.Passes.pass5
            |> Sg.depthTest' DepthTest.None
            |> Sg.blendMode' BlendMode.Blend

        let respawnTimeTextSg =
            let text = 
                timeLeftUntilRespawn |> AVal.map (fun t -> 
                    match t with 
                    | None -> "Press [Space] to respawn!"
                    | Some t -> 
                        sprintf "(%.1f) until respawn" t
                )
            Sg.text font C4b.White text
            |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
            |> Sg.projTrafo' (Trafo3d.Scale(0.05)*Trafo3d.Translation(0.0,0.5,0.0))
            |> Sg.pass Elm.Passes.pass5
            |> Sg.depthTest' DepthTest.None
            |> Sg.blendMode' BlendMode.Blend

        Sg.ofList [overlay;textSg;respawnTimeTextSg]
        |> Sg.onOff enabled