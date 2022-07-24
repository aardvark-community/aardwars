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
                let alpha = time |> AVal.map (fun time -> clamp 0.0 1.0 (1.0-(time-startTime)/PlayerConstant.killfeedFadeTime))
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
        let font = FontSquirrel.Anonymous_Pro.Regular
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

    
    let gameOverScreenSg (win : IRenderWindow) (enabled : aval<bool>) (timeLeftUntilCanRestart : aval<Option<float>>)  (myFrags : aval<int>) (myDeaths : aval<int>) (myColor : aval<string>) (myName : aval<string>) (others : amap<string,OtherPlayerInfo>) =
        
        let font = FontSquirrel.Anonymous_Pro.Regular
        let overlay = overlaySg enabled
        let winner = 
            let me = (myFrags,myName) ||> AVal.map2 (fun f n -> [f,n])
            let all = others |> AMap.map (fun n i -> i.frags,n) |> AMap.toASetValues |> ASet.union (ASet.ofAVal me)
            all |> ASet.tryMax |> AVal.map (Option.defaultValue (0,""))
            
        let text1 = 
            winner |> AVal.map (fun (frags,name) -> 
                sprintf "%s wins the match with %d frags!" name frags
            )
        let text2 = 
            AVal.constant "GG"
        let textProj =
            win.Sizes |> AVal.map (fun s ->
                Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
            )
        let thing text size xo yo =
            let shape = text |> AVal.map (fun t -> font.Layout(C4b.White, t))
            let trafo = 
                (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
                    let scale = size / float s.Y * 2.0
                    let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                    
                    let minX = -float s.X / float s.Y // left
                    let maxX = (float s.X / float s.Y) - bounds.Max.X // right
                    let rangeX = maxX - minX

                    let minY =  1.0 - bounds.Max.Y // top
                    let maxY = -1.0 + bounds.Max.Y // bottom
                    let rangeY = maxY - minY
                 
                    let x = minX + xo
                    let y = maxY + yo

                    Trafo3d.Scale(scale) *
                    Trafo3d.Translation(x, y, -1.0)
                )
            Sg.shape shape
            |> Sg.trafo trafo
        let thing1 = thing text1 40.0 0.5 1.2
        let thing2 = thing text2 80.0 0.15 0.2

        let textSg = 
            Sg.ofList [thing1;thing2]
            |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
            |> Sg.projTrafo textProj
            |> Sg.pass Elm.Passes.pass5
            |> Sg.depthTest' DepthTest.None
            |> Sg.blendMode' BlendMode.Blend
        
        //let scoreboardSg = 
        //    scoreboard win (AVal.constant true) myFrags myDeaths myColor myName others
        //    |> Sg.trafo' (Trafo3d.Translation(0.5,0.2,0.0))

        let respawnTimeTextSg =
            let text = 
                timeLeftUntilCanRestart |> AVal.map (fun t -> 
                    match t with 
                    | None -> "Host must press [Ctrl+Shift+P] to start new round!"
                    | Some t -> 
                        sprintf "(%.1f) until new round" t
                )
            Sg.text font C4b.White text
            |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
            |> Sg.projTrafo' (Trafo3d.Scale(0.045)*Trafo3d.Translation(-0.35,0.8,0.0))
            |> Sg.pass Elm.Passes.pass5
            |> Sg.depthTest' DepthTest.None
            |> Sg.blendMode' BlendMode.Blend

        Sg.ofList [overlay;textSg;respawnTimeTextSg]
        |> Sg.onOff enabled

    let timeleftTextSg (win : IRenderWindow) (timeleft: aval<string>) (onoff : aval<bool>) =
    
        let textProj =
            win.Sizes |> AVal.map (fun s ->
                Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
            )
                        
        let font = FontSquirrel.Anonymous_Pro.Regular

        let tt =
            let shape = timeleft |> AVal.map (fun t -> font.Layout(C4b.White, t))

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
                 
                    let x = minX+rangeX/2.0
                    let y = minY-0.1

                    Trafo3d.Scale(scale) *
                    Trafo3d.Translation(x, y, -1.0)
                )

            Sg.shape shape
            |> Sg.trafo trafo

        tt
        |> Sg.onOff onoff
        |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
        |> Sg.projTrafo textProj
        |> Sg.pass Elm.Passes.pass5
        |> Sg.depthTest' DepthTest.None
        |> Sg.blendMode' BlendMode.Blend