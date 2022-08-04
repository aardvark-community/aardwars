# aardwars
Multiplayer arena shooter FPS game built using Aardvark Platform.

Gameplay video: https://youtu.be/x7_Yy6IQzog

<table>
  <tr>
    <td> <img src="/screenshots/slice6.jpg" width="200px" /> </td>
    <td> <img src="/screenshots/slice5.jpg" width="200px" /> </td>
 </tr>
  <tr>
    <td> <img src="/screenshots/slice4.jpg" width="200px" /> </td>
    <td> <img src="/screenshots/slice3.jpg" width="200px" /> </td>
 </tr>
  <tr>
    <td> <img src="/screenshots/slice1.jpg" width="200px" /> </td>
    <td> <img src="/screenshots/slice2.jpg" width="200px" /> </td>
 </tr>
</table>

## How to play

* Either [download the latest github Release](https://github.com/aardvark-community/aardwars/releases/latest) and run Aardwars.Launcher.exe (Windows only)
* Or install as dotnet tool:
  1. `dotnet tool install --global aardwars` 
  2. `aardwars <server> <port>`

## Created by

* Georg Haaser (technical programming, [github/krauthaufen](https://github.com/krauthaufen))
* Lisa Kellner (additional gameplay programming, [github/gnufu](https://github.com/gnufu))
* Mateusz Kuzaj (3D models, [github/M4t3usz1](https://github.com/M4t3usz1))
* Sebastian Maierhofer (gameplay programming, [github/sebastianjulian](https://github.com/sebastianjulian) [instagram @sebi_maierhofer](https://www.instagram.com/sebi_maierhofer/))
* Stefan Maierhofer (minecraft world parser, screenshotr, [github/stefanmaierhofer](https://github.com/stefanmaierhofer))
* Attila Szabo (additional gameplay programming, [github/aszabo314](https://github.com/aszabo314))
* Andreas Walch (additional gameplay programming, [github/WalchAndreas](https://github.com/WalchAndreas)))

## Implementation

This project is a proof-of-concept implementation of a playable real-time video game using the [Aardvark Platform](https://aardvarkians.com/) scientific visualization toolset. It was initially created as part of an internship at [VRVis](https://www.vrvis.at/). Features include:

* Elm-style ("model-view-update") application structure using [FSharp.Data.Adaptive](https://github.com/fsprojects/FSharp.Data.Adaptive)/[Adaptify](https://github.com/krauthaufen/Adaptify) and [aardvark.rendering's](https://github.com/aardvark-platform/aardvark.rendering) `ISg` graphics API. 
* Heavy use of [aardvark.rendering's](https://github.com/aardvark-platform/aardvark.rendering) automatic instancing (`Sg.instanced`) and [FShade](https://www.fshade.org/) for high performance rendering
* Implementation of the Level of Detail renderer in [aardvark.algodat](https://github.com/aardvark-platform/aardvark.algodat) to render Minecraft-style worlds
* Parser for Minecraft world files
* simple multiplayer using dotnet websockets

## Additional Credits

* community texture packs from https://www.curseforge.com/minecraft/texture-packs/quadral-pack and https://resourcepack.net/mythic-resource-pack/
* Minecraft world file from https://www.minecraftmaps.com/pvp-maps/jakobs-kitpvp
