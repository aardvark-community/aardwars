namespace Aardwars

open System.IO
open ICSharpCode.SharpZipLib.Zip.Compression.Streams

module Minecraft =
    
    type Region = { FileName : string; X : int; Z : int }

    /// Returns all regions in given minecraft map directory.
    let getRegions minecraftMapDir =
        let dir = Path.Combine(minecraftMapDir, "region")
        match Directory.Exists dir with
        | true -> Directory.EnumerateFiles(dir, "*.mca")
                  |> Seq.map (fun fn ->
                      let ts = fn.Split('.')
                      { FileName = fn; X = int(ts.[1]); Z = int(ts.[2]); }
                      )
                  |> Seq.toArray
        | false -> Array.empty

    let enumerateRawNbtBuffers (region : Region) : seq<byte[]> =

        let offset x z : int = ((x &&& 31) + ((z &&& 31) <<< 5)) <<< 2
        let unzip (buffer : byte[]) : byte[] =
            use compressed = new MemoryStream(buffer)
            use input = new InflaterInputStream(compressed)
            use uncompressed = new MemoryStream()
            input.CopyTo(uncompressed)
            uncompressed.ToArray()

        let ungzip (buffer : byte[]) : byte[] =
            failwith "GZip (RFC1952) not implemented."

        seq {
            let xs = File.ReadAllBytes region.FileName
            let readBigEndian24At o : int = (int(xs.[o]) <<< 16) ||| (int(xs.[o+1]) <<< 8) ||| int(xs.[o+2])
            let readBigEndian32At o : int = (int(xs.[o]) <<< 24) ||| (int(xs.[o+1]) <<< 16) ||| (int(xs.[o+2]) <<< 8) ||| int(xs.[o+3])
            printfn "file size: %d" xs.Length
            for z = 0 to 31 do
                for x = 0 to 31 do
                    let o = (offset x z |> readBigEndian24At) <<< 12
                    match o with
                    | 0 -> () // chunk (x, z) does not exist
                    | _ ->
                        let blockCount4k = int32(xs.[o+3]) <<< 12
                        let compressedBufferSize = readBigEndian32At o
                        let compressionType = int(xs.[o+4])
                        
                        let compressedBuffer = Array.sub xs (o+5) (compressedBufferSize-1)
                        
                        let uncompress =
                            match compressionType with
                            | 1 -> ungzip   // 1 ... GZip (RFC1952)
                            | 2 -> unzip    // 2 ... Zlib (RFC1950)
                            | 3 -> id       // 3 ... uncompressed
                            | _ -> failwith (sprintf "Unknown compression type %d." compressionType)

                        let buffer = compressedBuffer |> uncompress
                        //printfn "region %3d %3d | chunk %2d %2d" region.X region.Z x z
                        yield buffer
                        
                        ()
            }

    let test () =
        
        let rs = getRegions @"T:\Dropbox\Data\minecraft\Notre_Dame_and_Medieval_City\Notre Dame and Medieval City"
        for r in rs do 
            printfn "region %3d %3d" r.X r.Z
            let xs = enumerateRawNbtBuffers r |> Seq.toArray
            ()
            //for x in xs do printfn "%d" x.Length
        System.Environment.Exit 0
    
