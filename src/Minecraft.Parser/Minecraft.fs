namespace Aardwars

open System.IO
open ICSharpCode.SharpZipLib.Zip.Compression.Streams
open System.Text
open System
open System.Collections
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

module Minecraft =
    
    (*
        https://minecraft.fandom.com/wiki/Region_file_format
        https://minecraft.fandom.com/wiki/Chunk_format
        https://minecraft.fandom.com/wiki/NBT_format#Binary_format
    *)

    type Region = { FileName : string; X : int; Z : int }
   
    type RawNbtBuffer = { Region : Region; X : int; Z : int; Buffer : byte[] }
    
    type Section = {
        Region : Region
        //BlockStatesRaw : int64[]
        BlockStates : string[] // 16x16x16 material names
        Palette : string[]
        Y : int
        XPos : int
        ZPos : int
        }


    // parsing ...
    type NbtPayload =
        | PayloadEnd                        // 0
        | PayloadByte of byte               // 1
        | PayloadShort of int16             // 2
        | PayloadInt of int32               // 3
        | PayloadLong of int64              // 4
        | PayloadFloat of float32           // 5
        | PayloadDouble of float            // 6
        | PayloadByteArray of byte[]        // 7
        | PayloadString of string           // 8
        | PayloadList of NbtPayload[]       // 9
        | PayloadCompound of NbtTag list    // 10
        | PayloadIntArray of int32[]        // 11
        | PayloadLongArray of int64[]       // 12
    and 
        NbtTag = { Name : string; Payload : NbtPayload }

    let parseInt16 (xs : byte[]) o : int16 = (int16(xs.[o  ]) <<<  8) ||| (int16(xs.[o+1])       )
    let parseInt24 (xs : byte[]) o : int32 = (int32(xs.[o  ]) <<< 16) ||| (int32(xs.[o+1]) <<<  8) ||| (int32(xs.[o+2])       )
    let parseInt32 (xs : byte[]) o : int32 = (int32(xs.[o  ]) <<< 24) ||| (int32(xs.[o+1]) <<< 16) ||| (int32(xs.[o+2]) <<<  8) ||| (int32(xs.[o+3])       )
    let parseInt64 (xs : byte[]) o : int64 = (int64(xs.[o  ]) <<< 56) ||| (int64(xs.[o+1]) <<< 48) ||| (int64(xs.[o+2]) <<< 40) ||| (int64(xs.[o+3]) <<< 32) |||
                                             (int64(xs.[o+4]) <<< 24) ||| (int64(xs.[o+5]) <<< 16) ||| (int64(xs.[o+6]) <<<  8) ||| (int64(xs.[o+7])       )
    
    let parseFloat32 (xs : byte[]) o : float32 =
        [| xs.[o + 3]; xs.[o + 2]; xs.[o + 1]; xs.[o    ] |]
        |> BitConverter.ToSingle

    let parseFloat64 (xs : byte[]) o : float =
        [| xs.[o + 7]; xs.[o + 6]; xs.[o + 5]; xs.[o + 4]; xs.[o + 3]; xs.[o + 2]; xs.[o + 1]; xs.[o    ] |]
        |> BitConverter.ToDouble
    
    let parseString xs o =
        let tagNameLength = int(parseInt16 xs o)
        let tagName = Array.sub xs (o + 2) tagNameLength |> Encoding.UTF8.GetString
        (tagName, o + 2 + tagNameLength)

    /// Returns all regions in given minecraft map directory.
    let getRegions minecraftMapDir =
        let dir = Path.Combine(minecraftMapDir, "region")
        match Directory.Exists dir with
        | true -> Directory.EnumerateFiles(dir, "*.mca")
                  |> Seq.map (fun fn ->
                      let ts = Path.GetFileName(fn).Split('.')
                      { FileName = fn; X = int(ts.[1]); Z = int(ts.[2]); }
                      )
                  |> Seq.toArray
        | false -> Array.empty

    let enumerateRawNbtBuffers (region : Region) : seq<RawNbtBuffer> =

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
            printfn "%s [%d]" region.FileName xs.Length
            for z = 0 to 31 do
                for x = 0 to 31 do
                    let o = (offset x z |> parseInt24 xs) <<< 12
                    match o with
                    | 0 -> () // chunk (x, z) does not exist
                    | _ ->
                        let blockCount4k = int32(xs.[o+3]) <<< 12
                        let compressedBufferSize = parseInt32 xs o
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
                        yield { Region = region; X = x; Z = z; Buffer = buffer }
                        
                        ()
            }

    let parseRawNbtBuffer (x : RawNbtBuffer) =

        let buffer = x.Buffer
        //File.WriteAllBytes("debug.nbt", buffer)

        let rec parse (tagId : int option) doNotParseTagName offset =

            // parses list of tags until PayloadEnd is reached
            let rec parseList doNotParseTagName o =
                match parse None doNotParseTagName o with
                | ({ Payload = PayloadEnd }, newOffset) -> ([], newOffset)
                | ( tag, o1) -> let (xs, o2) = parseList doNotParseTagName o1
                                (tag :: xs, o2)

            let mutable o = offset

            let tagId = 
                match tagId with
                | Some x -> x
                | None   -> let x = int(buffer.[o])
                            o <- o + 1
                            x

            if tagId = 0 then

                // TAG_End
                ({ Name = ""; Payload = PayloadEnd }, o)

            else

                let tagName = 
                    match doNotParseTagName with
                    | true  -> ""
                    | false ->
                        let (tagName, newOffset) = parseString buffer o
                        o <- newOffset
                        tagName

                //printfn "%d %s" tagId tagName

                match tagId with
                      
                // TAG_Byte   
                |  1 ->
                    ({ Name = tagName; Payload = PayloadByte buffer.[o] }, o + 1)

                // TAG_Short   
                |  2 ->
                    let x = parseInt16 buffer o |> PayloadShort
                    ({ Name = tagName; Payload = x }, o + 2)

                // TAG_Int   
                |  3 ->
                    let x = parseInt32 buffer o |> PayloadInt
                    ({ Name = tagName; Payload = x }, o + 4)
                
                // TAG_Long   
                |  4 ->
                    let x = parseInt64 buffer o |> PayloadLong
                    ({ Name = tagName; Payload = x }, o + 8)

                // TAG_Double   
                |  5 ->
                    let x = parseFloat32 buffer o |> PayloadFloat
                    ({ Name = tagName; Payload = x }, o + 4)

                // TAG_Double   
                |  6 ->
                    let x = parseFloat64 buffer o |> PayloadDouble
                    ({ Name = tagName; Payload = x }, o + 8)

                // TAG_Byte_Array
                |  7 ->
                    let count = parseInt32 buffer o
                    o <- o + 4
                    let xs = Array.sub buffer o count
                    o <- o + count
                    ({ Name = tagName; Payload = PayloadByteArray xs }, o)

                // TAG_String  
                |  8 ->
                    let (s, newOffset) = parseString buffer o
                    ({ Name = tagName; Payload = PayloadString s }, newOffset)

                 // TAG_List   
                |  9 ->
                    let listItemsTagId = int(buffer.[o])
                    o <- o + 1
                    let count = parseInt32 buffer o
                    o <- o + 4
                    let xs = Array.zeroCreate count
                    for i = 0 to count-1 do
                        let (x, newOffset) = parse (Some listItemsTagId) true o 
                        xs.[i] <- x.Payload
                        o <- newOffset
                    ({ Name = tagName; Payload = PayloadList xs }, o)
                    
                // TAG_Compound
                | 10 ->
                    let (xs, newOffset) = parseList false o
                    ({ Name = tagName; Payload = PayloadCompound xs }, newOffset)

                // TAG_Int_Array 
                | 11 ->
                    let count = parseInt32 buffer o
                    o <- o + 4
                    let xs = Array.zeroCreate count
                    for i = 0 to count-1 do
                        xs.[i] <- parseInt32 buffer o
                        o <- o + 4
                    ({ Name = tagName; Payload = PayloadIntArray xs }, o)
                
                // TAG_Long_Array
                | 12 ->
                    let count = parseInt32 buffer o
                    o <- o + 4
                    let xs = Array.zeroCreate count
                    for i = 0 to count-1 do
                        xs.[i] <- parseInt64 buffer o
                        o <- o + 8
                    ({ Name = tagName; Payload = PayloadLongArray xs }, o)

                // unknown TAG
                | _  ->
                    failwith "not implemented"

        let (nbt, newOffset) = parse None false 0
        if (newOffset <> buffer.Length) then failwith "Assertion failed. Expected to parse complete buffer."

        (nbt.Payload, x.Region)

    let lookup (name : string) (tag : NbtPayload) : NbtPayload option =
        match tag with
        | PayloadCompound xs ->
            match xs |> List.tryFind (fun x -> x.Name = name) with
            | Some t -> Some t.Payload
            | None -> None
        | _ -> None

    let rec lookupPath (path : string list) (node : NbtPayload) : NbtPayload option =
        match path with
        | [] -> Some node
        | first :: rest ->
            match lookup first node with
            | None -> None
            | Some x -> lookupPath rest x

    let decodeBlockStates (ls : int64[]) (palette : string[]) : string[] =

        let unpackDense bitcount =
            // bitcount bit indices, densely packed
            let bits = new BitArray(MemoryMarshal.AsBytes(ls.AsSpan()).ToArray())
            let rs = Array.zeroCreate 4096
            for i = 0 to 4096-1 do
                let j0 = i * bitcount
                let mutable x = 0
                for j = 0 to bitcount-1 do
                    x <- if bits.Get(j0 + j) then x ||| (1 <<< j) else x
                rs.[i] <- palette.[x]
            rs

        match ls.Length with

        | 256 ->
            let foo = 
                ls
                |> Array.collect (fun l -> 
                    let xs = Array.zeroCreate 16
                    let mutable j = 0
                    for bi = 0 to 7 do
                        let b = int(l >>> (bi <<< 3))
                        xs.[j] <- palette.[ b        &&& 0b00001111]
                        j <- j + 1
                        xs.[j] <- palette.[(b >>> 4) &&& 0b00001111]
                        j <- j + 1
                    xs
                )
            foo

        | 320 -> unpackDense 5
        | 384 -> unpackDense 6
        | 448 -> unpackDense 7

        | 512 ->
            let foo = 
                ls
                |> Array.collect (fun l -> 
                    let xs = Array.zeroCreate 8
                    for j = 0 to 7 do
                        let b = int(l >>> (j <<< 3))
                        xs.[j] <- palette.[b &&& 0b11111111]
                    xs
                )
            foo

        | _ -> sprintf "oh no, failed to decode %d longs" ls.Length |> failwith

    let extractSectionsFromChunk (nbt, region) =
        
        let xPos = match lookupPath [ "Level"; "xPos" ] nbt with | Some (PayloadInt x) -> x | _ -> failwith ""
        let zPos = match lookupPath [ "Level"; "zPos" ] nbt with | Some (PayloadInt x) -> x | _ -> failwith ""

        let sections = lookupPath [ "Level"; "Sections" ] nbt
        match sections with
        | Some (PayloadList xs) ->
            xs
            |> Array.map (fun section ->
                match section with
                | PayloadCompound _ ->
                    let blockStates = match lookup "BlockStates" section with
                                      | Some (PayloadLongArray xs) -> Some xs
                                      | _ -> None
                    let palette     = match lookup "Palette" section with
                                      | Some (PayloadList xs) ->
                                            xs 
                                            |> Array.map (fun x ->
                                                let name = 
                                                    match lookup "Name" x with
                                                    | Some (PayloadString s) -> s
                                                    | _ -> failwith ""
                                                let level = 
                                                    match lookupPath [ "Properties"; "level"] x with
                                                    | Some (PayloadString s) -> Some s
                                                    | _ -> None
                                            
                                                match level with
                                                | None -> name
                                                | Some level -> name + ":" + level

                                                )
                                            |> Some
                                      | _ -> None
                    let y           = match lookup "Y" section with
                                      | Some (PayloadByte x) -> Some(int(x))
                                      | _ -> None
                    
                    match (blockStates, palette, y) with
                    | (Some blockStates, Some palette, Some y) ->
                        Some {
                            Region = region
                            BlockStates = decodeBlockStates blockStates palette
                            Palette = palette
                            Y = y
                            XPos = xPos; ZPos = zPos
                            }
                    
                    | (None, Some palette, Some y) ->
                        let blockStates =
                            match palette.Length with
                            | 1 -> Array.create 4096 (palette.[0])
                            | _ -> failwith "not implemented"
                        Some { 
                            Region = region
                            BlockStates = blockStates
                            Palette = palette
                            Y = y 
                            XPos = xPos; ZPos = zPos
                            }

                    | (None, None, Some y) -> None

                    | _ -> failwith ""

                | _ -> failwith ""
                )
            //|> Seq.filter Option.isSome
            |> Seq.choose id
        | _ -> Array.empty

    let test () =
        
        let sections =
            //@"T:\Dropbox\Data\minecraft\Notre_Dame_and_Medieval_City\Notre Dame and Medieval City"
            @"\\euclid\InOut\aardwars\Notre_Dame_and_Medieval_City\Notre Dame and Medieval City"
            |> getRegions
            //|> Seq.take 1
            |> Seq.collect enumerateRawNbtBuffers
            |> Seq.map parseRawNbtBuffer
            |> Seq.collect extractSectionsFromChunk
            |> Seq.toArray

        let materials = sections |> Seq.collect (fun x -> x.Palette) |> Seq.distinct |> Seq.sort |> Seq.toArray
        for x in materials do printfn "%s" x

        let histo = sections |> Seq.collect (fun x -> x.BlockStates) |> Seq.countBy id |> Seq.sortByDescending (fun (_,c) -> c) |> Seq.toArray
        for (m, c) in histo do printfn "%30s %10d" m c


        //printfn ""
        //printfn "count       : %10d" count
        //printfn "countBedrock: %10d" countBedrock
        //for x in xs do printfn "%A" x

        System.Environment.Exit 0
    
