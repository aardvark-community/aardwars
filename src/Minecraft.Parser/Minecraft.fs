namespace Aardwars

open System.IO
open ICSharpCode.SharpZipLib.Zip.Compression.Streams
open System.Text

module Minecraft =
    
    (*
        https://minecraft.fandom.com/wiki/Region_file_format
        https://minecraft.fandom.com/wiki/Chunk_format
        https://minecraft.fandom.com/wiki/NBT_format#Binary_format
    *)

    type Region = { FileName : string; X : int; Z : int }
    type RawNbtBuffer = { Region : Region; X : int; Z : int; Buffer : byte[] }
    
    type NbtPayload =
        | PayloadEnd                        // 0
        | PayloadByte of int8               // 1
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

    let readBigEndianInt16 (xs : byte[]) o : int16 = (int16(xs.[o  ]) <<<  8) ||| (int16(xs.[o+1])       )
    let readBigEndianInt32 (xs : byte[]) o : int32 = (int32(xs.[o  ]) <<< 24) ||| (int32(xs.[o+1]) <<< 16) ||| (int32(xs.[o+2]) <<<  8) ||| (int32(xs.[o+3])       )
    let readBigEndianInt64 (xs : byte[]) o : int64 = (int64(xs.[o  ]) <<< 56) ||| (int64(xs.[o+1]) <<< 48) ||| (int64(xs.[o+2]) <<< 40) ||| (int64(xs.[o+3]) <<< 32) |||
                                                     (int64(xs.[o+4]) <<< 24) ||| (int64(xs.[o+5]) <<< 16) ||| (int64(xs.[o+6]) <<<  8) ||| (int64(xs.[o+7])       )

    let readBigEndian16At (xs : byte[]) o : int = (int(xs.[o]) <<<  8) ||| (int(xs.[o+1])       )
    let readBigEndian24At (xs : byte[]) o : int = (int(xs.[o]) <<< 16) ||| (int(xs.[o+1]) <<< 8 ) ||| (int(xs.[o+2])      )
    let readBigEndian32At (xs : byte[]) o : int = (int(xs.[o]) <<< 24) ||| (int(xs.[o+1]) <<< 16) ||| (int(xs.[o+2]) <<< 8) ||| int(xs.[o+3])

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
            printfn "file size: %d" xs.Length
            for z = 0 to 31 do
                for x = 0 to 31 do
                    let o = (offset x z |> readBigEndian24At xs) <<< 12
                    match o with
                    | 0 -> () // chunk (x, z) does not exist
                    | _ ->
                        let blockCount4k = int32(xs.[o+3]) <<< 12
                        let compressedBufferSize = readBigEndian32At xs o
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

    let rec readPayload (buffer : byte[]) (offset : int) tagId =
        let mutable o = offset
        match tagId with
        
        // TAG_End
        |  0 -> (PayloadEnd, o)
        
        // TAG_Short
        |  2 -> (readBigEndianInt16 buffer o |> PayloadShort, o + 2)
        
        // TAG_Int
        |  3 -> (readBigEndianInt32 buffer o |> PayloadInt, o + 4)
        
        // TAG_Long
        |  4 -> (readBigEndianInt64 buffer o |> PayloadLong, o + 8)
        
        // TAG_Byte_Array
        |  7 -> 
            let count = readBigEndianInt32 buffer o
            o <- o + 4
            let xs = Array.sub buffer o count
            o <- o + count
            (PayloadByteArray xs, o)

        // TAG_String
        |  8 ->
            let length = readBigEndian16At buffer o
            o <- o + 2
            let s = Array.sub buffer o length |> Encoding.UTF8.GetString
            o <- o + length
            (PayloadString s, o)

        // TAG_List
        |  9 ->
            let listItemsTagId = int(buffer.[o])
            o <- o + 1
            let count = readBigEndianInt32 buffer o
            o <- o + 4
            let xs = Array.zeroCreate count
            for i = 0 to count-1 do
                let (x, newOffset) = readPayload buffer o listItemsTagId
                xs.[i] <- x
                o <- newOffset
            (PayloadList xs, o)

        // TAG_Int_Array
        | 11 -> 
            let count = readBigEndianInt32 buffer o
            o <- o + 4
            let xs = Array.zeroCreate count
            for i = 0 to count-1 do
                xs.[i] <- readBigEndianInt32 buffer o
                o <- o + 4
            (PayloadIntArray xs, o)

        // TAG_Long_Array
        | 12 -> 
            let count = readBigEndianInt32 buffer o
            o <- o + 4
            let xs = Array.zeroCreate count
            for i = 0 to count-1 do
                xs.[i] <- readBigEndianInt64 buffer o
                o <- o + 4
            (PayloadLongArray xs, o)

        // unknown TAG
        |  _ -> failwith "not implemented"

    let parseRawNbtBuffer (x : RawNbtBuffer) =

        let buffer = x.Buffer
        File.WriteAllBytes("T:/debug.nbt", buffer)

        let rec parse offset =

            // parses list of tags until PayloadEnd is reached
            let rec parseList o =
                match parse o with
                | ({ Payload = PayloadEnd }, newOffset) -> ([], newOffset)
                | ( tag, o1) -> let (xs, o2) = parseList o1
                                (tag :: xs, o2)

            let mutable o = offset
            let tagId = int(buffer.[o])
            o <- o + 1
            if tagId = 0 then

                // TAG_End
                ({ Name = ""; Payload = PayloadEnd }, o)

            else

                let tagNameLength = readBigEndian16At buffer o
                o <- o + 2
                let tagName = Array.sub buffer o tagNameLength |> Encoding.UTF8.GetString
                o <- o + tagNameLength
                match tagId with
                                
                |  2    // TAG_Short                
                |  3    // TAG_Int                
                |  4    // TAG_Long   
                |  7    // TAG_Byte_Array
                |  8    // TAG_String  
                |  9    // TAG_List              
                | 11    // TAG_Int_Array           
                | 12 -> // TAG_Long_Array
                    let (x, newOffset) = readPayload buffer o tagId
                    ({ Name = tagName; Payload = x }, newOffset)

                // TAG_Compound
                | 10 ->
                    let (xs, newOffset) = parseList o
                    ({ Name = tagName; Payload = PayloadCompound(xs) }, newOffset)

                // unknown TAG
                | _  ->
                    failwith "not implemented"

        parse 0


    let test () =
        
        let xs =
            @"T:\Dropbox\Data\minecraft\Notre_Dame_and_Medieval_City\Notre Dame and Medieval City"
            |> getRegions
            |> Seq.collect enumerateRawNbtBuffers
            |> Seq.map parseRawNbtBuffer
            |> Seq.toArray

        //for x in xs do printfn "%A" x

        System.Environment.Exit 0
    
