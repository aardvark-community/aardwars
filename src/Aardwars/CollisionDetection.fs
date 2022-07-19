namespace Aardwars

open Aardvark.Base

module Motion = 

    let repairMotion (p0 : V3d) (p1 : V3d) (b : Box3d) (r : float) =
        let d1 = 
            if b.Contains p1 then 0.0
            else b.GetMinimalDistanceTo p1

        if d1 > r then
            None
        else
            let dir = p1 - p0

            let pt = p1.GetClosestPointOn(b)
            let d = Vec.distance pt p1
            if d <= r then
                let e = 1E-1
                let x = 
                    if Fun.ApproximateEquals(pt.X, b.Max.X,e) then 1
                    elif Fun.ApproximateEquals(pt.X, b.Min.X,e) then -1
                    else 0
                let y = 
                    if Fun.ApproximateEquals(pt.Y, b.Max.Y,e) then 1
                    elif Fun.ApproximateEquals(pt.Y, b.Min.Y,e) then -1
                    else 0
                let z = 
                    if Fun.ApproximateEquals(pt.Z, b.Max.Z,e) then 1
                    elif Fun.ApproximateEquals(pt.Z, b.Min.Z,e) then -1
                    else 0

                let inline repairX() =
                    if x > 0 then Some (false, V3d(b.Max.X + r, p1.Y, p1.Z))
                    else Some (false, V3d(b.Min.X - r, p1.Y, p1.Z))
                
                let inline repairY() =
                    if y > 0 then Some (false,V3d(p1.X, b.Max.Y + r, p1.Z))
                    else Some (false, V3d(p1.X, b.Min.Y - r, p1.Z))
                
                let inline repairZ() =
                    if z > 0 then Some (true, V3d(p1.X, p1.Y, b.Max.Z + r))
                    else Some (false, V3d(p1.X, p1.Y, b.Min.Z - r))


                if x <> 0 && y = 0 && z = 0 then repairX()
                elif x = 0 && y <> 0 && z = 0 then repairY()
                elif x = 0 && y = 0 && z <> 0 then repairZ()

                elif x <> 0 && y <> 0 && z = 0 then
                    if (p0.XY - pt.XY).Abs().MajorDim = 0 then repairX()
                    else repairY()
                    
                elif x <> 0 && y = 0 && z <> 0 then
                    if (p0.XZ - pt.XZ).Abs().MajorDim = 0 then repairX()
                    else repairZ()
                
                elif x = 0 && y <> 0 && z <> 0 then
                    if (p0.YZ - pt.YZ).Abs().MajorDim = 0 then repairY()
                    else repairZ()

                else
                    match (p0 - pt).Abs().MajorDim with
                    | 0 -> repairX()
                    | 1 -> repairY()
                    | _ -> repairZ()

            else
                None