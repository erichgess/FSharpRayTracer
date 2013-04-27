module Point
    open System

    type Point4 ( x: float, y: float, z: float, w: float ) =
        member this.X = x
        member this.Y = y
        member this.Z = z
        member this.W = w

        static member init f =
            let a = Array.init 4 f
            Point4( a.[0], a.[1], a.[2], a.[3] )

        static member (*) (a: float, p: Point4 ) =
            Point4( a * p.X, a * p.Y, a * p.Z, a * p.W )

        static member (*) ( p: Point4, a: float ) =
            a * p