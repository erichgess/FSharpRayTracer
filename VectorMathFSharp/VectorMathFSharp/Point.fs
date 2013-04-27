module Point
    open System
    open Matrix

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

        static member (*) (m: Matrix, p: Point4 ) =
            Point4.init (fun i -> p.X*m.[i,0] + p.Y*m.[i,1] + p.Z*m.[i,2] + p.W*m.[i,3])

        static member (*) ( p: Point4, m: Matrix ) =
            Point4.init (fun i -> p.X*m.[0,i] + p.Y*m.[1,i] + p.Z*m.[2,i] + p.W*m.[3,i])