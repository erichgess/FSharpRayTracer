module Point
    open System
    open Vector
    open Matrix

    type Point3 = 
        { X: float; Y: float; Z: float }
        static member init f =
            let a = Array.init 4 f
            { X = a.[0]; Y = a.[1]; Z = a.[2] }

        static member (*) (a: float, p ) =
            { X = a * p.X; Y = a * p.Y; Z = a * p.Z}

        static member (*) ( p, a: float ) =
            a * p

        static member (-) ( p, q ) =
            Vector3( p.X - q.X, p.Y - q.Y, p.Z - q.Z)

        static member (*) ( p, v: Vector3 ) =
            p.X*v.X + p.Y*v.Y + p.Z*v.Z

        static member (+) ( p, v: Vector3 ) =
            { X = p.X + v.X; Y = p.Y + v.Y; Z = p.Z + v.Z }

        // The 4th row/col of the matrix is added to the point, because that portion
        // of the matrix represents translations.
        static member (*) (m: Matrix, p ) =
            Point3.init (fun i -> p.X*m.[i,0] + p.Y*m.[i,1] + p.Z*m.[i,2] + m.[i,3])

        static member (*) ( p, m: Matrix ) =
            Point3.init (fun i -> p.X*m.[0,i] + p.Y*m.[1,i] + p.Z*m.[2,i] + m.[3,i])

        static member (*) ( p, q ) =
            p.X*q.X + p.Y*q.Y + p.Z*q.Z

        member this.Print () =
            sprintf "%f, %f, %f" this.X this.Y this.Z

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

        static member (-) ( p: Point4, q: Point4 ) =
            Vector4( p.X - q.X, p.Y - q.Y, p.Z - q.Z, p.W - q.W )