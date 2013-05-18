module Point
    open System
    open Vector
    open Matrix

    type Point3 ( x: float, y: float, z: float ) =
        member this.X = x
        member this.Y = y
        member this.Z = z

        member this.Item i =
            match i with
            | 0 -> (this.X)
            | 1 -> (this.Y)
            | 2 -> (this.Z)
            | _ -> failwith "Bad Index"

        static member init f =
            let a = Array.init 4 f
            Point3( a.[0], a.[1], a.[2] )

        static member (*) (a: float, p: Point3 ) =
            Point3( a * p.X, a * p.Y, a * p.Z)

        static member (*) ( p: Point3, a: float ) =
            a * p

        static member (-) ( p: Point3, q: Point3 ) =
            Vector3( p.X - q.X, p.Y - q.Y, p.Z - q.Z)

        static member (*) ( p: Point3, v: Vector3 ) =
            p.X*v.X + p.Y*v.Y + p.Z*v.Z

        static member (+) ( p: Point3, v: Vector3 ) =
            Point3( p.X + v.X, p.Y + v.Y, p.Z + v.Z )

        // The 4th row/col of the matrix is added to the point, because that portion
        // of the matrix represents translations.
        static member (*) (m: Matrix, p: Point3 ) =
            Point3.init (fun i -> p.X*m.[i,0] + p.Y*m.[i,1] + p.Z*m.[i,2] + m.[i,3])

        static member (*) ( p: Point3, m: Matrix ) =
            Point3.init (fun i -> p.X*m.[0,i] + p.Y*m.[1,i] + p.Z*m.[2,i] + m.[3,i])

        static member (*) ( p: Point3, q: Point3 ) =
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