module Vector
    open System

    type Vector3 ( x: float, y: float, z: float ) =
        member this.X = x
        member this.Y = y
        member this.Z = z

        static member init f =
            let a = Array.init 3 f
            Vector3( a.[0], a.[1], a.[2] )

        static member (*) (a: float, v: Vector3) =
            Vector3( a*v.X, a*v.Y, a*v.Z )

        static member (*) (v: Vector3, a: float ) =
            a*v

        static member (/) (v: Vector3, a: float ) =
            (1.0/a) * v

        static member (~-) (v: Vector3 ) =
            -1.0 * v

        static member (+) ( u: Vector3, v: Vector3 ) =
            Vector3( u.X + v.X, u.Y + v.Y, u.Z + v.Z )

        static member (-) ( u: Vector3, v: Vector3 ) =
            Vector3( u.X - v.X, u.Y - v.Y, u.Z - v.Z )

        static member (*) ( u: Vector3, v: Vector3 ) =
            u.X*v.X + u.Y*v.Y + u.Z*v.Z

        static member ( >< ) (u: Vector3, v: Vector3 ) =
            Vector3(u.Y*v.Z - u.Z*v.Y, -(u.X*v.Z - u.Z*v.X), u.X*v.Y - u.Y*v.X )

        member this.LengthSquared () =
            x*x + y*y + z*z

        member this.Length () =
            this.LengthSquared () |> Math.Sqrt

        member this.Normalize () =
            this / this.Length()

    type Vector4 ( x: float, y: float, z: float, w: float ) =
        member this.X = x
        member this.Y = y
        member this.Z = z
        member this.W = w

        static member init f =
            let a = Array.init 4 f
            Vector4( a.[0], a.[1], a.[2], a.[3] )

        static member (*) (a: float, v: Vector4) =
            Vector4( a*v.X, a*v.Y, a*v.Z, a*v.W )

        static member (*) (v: Vector4, a: float ) =
            a*v

        static member (/) (v: Vector4, a: float ) =
            (1.0/a) * v

        static member (~-) (v: Vector4 ) =
            -1.0 * v

        static member (+) ( u: Vector4, v: Vector4 ) =
            Vector4( u.X + v.X, u.Y + v.Y, u.Z + v.Z, u.W + v.W )

        static member (-) ( u: Vector4, v: Vector4 ) =
            Vector4( u.X - v.X, u.Y - v.Y, u.Z - v.Z, u.W - v.W )

        member this.LengthSquared () =
            x*x + y*y + z*z + w*w

        member this.Length () =
            this.LengthSquared () |> Math.Sqrt

        member this.Normalize () =
            this / this.Length()