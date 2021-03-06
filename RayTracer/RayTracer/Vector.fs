﻿module Vector
    open Matrix
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

        static member (><) (u: Vector3, v: Vector3 ) =
            Vector3(u.Y*v.Z - u.Z*v.Y, -(u.X*v.Z - u.Z*v.X), u.X*v.Y - u.Y*v.X )

        member this.LengthSquared () =
            x*x + y*y + z*z

        member this.Length () =
            this.LengthSquared () |> Math.Sqrt

        member this.Normalize () =
            this / this.Length()

        member this.ReflectAbout( n: Vector3 ) =
            2. * ( n * this ) * n - this

        member this.RefractThrough ( normal: Vector3, firstMediumRefractiveIndex: float, secondMediumRefractiveIndex: float ) =
            // refract the ray about the normal
            let ratio = firstMediumRefractiveIndex/secondMediumRefractiveIndex
            let mDotR = -this * normal
            let cos_theta_sqr = 1. - ratio * ratio * ( 1. - mDotR*mDotR)

            // Cos theta squared is <= 0 then there is total internal reflection, so don't bother
            // with calculating the refraction vector
            if cos_theta_sqr > 0. then
                let cos_theta = System.Math.Sqrt cos_theta_sqr
                Some(this * ratio + normal * ( ratio * mDotR - cos_theta))
            else
                None

        member this.Print() =
            sprintf "%f, %f, %f" x y z

        // This operation does not use the 4th column/row of the matrix, this is because
        // that portion of the matrix is used for translations and Vectors should not be
        // translated, because for our application they represent direction.
        static member (*) (m: Matrix, v: Vector3 ) =
            Vector3.init (fun i -> v.X*m.[i,0] + v.Y*m.[i,1] + v.Z*m.[i,2])

        static member (*) ( v: Vector3, m: Matrix ) =
            Vector3.init (fun i -> v.X*m.[0,i] + v.Y*m.[1,i] + v.Z*m.[2,i])

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

        member this.Vector3 () =
            Vector3( this.X, this.Y, this.Z )

        static member (*) (m: Matrix, v: Vector4 ) =
            Vector4.init (fun i -> v.X*m.[i,0] + v.Y*m.[i,1] + v.Z*m.[i,2] + v.W*m.[i,3])

        static member (*) ( v: Vector4, m: Matrix ) =
            Vector4.init (fun i -> v.X*m.[0,i] + v.Y*m.[1,i] + v.Z*m.[2,i] + v.W*m.[3,i])