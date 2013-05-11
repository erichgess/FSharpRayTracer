module Ray
    open Vector
    open Point
    open Matrix
    open System

    type Ray( p: Point3, v: Vector3 ) =
        let origin = p
        let direction = Vector3( v.X, v.Y, v.Z )        // Using Vector4 means that the existing Matrix math will work 
                                                        // and setting W to 0.0 means that translations will not affect the vector.
                                                        // Direction should not be changed by translations.  Scaling won't matter.

        member this.Origin =
            origin

        member this.Direction =
            direction

        static member (*) (m: Matrix, r: Ray ) =
            Ray( m * r.Origin, m * r.Direction )

        static member (*) (r: Ray, m: Matrix ) =
            Ray( r.Origin * m, r.Direction * m )

        static member (*) (t: float, r: Ray ) =
            r.Origin + t * r.Direction

        static member (~-) ( r: Ray ) =
            Ray( r.Origin, -r.Direction )

        member this.Print() =
            sprintf "(%s) -> (%s)" ( this.Origin.Print() ) (this.Direction.Print() )