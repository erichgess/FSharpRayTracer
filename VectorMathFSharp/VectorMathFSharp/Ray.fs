module Ray
    open Vector
    open Point
    open Matrix
    open Material
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

        member this.ReflectAt (time: float, normal: Vector3 ) =
            let reflectedDirection = -this.Direction.ReflectAbout normal
            new Ray( time * this + reflectedDirection * 0.0001, reflectedDirection )

        member this.RefractAt ( time: float, normal: Vector3, firstMediumIndex: float, secondMediumIndex: float ) =
            let eyeDir = this.Direction.Normalize()
            match eyeDir.RefractThrough( normal, firstMediumIndex, secondMediumIndex ) with
            | None -> None
            | Some(refractedDirection)-> Some(new Ray( time * this + refractedDirection * 0.0001, refractedDirection ))

        member this.Print() =
            sprintf "(%s) -> (%s)" ( this.Origin.Print() ) (this.Direction.Print() )