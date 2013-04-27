module Ray
    open Vector
    open Point
    open System

    type Ray( p: Point4, v: Vector4 ) =
        let origin = p
        let direction = Vector4( v.X, v.Y, v.Z, 0.0 )       // Using Vector4 means that the existing Matrix math will work 
                                                            // and setting W to 0.0 means that translations will not affect the vector.
                                                            // Direction should not be changed by translations.  Scaling won't matter.

        member this.Origin =
            origin

        member this.Direction =
            direction