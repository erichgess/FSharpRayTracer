module Plane
    open Vector
    open Point
    open Matrix
    open Ray
    open Shape
    open System

    type Plane ( transformation: Matrix ) =
        let transformation = transformation
        let inverseTransformation = transformation.Invert()
        let plane = transformation * Point3( 0., 0., 0. )
        let normal = (inverseTransformation.Transpose() * Vector3( 0., 1., 0. )).Normalize()

        interface IShape with
            member this.Intersection( r: Ray ) =
                let transformedRay = inverseTransformation * r

                let denom = transformedRay.Direction.Y
            
                if Math.Abs denom <= 0.0001 then
                    None
                else
                    let time = -transformedRay.Origin.Y / denom

                    if time <= 0. then
                        None
                    else
                        Some( time, normal)