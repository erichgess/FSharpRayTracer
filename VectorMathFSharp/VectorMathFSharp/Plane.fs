module Plane
    open Vector
    open Point
    open Matrix
    open Ray
    open Shape
    open System
    open System.Drawing

    type Plane ( transformation: Matrix, color: Color ) =
        let color = color
        let transformation = transformation
        let inverseTransformation = transformation.Invert()
        let plane = transformation * Point3( 0., 0., 0. )
        let normal = (inverseTransformation.Transpose() * Vector3( 0., 1., 0. )).Normalize()

        interface IShape with
            member this.Color = color

            member this.Intersection( r: Ray ) =
                let transformedRay = inverseTransformation * r

                let denom = transformedRay.Direction.Y
            
                if Math.Abs denom <= 0.0001 then
                    None
                else
                    let time = -transformedRay.Origin.Y / denom
                    let intersectionPoint = time * transformedRay
                    let withinSquare = intersectionPoint.X <= 1. && -1. <= intersectionPoint.X
                                        && intersectionPoint.Z <= 1. && -1. <= intersectionPoint.Z
                    if time <= 0. || not withinSquare then
                        None
                    else
                        let shape = this :> IShape
                        Some( time, normal, shape.Color)