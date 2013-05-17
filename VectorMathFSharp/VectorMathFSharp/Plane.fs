module Plane
    open Vector
    open Point
    open Matrix
    open Ray
    open Material
    open Shape
    open System
    open System.Drawing

    type Plane ( transformation: Matrix, material: Material ) =
        let material = material
        let transformation = transformation
        let inverseTransformation = transformation.Invert()
        let plane = transformation * Point3( 0., 0., 0. )
        let normal = (inverseTransformation.Transpose() * Vector3( 0., 1., 0. )).Normalize()

        interface IShape with
            member this.Material = material

            member this.Intersection( ray: Ray ) =
                let transformedRay = inverseTransformation * ray

                let denom = transformedRay.Direction.Y
            
                if Math.Abs denom <= 0.0001 then
                    []
                else
                    let time = -transformedRay.Origin.Y / denom
                    let transformedIntersectionPoint = time * transformedRay
                    let withinSquare = transformedIntersectionPoint.X <= 1. && -1. <= transformedIntersectionPoint.X
                                        && transformedIntersectionPoint.Z <= 1. && -1. <= transformedIntersectionPoint.Z
                    if time <= 0. || not withinSquare then
                        []
                    else
                        let shape = this :> IShape
                        [( time, time * ray, normal, shape.Material, true)]