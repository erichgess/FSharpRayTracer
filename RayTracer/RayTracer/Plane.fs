module Plane
    open Vector
    open Point
    open Matrix
    open Ray
    open Material
    open Shape
    open System
    open System.Drawing

    let Plane (transformation:Matrix) (material:Material) =
        {   Material = material; 
            Intersection = (fun ray -> 
                                let inverseTransformation = transformation.Invert()
                                let plane = transformation * Point3( 0., 0., 0. )
                                let normal = (inverseTransformation.Transpose() * Vector3( 0., 1., 0. )).Normalize()

                                let transformedRay = inverseTransformation * ray

                                let denom = transformedRay.Direction.Y
            
                                if Math.Abs denom <= 0.0001 then
                                    None
                                else
                                    let time = -transformedRay.Origin.Y / denom
                                    let transformedIntersectionPoint = time * transformedRay
                                    let withinSquare = transformedIntersectionPoint.X <= 1. && -1. <= transformedIntersectionPoint.X
                                                        && transformedIntersectionPoint.Z <= 1. && -1. <= transformedIntersectionPoint.Z
                                    if time <= 0. || not withinSquare then
                                        None
                                    else
                                        Some( time, time * ray, normal, material, true))}