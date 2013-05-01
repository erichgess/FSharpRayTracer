module Sphere
    open Matrix
    open Point
    open Vector
    open Ray
    open Shape
    open System

    // The only thing which is needed is the transformation matrix.
    // The center of the sphere can be set with a translation matrix
    // The radius of the sphere can be set with a scaling matrix
    type Sphere ( transformation: Matrix ) =
        let transformation = transformation
        let invTransformation = transformation.Invert()

        interface IShape with
            member this.Intersection ( r: Ray ) = 
                let transformedRay = invTransformation * r

                let a = transformedRay.Direction * transformedRay.Direction
                let b = 2. * transformedRay.Origin * transformedRay.Direction
                let c = transformedRay.Origin * transformedRay.Origin - 1.0

                let discrim = b*b - 4.*a*c

                if discrim < 0. then
                    None
                else
                    let discrimRoot = discrim |> Math.Sqrt

                    let q = if b < 0. then (-b - discrimRoot )/2. else (-b + discrimRoot )/2.

                    let t0 = q / a
                    let t1 = c / q

                    let (tFirstHit, tSecondHit) = if t0 < t1 then (t0, t1) else (t1, t0)

                    if tSecondHit < 0. then
                        None
                    else
                        let pointOfIntersection = r.Origin + r.Direction * tFirstHit
                        let normal = transformedRay.Origin + transformedRay.Direction * tFirstHit
                        let normal = invTransformation.Transpose() * Vector3( normal.X, normal.Y, normal.Z)
                        Some( tFirstHit, normal.Normalize() )