module Sphere
    open Matrix
    open Point
    open Vector
    open Ray
    open Shape
    open Material
    open System

    // The only thing which is needed is the transformation matrix.
    // The center of the sphere can be set with a translation matrix
    // The radius of the sphere can be set with a scaling matrix
    type Sphere ( transformation: Matrix, material: Material ) =
        let material = material
        let transformation = transformation
        let invTransformation = transformation.Invert()

        interface IShape with
            member this.Material = material

            member this.Intersection ( ray: Ray ) = 
                let transformedRay = invTransformation * ray

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
                    let tHit = if 0. <= tFirstHit then tFirstHit else tSecondHit

                    if tHit < 0. then
                        None
                    else
                        let pointOfIntersection = tHit * ray
                        let normal = transformedRay.Origin + transformedRay.Direction * tHit
                        let normal = invTransformation.Transpose() * Vector3( normal.X, normal.Y, normal.Z)

                        let isEntering = if tFirstHit > 0. then true else false
                        let normal = if isEntering then normal else -normal

                        let shape = this :> IShape
                        Some( tHit, pointOfIntersection, normal.Normalize(), shape.Material, isEntering )