module Sphere
    open Matrix
    open Point
    open Ray
    open System

    // The only thing which is needed is the transformation matrix.
    // The center of the sphere can be set with a translation matrix
    // The radius of the sphere can be set with a scaling matrix
    type Sphere ( transformation: Matrix ) =
        let transformation = transformation
        let invTransformation = transformation.Invert()

        member this.Intersection ( r: Ray ) = 
            let transformedRay = invTransformation * r

            let a = transformedRay.Direction * transformedRay.Direction
            let b = transformedRay.Origin * transformedRay.Direction
            let c = transformedRay.Origin * transformedRay.Origin - 1.0

            let discrim = b*b - a*c

            if discrim < 0. then
                false
            else
                let discrimRoot = discrim |> Math.Sqrt

                let q = if b < 0. then (-b - discrimRoot )/2. else (-b + discrimRoot )/2.

                let t0 = q / a
                let t1 = c / q

                let (t0, t1) = if t0 < t1 then (t0, t1) else (t1, t0)

                if t1 < 0. then
                    false
                else
                    true