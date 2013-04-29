module Sphere
    open Matrix
    open Point
    open Ray

    // The only thing which is needed is the transformation matrix.
    // The center of the sphere can be set with a translation matrix
    // The radius of the sphere can be set with a scaling matrix
    type Sphere ( transformation: Matrix ) =
        let transformation = transformation
        let invTransformation = transformation.Invert()

        member this.Intersection ( r: Ray ) = 
            let transformedRay = invTransformation * r

            let A = transformedRay.Direction * transformedRay.Direction
            let B = transformedRay.Origin * transformedRay.Direction
            let C = transformedRay.Origin * transformedRay.Origin - 1.0

            let discrim = B*B - A*C

            if discrim < 0. then
                false
            else 
                true

//            let discrimRoot = discrim |> Math.Sqrt
//
//            let t1 = ( -B - discrimRoot ) / A;
//
//            if t1 > Math.Epsilon then
//                true