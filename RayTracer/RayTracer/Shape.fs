module Shape
    open Point
    open Vector
    open Ray
    open Material
    open System.Drawing

    type Shape = { Intersection: (Ray) -> (float * Point3 * Vector3 * Material * bool) option; Material:  Material }