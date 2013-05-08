module Shape
    open Point
    open Vector
    open Ray
    open Material
    open System.Drawing

    type IShape =
        abstract Intersection: (Ray) -> (float * Point3 * Vector3 * Material * bool) option
        abstract Material:  Material