module Shape
    open Point
    open Vector
    open Ray
    open System.Drawing

    type IShape =
        abstract Intersection: (Ray) -> (float * Point3 * Vector3 * Color) option
        abstract Color:  Color