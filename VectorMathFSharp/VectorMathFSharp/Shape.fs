module Shape
    open Point
    open Vector
    open Ray

    type IShape =
        abstract Intersection: (Ray) -> (float * Vector3) option