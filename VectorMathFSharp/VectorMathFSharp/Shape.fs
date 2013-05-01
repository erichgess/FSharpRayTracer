module Shape
    open Point
    open Vector
    open Ray

    type IShape =
        abstract Intersection: (Ray) -> (Point3 * Vector3) option