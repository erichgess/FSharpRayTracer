module CSG
    open Matrix
    open Point
    open Vector
    open Ray
    open Shape
    open Material
    open System

    type CSGTree =
    | Shape of IShape
    | Add of CSGTree * CSGTree
    | Subtract of CSGTree * CSGTree

        static member ToShapeList(tree: CSGTree) =
            match tree with
            | Shape(s) -> [s]
            | Add(l,r)
            | Subtract(l,r) -> List.append (CSGTree.ToShapeList l) (CSGTree.ToShapeList r )



    type CSG ( transformation: Matrix, material: Material, csgTree: CSGTree ) =
        let material = material
        let transformation = transformation
        let invTransformation = transformation.Invert()
        let CsgTree = csgTree

        interface IShape with
            member this.Material = material

            member this.Intersection ( ray: Ray ) = 
                []