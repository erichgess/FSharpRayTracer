module CSG
    open Matrix
    open Point
    open Vector
    open Ray
    open Shape
    open Material
    open System

    type Operand =
    | Left
    | Right

    type CSGTree =
    | Shape of IShape
    | Add of CSGTree * CSGTree
    | Subtract of CSGTree * CSGTree

        static member ToShapeList(tree: CSGTree) =
            match tree with
            | Shape(s) -> [s]
            | Add(l,r)
            | Subtract(l,r) -> List.append (CSGTree.ToShapeList l) (CSGTree.ToShapeList r )


        static member AddIntersection left right =
            // the first isEntering intersection
            // if the first intersection is not IsEntering then go with the first not IsEntering
            let left = List.map ( fun (t, p, n, m, e) -> (t, p, n, m, e, Left ) ) left
            let right = List.map ( fun (t, p, n, m, e) -> (t, p, n, m, e, Right ) ) right
            let intersections = List.append left right |> List.sortBy ( fun (time,_,_,_,_,_) -> time )

            []

        member this.Intersection ( ray: Ray ) = 
                match this with
                | Shape(s) -> s.Intersection ray
                | Add(l,r) -> let leftIntersections = l.Intersection ray
                              let rightIntersections = l.Intersection ray
                              CSGTree.AddIntersection leftIntersections rightIntersections
                | Subtract(l,r) -> []


    type CSG ( transformation: Matrix, material: Material, csgTree: CSGTree ) =
        let material = material
        let transformation = transformation
        let invTransformation = transformation.Invert()
        let CsgTree = csgTree

        static member AddIntersection (left: IShape) (right: IShape) =
            []

        interface IShape with
            member this.Material = material

            member this.Intersection ( ray: Ray ) = 
                csgTree.Intersection ray