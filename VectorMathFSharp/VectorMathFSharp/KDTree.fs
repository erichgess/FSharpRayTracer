module KDTree
    open Point
    open Vector
    open Color

    type KDTreeNode<'a> =
    | Empty 
    | Node of KDTreeNode<'a> * Point3 * 'a * KDTreeNode<'a>

    let rec BuildKdTree ( photons: (Point3*Color) [] ) (depth:int)=
        if Array.length photons = 0 then
            Empty
        else if Array.length photons = 1 then
            let point,color = photons.[0]  // this gets the median data point 
            Node( Empty, point, color, Empty )
        else 
            let axis = (depth) % 3
        
            let length = photons.Length
            let median = length / 2
            let sortedPhotonArray = photons |> Array.sortBy ( fun (p,c) -> p.[axis] )

            // Add an index to each element of the list so that we can easily partition
            let left = Array.sub sortedPhotonArray 0 median
            let right = Array.sub sortedPhotonArray median (sortedPhotonArray.Length - median)

            let point,color = sortedPhotonArray.[median]  // this gets the median data point

            Node ( BuildKdTree left (depth + 1), point, color, BuildKdTree right (depth + 1) )

    let rec FlattenKdTree (tree: KDTreeNode<'a> ) =
        match tree with
        | Empty -> []
        | Node( Empty, p, c, Empty ) -> [(p,c)]
        | Node( left, p, c, right ) -> (p,c) :: FlattenKdTree left @ FlattenKdTree right

    let rec FindAllPointsNearPoint (tree: KDTreeNode<'a>) (target: Point3 ) (radiusSquared: float) (depth: int)=
        match tree with
        | Empty -> []
        | Node( left, point, data, right ) -> 
            let axis = (depth) % 3
            let points =    if target.[axis] < point.[axis] then
                                FindAllPointsNearPoint left target radiusSquared (depth+1)
                            else
                                FindAllPointsNearPoint right target radiusSquared (depth+1)
            let x = point - target
            if x.LengthSquared() <= radiusSquared then
                (point,data) :: points
            else
                points


    let rec FindAllPointsNearPoint2 (tree: KDTreeNode<'a>) (target: Point3 ) (radiusSquared: float) (depth: int)=
        match tree with
        | Empty -> []
        | Node( Empty, point, data, Empty ) -> if (point-target).Length() <= radiusSquared then [data] else []
        | Node( left, point, data, right ) -> 
            let axis = (depth) % 3
            let t1 = target.[axis]
            let p1 = point.[axis]
            let points =    if System.Math.Abs ( t1 - p1 ) <= radiusSquared then
                                (FindAllPointsNearPoint2 left target radiusSquared (depth+1)) 
                                @ (FindAllPointsNearPoint2 right target radiusSquared (depth+1))
                            else if t1 < p1 then
                                FindAllPointsNearPoint2 left target radiusSquared (depth+1)
                            else
                                FindAllPointsNearPoint2 right target radiusSquared (depth+1)
            let x = point - target
            if x.Length() <= radiusSquared then
                data :: points
            else
                points