﻿module KDTree
    open Point
    open Vector
    open Color

    type KDTreeNode<'a> =
    | Empty 
    | Node of KDTreeNode<'a> * Point3 * 'a * KDTreeNode<'a>

    let rec BuildKdTree ( photonList: (Point3*Color) list ) (depth:int)=
        if List.length photonList = 0 then
            Empty
        else 
            let axis = depth % 3
        
            let length = List.length photonList
            let median = (List.length photonList) / 2
            let sortedPhotonArray = photonList |> List.sortBy ( fun (p,c) -> p.[axis] ) |> List.toArray

            // Add an index to each element of the list so that we can easily partition
            let sortedPhotonList = List.init length (fun i -> (i,sortedPhotonArray.[i]) )
            let left,right = sortedPhotonList |> List.partition ( fun (i,_) -> i < median )

            // Remove the index values
            let left = left |> List.map ( fun (i,x) -> x )
            let right = right |> List.map ( fun (i,x) -> x )

            let point,color = List.head right  // this gets the median data point
            let right = List.tail right        // this will remove the median element

            Node ( BuildKdTree left (depth + 1), point, color, BuildKdTree right (depth + 1) )

    let rec FindAllPointsNearPoint (tree: KDTreeNode<'a>) (target: Point3 ) (radiusSquared: float) (depth: int)=
        match tree with
        | Empty -> []
        | Node( left, point, data, right ) -> 
            let axis = depth % 3
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
        | Node( left, point, data, right ) -> 
            let axis = depth % 3
            let points =    if System.Math.Abs ( target.[axis] - point.[axis] ) < radiusSquared then
                                (FindAllPointsNearPoint left target radiusSquared (depth+1)) 
                                @ (FindAllPointsNearPoint right target radiusSquared (depth+1))
                            else if target.[axis] < point.[axis] then
                                FindAllPointsNearPoint left target radiusSquared (depth+1)
                            else
                                FindAllPointsNearPoint right target radiusSquared (depth+1)
            let x = point - target
            if x.LengthSquared() <= radiusSquared then
                (point,data) :: points
            else
                points