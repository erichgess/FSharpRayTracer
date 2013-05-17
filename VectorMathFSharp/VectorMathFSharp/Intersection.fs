module Intersection
    open Point
    open Vector
    open Material
    open Color
    open Shape
    open Ray

    type Intersection( point: Point3, normal: Vector3, material: Material, illumination: Color ) =
        member this.Point = point
        member this.Normal = normal
        member this.Material = material
        member this.Illumination = illumination


    let FindIntersections (shapes: IShape list ) ( ray: Ray ) =
        let intersections = shapes |> List.map( fun s -> (s.Intersection ray) ) |> List.reduce ( fun acc hitList -> List.append acc hitList )

        intersections |> List.filter ( fun (time,_,_,_,_) -> 0. < time )
                      |> List.sortBy ( fun (time,_,_,_,_) -> time )

    let FindNearestIntersection (shapes: IShape list) (ray:Ray) =
        match FindIntersections shapes ray with
        | [] -> None
        | head :: tail -> Some(head)