module RayTracer
    open Shape
    open Ray
    open Point
    open Vector
    open Material
    open Light
    open Scene
    open Color

    type Intersection( point: Point3, normal: Vector3, material: Material, illumination: Color ) =
        member this.Point = point
        member this.Normal = normal
        member this.Material = material
        member this.Illumination = illumination

    type IlluminationTree =
        | NoIllumination
        | IlluminationSource of Intersection * IlluminationTree * IlluminationTree


    let colors = Color.ByName
    let black = colors.["Black"]

    let rec CalculateTotalIllumination (illuminationTree: IlluminationTree) =
        match illuminationTree with
        | NoIllumination -> black
        | IlluminationSource(hit, reflected, refracted ) -> 
                                let percentFromRefraction = 1. - hit.Material.Reflectivity
                                hit.Illumination 
                                + hit.Material.Reflectivity * ( CalculateTotalIllumination reflected ) 
                                + percentFromRefraction * (CalculateTotalIllumination refracted )

    

    let FindIntersections (shapes: IShape list ) ( ray: Ray ) =
        shapes   |> List.map( fun s -> (s.Intersection ray) ) 
                |> List.filter ( 
                    fun h -> match h with
                                | None -> true
                                | Some(time,_,_,_,_) -> 0. < time )

    let FindNearestIntersection (shapes: IShape list) (ray:Ray) =
        FindIntersections shapes ray |> List.reduce (  
                                            fun acc intersection -> 
                                                match acc with
                                                | None -> intersection
                                                | Some(time, _, _, _,_) ->
                                                    match intersection with
                                                    | Some(intersectionTime, _, _, _,_) when intersectionTime < time
                                                        -> intersection
                                                    | _ -> acc )

    let CalculateLightIllumination (material: Material) (point: Point3) (normal: Vector3) (eyeDirection: Vector3) (shapes: IShape list) (light: Light) =
        let surfaceToLight = ( light.Position - point ).Normalize()
        let surfaceToLightRay = new Ray( point + surfaceToLight * 0.0001, surfaceToLight )

        match FindNearestIntersection shapes surfaceToLightRay with
        | None -> material.CalculateLightIllumination eyeDirection surfaceToLight normal light
        | _ -> black

    let TotalIlluminationFromSceneLights (scene: Scene) (material: Material) (point: Point3) (normal: Vector3) (ray: Ray) =
        let CalculateLightIlluminationAtThisPoint = CalculateLightIllumination material point normal -ray.Direction
        scene.Lights |> List.map ( fun light -> CalculateLightIlluminationAtThisPoint scene.Shapes light )
                     |> List.reduce ( fun acc color -> acc + color )
    
    let rec TraceLightRay (scene:Scene) numberOfReflections ray =
        // Find the nearest intersection
        let FindNearestHitInScene = FindNearestIntersection scene.Shapes
        let hit = if numberOfReflections <= 0 then None else FindNearestHitInScene ray

        match hit with
        | None -> black
        | Some(time, point, normal, material, isEntering) -> 
            let CalculateLightIlluminationAtThisPoint = CalculateLightIllumination material point normal -ray.Direction
            let lightingColor = TotalIlluminationFromSceneLights scene material point normal ray

            let lightRays = [ (material.ReflectRay( time, ray, normal ), material.Reflectivity) ]

            let (firstMediumIndex, secondMediumIndex) = if isEntering then (1.0, material.RefractionIndex) else (material.RefractionIndex, 1.0 )
            let lightRays = match material.RefractRay( time, ray, normal, isEntering) with
                            | Some(r) -> (r, 0.7) :: lightRays
                            | _ -> lightRays

            let opticalColor =  lightRays   |> List.map( fun (ray, influence) -> influence * TraceLightRay scene (numberOfReflections-1) ray) 
                                                                    |> List.reduce( fun acc color -> acc + color )

            opticalColor + lightingColor

    let rec BuildLightRayTree (scene: Scene) numberOfReflections ray =
        let FindNearestHitInScene = FindNearestIntersection scene.Shapes
        let hit = if numberOfReflections <= 0 then None else FindNearestHitInScene ray

        match hit with
        | None -> NoIllumination
        | Some(time, point, normal, material, isEntering) -> 
            let CalculateLightIlluminationAtThisPoint = CalculateLightIllumination material point normal -ray.Direction
            let lightingIllumination = TotalIlluminationFromSceneLights scene material point normal ray

            let reflectedRay = material.ReflectRay( time, ray, normal )
            let reflectedIlluminationTree = BuildLightRayTree scene (numberOfReflections - 1) reflectedRay

            let (firstMediumIndex, secondMediumIndex) = if isEntering then (1.0, material.RefractionIndex) else (material.RefractionIndex, 1.0 )
            let refractedIlluminationTree = match material.RefractRay( time, ray, normal, isEntering) with
                                            | None -> NoIllumination
                                            | Some(r) -> BuildLightRayTree scene (numberOfReflections - 1) r

            IlluminationSource( new Intersection( point, normal, material, lightingIllumination ), reflectedIlluminationTree, refractedIlluminationTree )

            
            