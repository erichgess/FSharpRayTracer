﻿module PhotonMapper

    open RayTracer
    open Scene
    open Light
    open System
    open Point
    open Ray
    open KDTree
    open System.Threading
    open System.Threading.Tasks
    
    let rand = new Random()

    let RandomPoint () =
        Point3( 2. * rand.NextDouble() - 1., 0., rand.NextDouble() * 2. - 1. )
    
    let CalculatePhotonMapForLight (scene: Scene) (light: Light ) =
        // pick a random point in the unit circle
        let rayTarget = 4. * RandomPoint()

        // Create a ray which passes through that point
        let ray = Ray( light.Position, (rayTarget - light.Position).Normalize() )

        // Build an illumination tree for this ray
        let illuminationTree = BuildLightRayTree scene 15 ray

        // Traverse the illumination tree and add each point to a list
        let rec BuildPointList  (tree: IlluminationTree) =
            match tree with
            | NoIllumination -> []
            | IlluminationSource(hit, reflected, refracted ) -> (hit.Point, hit.Illumination) :: BuildPointList refracted @ BuildPointList reflected
        
        BuildPointList illuminationTree

    let BuildListOfPhotons (loops: int) (scene: Scene) (light: Light) =
        let photonList = ref []

        let _ = Parallel.For( 1, 10000, new System.Action<int>( fun i ->
                                                                    let newPhotonList = CalculatePhotonMapForLight scene light
                                                                    lock photonList ( fun () -> photonList := newPhotonList @ !photonList ) ) )
        !photonList

    let BuildPhotonMap (scene: Scene) (light: Light ) =
        let photonList = BuildListOfPhotons 10000 scene light
        printfn "Photons: %d" ( List.length photonList )
        BuildKdTree photonList 0