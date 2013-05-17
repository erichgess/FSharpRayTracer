module PhotonMapper

    open RayTracer
    open Scene
    open Light
    open System
    open Point
    open Ray
    open KDTree

    let RandomPoint () =
        let rand = new Random()
        Point3( 2. * rand.NextDouble() - 1., 0., rand.NextDouble() * 2. - 1. )
    
    let CalculatePhotonMapForLight (scene: Scene) (light: Light ) =
        // pick a random point in the unit circle
        let rayTarget = 4. * RandomPoint()

        // Create a ray which passes through that point
        let ray = Ray( light.Position, (rayTarget - light.Position).Normalize() )

        // Build an illumination tree for this ray
        let illuminationTree = BuildLightRayTree scene 5 ray

        // Traverse the illumination tree and add each point to a list
        let rec BuildPointList  (tree: IlluminationTree) =
            match tree with
            | NoIllumination -> []
            | IlluminationSource(hit, reflected, refracted ) -> (hit.Point, hit.Illumination) :: BuildPointList refracted
        
        BuildPointList illuminationTree

    let BuildListOfPhotons (scene: Scene) (light: Light) =
        let mutable photonList = []

        for i = 1 to 1000 do
            photonList <- photonList @ (CalculatePhotonMapForLight scene light )
        photonList

    let BuildPhotonMap (scene: Scene) (light: Light ) =
        let photonList = BuildListOfPhotons scene light
        BuildKdTree photonList 0