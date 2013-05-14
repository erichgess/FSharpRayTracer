open Matrix
open Ray
open Point
open Vector
open Shape
open Sphere
open Plane
open Color
open Light
open Material
open BRDF
open System.Threading
open System.Threading.Tasks
open System.Timers


let xResolution = 512
let yResolution = 512

let GetCameraRay (u: int) (v: int ) =
    let center = Vector3( 0., 0., -8. )
    let xmin = -3
    let xmax = 3
    let ymin = -3
    let ymax = 3

    let xDelta = float( xmax - xmin ) / float(xResolution )
    let yDelta = float( ymax - ymin ) / float( yResolution )
    let xPos = float(xmin) + float(u) * xDelta
    let yPos = float(ymax) - float(v) * yDelta
    let viewPoint = Vector3( xPos, yPos, 0. )
    Ray( Point3( center.X, center.Y, center.Z ), (viewPoint - center).Normalize() )


[<EntryPoint>]
let main argv = 
    let colors = Color.ByName
    let black = colors.["Black"]
    let l = new Light(Point3( -4., 8., -3. ), colors.["White"] )
    let l2 = new Light(Point3( 1., 2., -7. ), colors.["Aquamarine"] )
    let lightSet = [ l; l2 ]

    let cookTorranceMaterial = new MaterialFactory( Lambertian, CookTorrance 0.3 2.1 )
    let phong20Material = new MaterialFactory( Lambertian, Phong 20. )
    let phong150Material = new MaterialFactory( Lambertian, Phong 150.0 )
    let phong400Material = new MaterialFactory( Lambertian, Phong 400.0 )
    let phong600Material = new MaterialFactory( Lambertian, Phong 600.0 )

    let scene = [   new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( -1., 0.0, 0.0 ), 
                                cookTorranceMaterial.CreateMaterial( 0.8 * colors.["CornflowerBlue"], 
                                 colors.["Red"], 0.0, 1.01 )) :> IShape;

                    new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( 1., 0.0, 0. ), 
                                phong20Material.CreateMaterial( colors.["CornflowerBlue"], 
                                 colors.["CornflowerBlue"], 0.3, 0. ) ) :> IShape;

                    new Sphere( Matrix.Translate( 0., 3.0, 0. ) * Matrix.Scale( 2., 2., 2. ), 
                                phong150Material.CreateMaterial( colors.["LightSeaGreen"], 
                                 colors.["LightSeaGreen"], 0.5, 0. ) ) :> IShape;

                    new Plane( Matrix.Translate( 0., -1., 0.) * Matrix.Scale( 10., 10., 10. ), 
                               phong400Material.CreateMaterial( colors.["Green"], 
                                colors.["Green"], 0.2, 0. ) ) :> IShape;

                    new Plane(  Matrix.RotateY(45.0) * Matrix.Translate( 0., 0., 5.) * Matrix.Scale( 50., 50., 50. ) * Matrix.RotateX( -90.0 ), 
                                phong600Material.CreateMaterial( colors.["Blue"], 
                                 colors.["Blue"], 1., 0.) ) :> IShape 
                ]

    let FindNearestHit (scene:IShape list) (ray:Ray) =
        // This finds all the intersections on this ray
        let intersections = scene   |> List.map( fun s -> (s.Intersection ray) ) 
                                    |> List.filter ( fun h -> match h with
                                                              | None -> true
                                                              | Some(time,_,_,_,_) -> 0. < time )
        
        // This finds the nearest intersection
        if intersections.Length = 0 then
            None
        else
            intersections |> List.reduce ( fun acc intersection -> 
                match acc with
                | None -> intersection
                | Some(time, _, _, _,_) ->
                    match intersection with
                    | Some(intersectionTime, _, _, _,_) when intersectionTime < time
                        -> intersection
                    | _ -> acc
                )



    let CalculateLightIllumination (material: Material) (point: Point3) (normal: Vector3) (eyeDirection: Vector3) (light: Light) =
        let surfaceToLight = ( light.Position - point ).Normalize()
        let surfaceToLightRay = new Ray( point + surfaceToLight * 0.0001, surfaceToLight )

        match FindNearestHit scene surfaceToLightRay with
        | None -> material.CalculateLightIllumination eyeDirection surfaceToLight normal light
        | _ -> black


    
    let rec TraceLightRay numberOfReflections ray =
        // Find the nearest intersection
        let FindNearestHitInScene = FindNearestHit scene
        let hit = FindNearestHitInScene ray

        match hit with
        | None -> black
        | Some(time, point, normal, material, isEntering) -> 
            let CalculateLightIlluminationAtThisPoint = CalculateLightIllumination material point normal -ray.Direction
            let lightingColor = lightSet    |> List.map ( fun light -> CalculateLightIlluminationAtThisPoint light )
                                            |> List.reduce ( fun acc color -> acc + color )

            let lightRays = [ (material.ReflectRay( time, ray, normal ), material.Reflectivity) ]

            let (firstMediumIndex, secondMediumIndex) = if isEntering then (1.0, material.RefractionIndex) else (material.RefractionIndex, 1.0 )
            let lightRays = match material.RefractRay( time, ray, normal, isEntering) with
                            | Some(r) -> (r, 0.7) :: lightRays
                            | _ -> lightRays

            if numberOfReflections > 0 then
                let opticalColor =  lightRays   |> List.map( fun (ray, influence) -> influence * TraceLightRay (numberOfReflections-1) ray) 
                                                |> List.reduce( fun acc color -> acc + color )
                opticalColor + lightingColor
            else
                black
   
    let ColorPixel u v =
        let ray = GetCameraRay u v
        TraceLightRay 5 ray

    
    let ColorXRow v =
        let mutable pixels = []
        for u = 0 to xResolution-1 do 
            let shade = ColorPixel u v
            pixels <- (u, v, shade) :: pixels
        pixels

    let startTime = System.DateTime.Now
    let pixelColors = ref []
    let _ = Parallel.For( 0, yResolution - 1, new System.Action<int>( fun y -> let row = ColorXRow y
                                                                               lock pixelColors ( fun () -> pixelColors := row :: !pixelColors )  ) )

    let bmp = new System.Drawing.Bitmap( xResolution, yResolution )
    !pixelColors |> List.iter ( fun pl -> pl |> List.iter ( fun p -> let (u, v, color) = p 
                                                                     bmp.SetPixel(u, v, color.GetSystemColor() ) ) )
    bmp.Save("test2.bmp" )
    let endTime = System.DateTime.Now
    let duration = (endTime - startTime).TotalSeconds
    printfn "Parallel Duration: %f" duration

    
    0 // return an integer exit code