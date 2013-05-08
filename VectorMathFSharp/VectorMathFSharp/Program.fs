open Matrix
open Ray
open Point
open Vector
open Shape
open Sphere
open Plane
open Light
open Material
open System.Drawing
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
    let light = new Light(Point3( -4., 8., -3. ), Color.White )
    let light2 = new Light(Point3( 0., -9., -7. ), Color.White )
    let lightSet = [ light; light2 ]
    let scene = [   new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( -1., 0.0, 0. ), new Material(Color.DarkGray, 0.4, 1.52 )) :> IShape;
                    new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( 1., 0.0, 0. ), new Material( Color.CornflowerBlue, 0.2, 0. ) ) :> IShape;
                    new Sphere( Matrix.Translate( 0., 3.0, 0. ) * Matrix.Scale( 2., 2., 2. ), new Material( Color.LightSeaGreen, 0.5, 0. ) ) :> IShape;
                    new Plane( Matrix.Translate( 0., -1., 0.) * Matrix.Scale( 10., 10., 10. ), new Material( Color.Green, 0.2, 0. ) ) :> IShape;
                    new Plane(  Matrix.RotateY(45.0) * Matrix.Translate( 0., 0., 5.) * Matrix.Scale( 5., 5., 5. ) * Matrix.RotateX( -90.0 ), 
                        new Material( Color.Blue, 1., 0.) ) :> IShape ]

    let FindNearestHit (ray:Ray) (scene:IShape list) =
        // This finds all the intersections on this ray
        let intersections = scene   |> List.map( fun s -> (s.Intersection ray) ) 
                                    |> List.filter ( fun h -> match h with
                                                              | None -> true
                                                              | Some(time,_,_,_) -> 0. < time )
        
        // This finds the nearest intersection
        if intersections.Length = 0 then
            None
        else
            intersections |> List.reduce ( fun acc intersection -> 
                match acc with
                | None -> intersection
                | Some(time, _, _, _) ->
                    match intersection with
                    | Some(intersectionTime, _, _, _) when intersectionTime < time
                        -> intersection
                    | _ -> acc
                )
    
    let rec TraceLightRayRefactor numberOfReflections ray =
        // Find the nearest intersection
        let hit = FindNearestHit ray scene

        match hit with
        | None -> Color.Black
        | Some(time, point, normal, material) -> 
                    // Calculate the lighting at this point
                    let surfaceToLight = ( light.Position - point ).Normalize()
                    let surfaceToLightRay = new Ray( point + surfaceToLight * 0.0001, surfaceToLight )
                    let lightingColor = material.CalculateLightInteraction -ray.Direction surfaceToLight normal light2

                    // If numberOfReflections > 0 then
                    if numberOfReflections > 0 then
                        // reflect the ray about the normal
                        let reflectedDirection = -ray.Direction.ReflectAbout normal
                        // Call tracelightray
                        let reflectionColor = TraceLightRayRefactor (numberOfReflections-1) (new Ray( time * ray + reflectedDirection * 0.0001, reflectedDirection ))
                        // Add the reflected ray color to the surface color
                        let reflectionColor = ScaleColor material.Reflectivity reflectionColor

                        let lightingColor = AddColors reflectionColor lightingColor

                        // refract the ray about the normal
                        let eyeDir = ray.Direction.Normalize()
                        let refractedDirection = eyeDir.RefractThrough( normal, 1.0, material.RefractionIndex )
                        match refractedDirection with
                                            | Some(refractedVector) ->  let refractedRay = new Ray( point + refractedVector * 0.0001, refractedVector )
                                                                        let refractedColor = TraceLightRayRefactor (numberOfReflections - 1) refractedRay
                                                                        let refractedColor = ScaleColor 0.7 refractedColor
                                                                        AddColors refractedColor lightingColor
                                            | None -> lightingColor
                    else
                        lightingColor

    let rec TraceLightRay numberOfReflections ray = 
        let hit = FindNearestHit ray scene

        if numberOfReflections = 0 then
            hit :: []
        else match hit with
             | None -> []
             | Some( time,_, normal,_) -> let reflectedDirection = -ray.Direction.ReflectAbout normal
                                          hit :: TraceLightRay (numberOfReflections - 1) ( new Ray( time * ray + reflectedDirection * 0.0001, reflectedDirection ))

    let CalculateShading (light: Light) (ray:Ray) (nearestShape:(float*Point3*Vector3*Material) option ) =
        match nearestShape with
        | None -> Color.Black
        | Some(time, point, n, material) -> 
            let surfaceToLight = ( light.Position - point ).Normalize()

            // This small value is to prevent self intersection with the surface near the origin
            let surfaceToLightRay = new Ray( point + surfaceToLight * 0.0001, surfaceToLight )

            // Check if this surface point is able to see the light
            match (TraceLightRay 0 surfaceToLightRay) with
            | Some(_) :: tail -> Color.Black
            | _ -> material.CalculateLightInteraction -ray.Direction surfaceToLightRay.Direction n light
   
    let ColorPixel u v =
        let ray = GetCameraRay u v
        TraceLightRayRefactor 5 ray

    
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
    
//    for y = 0 to yResolution-1 do
//        pixelColors := ColorXRow y :: !pixelColors

    let bmp = new Bitmap( xResolution, yResolution )
    !pixelColors |> List.iter ( fun pl -> pl |> List.iter ( fun p -> let (u, v, color) = p 
                                                                     bmp.SetPixel(u, v, color) ) )
    bmp.Save("test2.bmp" )

    let endTime = System.DateTime.Now
    let duration = (endTime - startTime).TotalSeconds
    printfn "Parallel Duration: %f" duration

    
    0 // return an integer exit code