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
    let light = new Light(Point3( -4., 8., -3. ), Color.init Color.White )
    let light2 = new Light(Point3( 1., 2., -7. ), Color.init Color.Aquamarine )
    let lightSet = [ light; light2 ]
    let scene = [   new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( -1., 0.0, 0. ), new Material(Color.init Color.DarkGray, 0.1, 1.01 )) :> IShape;
                    new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( 1., 0.0, 0. ), new Material( Color.init Color.CornflowerBlue, 0.2, 0. ) ) :> IShape;
                    new Sphere( Matrix.Translate( 0., 3.0, 0. ) * Matrix.Scale( 2., 2., 2. ), new Material( Color.init Color.LightSeaGreen, 0.5, 0. ) ) :> IShape;
                    new Plane( Matrix.Translate( 0., -1., 0.) * Matrix.Scale( 10., 10., 10. ), new Material( Color.init Color.Green, 0.2, 0. ) ) :> IShape;
                    new Plane(  Matrix.RotateY(45.0) * Matrix.Translate( 0., 0., 5.) * Matrix.Scale( 5., 5., 5. ) * Matrix.RotateX( -90.0 ), 
                        new Material( Color.init Color.Blue, 1., 0.) ) :> IShape ]

    let FindNearestHit (ray:Ray) (scene:IShape list) =
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
    
    let rec TraceLightRay numberOfReflections ray =
        // Find the nearest intersection
        let hit = FindNearestHit ray scene

        match hit with
        | None -> Color.init Color.Black
        | Some(time, point, normal, material, isEntering) -> 
                    // Calculate the lighting at this point
                    

                    let lightingColor = lightSet    |> List.map ( fun light -> 
                                                                    let surfaceToLight = ( light.Position - point ).Normalize()
                                                                    let surfaceToLightRay = new Ray( point + surfaceToLight * 0.0001, surfaceToLight )

                                                                    match FindNearestHit surfaceToLightRay scene with
                                                                    | None -> material.CalculateLightInteraction -ray.Direction surfaceToLight normal light2
                                                                    | _ -> Color.init Color.Black )
                                                    |> List.reduce ( fun acc color -> acc + color )

                    let TraceLightBounce = TraceLightRay (numberOfReflections-1)
                    let reflectionColor =   if numberOfReflections > 0 then
                                                let reflectedDirection = -ray.Direction.ReflectAbout normal
                                                let reflectRay = new Ray( time * ray + reflectedDirection * 0.0001, reflectedDirection )
                                                let reflectionColor = TraceLightBounce reflectRay
                                                material.Reflectivity * reflectionColor
                                            else
                                                Color.init Color.Black

                    let refractionColor =   if numberOfReflections > 0 then
                                                let eyeDir = ray.Direction.Normalize()
                                                let (firstMediumIndex, secondMediumIndex) = if isEntering then (1.0, material.RefractionIndex) else (material.RefractionIndex, 1.0 )
                                                let refractedDirection = eyeDir.RefractThrough( normal, firstMediumIndex, secondMediumIndex )
                                                match refractedDirection with
                                                | None -> Color.init Color.Black
                                                | Some(refractedVector) ->  let refractedRay = new Ray( point + refractedVector * 0.0001, refractedVector )
                                                                            let refractedColor = TraceLightBounce refractedRay
                                                                            let refractedColor = 0.7 * refractedColor
                                                                            refractedColor
                                            else
                                                Color.init Color.Black
                    
                    lightingColor + reflectionColor + refractionColor
   
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
    
//    for y = 0 to yResolution-1 do
//        pixelColors := ColorXRow y :: !pixelColors

    let bmp = new Bitmap( xResolution, yResolution )
    !pixelColors |> List.iter ( fun pl -> pl |> List.iter ( fun p -> let (u, v, color) = p 
                                                                     bmp.SetPixel(u, v, color.GetSystemColor() ) ) )
    bmp.Save("test2.bmp" )

    let endTime = System.DateTime.Now
    let duration = (endTime - startTime).TotalSeconds
    printfn "Parallel Duration: %f" duration

    
    0 // return an integer exit code