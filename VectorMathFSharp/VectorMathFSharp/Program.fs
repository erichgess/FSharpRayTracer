open Matrix
open Ray
open Point
open Vector
open Shape
open Sphere
open Plane
open Light
open System.Drawing
open System.Threading
open System.Threading.Tasks
open System.Timers


let xResolution = 512
let yResolution = 512

let GetCameraRay (u: int) (v: int ) =
    let center = Vector3( 0., 0., -8. )
    let xmin = -5
    let xmax = 5
    let ymin = -5
    let ymax = 5

    let xDelta = float( xmax - xmin ) / float(xResolution )
    let yDelta = float( ymax - ymin ) / float( yResolution )
    let xPos = float(xmin) + float(u) * xDelta
    let yPos = float(ymax) - float(v) * yDelta
    let viewPoint = Vector3( xPos, yPos, 0. )
    Ray( Point3( center.X, center.Y, center.Z ), (viewPoint - center).Normalize() )


[<EntryPoint>]
let main argv = 
    let light = new Light(Point3( -4., 8., -3. ), Color.White )
    let light2 = new Light(Point3( 0., 9., -7. ), Color.White )
    let lightSet = [ light; light2 ]
    let scene = [   new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( -1., 0.0, 0. ), Color.Gray) :> IShape;
                    new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( 1., 0.0, 0. ), Color.CornflowerBlue) :> IShape;
                    new Sphere( Matrix.Translate( 0., 3.0, 0. ) * Matrix.Scale( 2., 2., 2. ), Color.LightSeaGreen) :> IShape;
                    new Plane( Matrix.Translate( 0., -1., 0.) * Matrix.Scale( 10., 10., 10. ), Color.Green) :> IShape;
                    new Plane( Matrix.RotateY( 45. ) * Matrix.Translate( 0., 0., 5.) * Matrix.Scale( 10., 10., 10. ) * Matrix.RotateX( -90.0 ), Color.DarkBlue) :> IShape ]

    let rec TraceLightRay numberOfReflections ray = 
        // This finds all the intersections on this ray
        let intersections = scene   |> List.map( fun s -> (s.Intersection ray) )
        
        // This finds the nearest intersection
        let hit = intersections |> List.reduce ( fun acc intersection -> 
            match acc with
            | None -> intersection
            | Some(time, point, normal, color) ->
                match intersection with
                | Some(intersectionTime, point, intersectionNormal, intersectionColor) when intersectionTime < time 
                    -> intersection
                | _ -> acc
            )

        if numberOfReflections = 0 then
            hit :: []
        else match hit with
             | None -> []
             | Some( time,_, normal,_) -> let reflectedDirection = -ray.Direction.ReflectAbout normal
                                          hit :: TraceLightRay (numberOfReflections - 1) ( new Ray( time * ray + reflectedDirection * 0.0001, reflectedDirection ))

    let CalculateShading (light: Light) (ray:Ray) (nearestShape:(float*Point3*Vector3*Color) option ) =
        match nearestShape with
        | None -> Color.Black
        | Some(time, point, n, color) -> 
            let surfaceToLight = ( light.Position - point ).Normalize()

            // This small value is to prevent self intersection with the surface near the origin
            let surfaceToLightRay = new Ray( point + surfaceToLight * 0.0001, surfaceToLight )

            match (TraceLightRay 0 surfaceToLightRay) with
            | Some(_) :: tail -> Color.Black
            | _ -> light.CalculateSurfaceInteration -ray.Direction surfaceToLightRay.Direction n color
   
    let ColorPixel u v =
        let ray = GetCameraRay u v
        let lightRayPath = TraceLightRay 15 ray |> List.rev     // Trace a ray of light from its starting point to the eye
        match lightRayPath with
        | [] -> Color.Black
        | _  -> lightRayPath |> List.map ( fun hit -> lightSet |> List.map ( fun l -> CalculateShading l ray hit ) 
                                                               |> List.reduce ( fun acc l -> AddColors acc l ) )
                             |> List.reduce( fun acc color -> AddColors ( ScaleColor 0.5 acc) color )

    
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
    
    let bmp = new Bitmap( xResolution, yResolution )
    !pixelColors |> List.iter ( fun pl -> pl |> List.iter ( fun p -> let (u, v, color) = p 
                                                                     bmp.SetPixel(u, v, color) ) )
    bmp.Save("test2.bmp" )

    let endTime = System.DateTime.Now
    let duration = (endTime - startTime).TotalSeconds
    printfn "Parallel Duration: %f" duration

    
    0 // return an integer exit code