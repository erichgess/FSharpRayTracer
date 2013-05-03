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
open System.Timers


let xResolution = 1024
let yResolution = 1024

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
    let SomeColor (color: Color) (shading: float) =
        let shade x = int( shading * float(x)) 
        Color.FromArgb(255, shade color.R, shade color.G, shade color.B )

    let light = new Light(Point3( 0., 8., -6. ), Color.Gray )
    let light2 = new Light(Point3( -7., 9., -7. ), Color.Gray )
    let lightSet = [ light; light2 ]
    let scene = [  new Sphere( Matrix.Translate(-1., 1., -1. ), Color.Blue ) :> IShape; 
                    new Plane( Matrix.Translate( 0., -2.0, 0. ), Color.Gray) :> IShape;
                    new Sphere( Matrix.Translate( 2., 0., 0.), Color.Green) :> IShape ]

    let rec CastRay depth ray = 
        let intersections = scene |> List.map( fun s -> (s.Intersection ray) )
        let hit = intersections |> List.reduce ( fun acc intersection-> 
            match acc with
            | None -> intersection
            | Some(time, normal, color) ->
                match intersection with
                | Some(intersectionTime, intersectionNormal, intersectionColor) when intersectionTime < time 
                    -> intersection
                | _ -> acc
            )
        if depth = 0 then
            hit :: []
        else if hit = None then
            []
        else
            let time, normal, _ = hit.Value
            let reflectedDirection = -ray.Direction.ReflectAbout normal
            hit :: CastRay (depth - 1) ( new Ray( time * ray + reflectedDirection * 0.0001, reflectedDirection ))

    let CalculateShading (light: Light) (ray:Ray) (nearestShape:(float*Vector3*Color) option ) =
        match nearestShape with
        | None -> Color.Black
        | Some(time, n, color) -> 
            let p = ray.Origin + ray.Direction * time
            let surfaceToLight = ( light.Position - p ).Normalize()

            // This small value is to prevent self intersection with the surface near the origin
            let surfaceToLightRay = new Ray( p + surfaceToLight * 0.0001, surfaceToLight )

            match (CastRay 0 surfaceToLightRay) with
            | Some(time, normal, color) :: tail -> Color.Black
            | _ ->
                let diffuse = n.Normalize() * surfaceToLight
                let diffuse = if diffuse < 0. then 0. else diffuse
                SomeColor (MultiplyColors color light.Color) diffuse
    
    let startTime = System.DateTime.Now

    let ColorPixel u v =
        let ray = GetCameraRay u v
        let intersection = CastRay 2 ray
        match intersection with
        | [] -> Color.Black
        | head :: tail -> intersection |> List.rev |> List.map ( fun hit -> 
                                                        lightSet |> List.map ( fun l -> CalculateShading l ray hit ) 
                                                                    |> List.reduce ( fun acc l -> AddColors acc l ) )
                                        |> List.reduce( fun acc color -> AddColors (SomeColor acc 0.5) color )

    let bmp = new Bitmap( xResolution, yResolution )
    for y= 0 to yResolution-1 do
        for x = 0 to xResolution-1 do 
            let shade = ColorPixel x y
            bmp.SetPixel( x, y, shade )

    let endTime = System.DateTime.Now
    let duration = (endTime - startTime).TotalSeconds
    printfn "Duration: %f" duration
    bmp.Save("test.bmp" )
    0 // return an integer exit code