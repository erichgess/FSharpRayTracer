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
    let SomeColor (color: Color) (shading: float) =
        let shade x = int( shading * float(x)) 
        Color.FromArgb(255, shade color.R, shade color.G, shade color.B )

    let light = new Light(Point3( 0., 7., -5. ), Color.SeaGreen )
    let light2 = new Light(Point3( 5., 7., -5. ), Color.SeaGreen )
    let lightSet = [ light; light2 ]
    let scene = [  new Sphere( Matrix.Translate(0., 1., 0. ), Color.Blue ) :> IShape; 
                    new Plane( Matrix.Translate( 0., -2.0, 0. ), Color.Red) :> IShape;
                    new Sphere( Matrix.Translate( 2., 0., 0.), Color.Green) :> IShape ]

    let CastRay ray = 
        let intersections = scene |> List.map( fun s -> (s.Intersection ray) )
        intersections |> List.reduce ( fun acc intersection-> 
            match acc with
            | None -> intersection
            | Some(time, normal, color) ->
                match intersection with
                | Some(intersectionTime, intersectionNormal, intersectionColor) when intersectionTime < time 
                    -> intersection
                | _ -> acc
            )

    let CalculateShading (light: Light) (ray:Ray) (nearestShape:(float*Vector3*Color) option ) =
        match nearestShape with
        | None -> Color.Black
        | Some(time, n, color) -> 
            let p = ray.Origin + ray.Direction * time
            let surfaceToLight = ( light.Position - p ).Normalize()

            let diffuse = n.Normalize() * surfaceToLight
            let diffuse = if diffuse < 0. then 0. else diffuse

            let surfaceToLightRay = new Ray( p, surfaceToLight )
            match (CastRay surfaceToLightRay) with
            | Some(time, normal, color) when time >= 0.001 -> Color.Black   // This small value is to prevent self intersection with the surface near the origin
            | _ -> SomeColor color diffuse
    
    let startTime = System.DateTime.Now

    let bmp = new Bitmap( xResolution, yResolution )
    for y= 0 to yResolution-1 do
        for x = 0 to xResolution-1 do 
            let ray = GetCameraRay x y
            let intersection = CastRay ray
            let s1 = CalculateShading light ray intersection
            let s2 = CalculateShading light2 ray intersection
            let shade = lightSet |> List.map ( fun l -> CalculateShading l ray intersection ) |> List.reduce ( fun acc l -> AddColors acc l )
            bmp.SetPixel( x, y, (AddColors s1 s2) )

    let endTime = System.DateTime.Now
    let duration = (endTime - startTime).TotalSeconds
    printfn "Duration: %f" duration
    bmp.Save("test.bmp" )
    0 // return an integer exit code