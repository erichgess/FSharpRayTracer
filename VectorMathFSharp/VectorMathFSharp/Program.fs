open Matrix
open Ray
open Point
open Vector
open Shape
open Sphere
open Plane
open System.Drawing
open System.Threading
open System.Timers


let xResolution = 256
let yResolution = 256

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
    let t = Matrix.Translate( 0., -2.0, 0. )
    let sp = new Plane( t )

    let bmp = new Bitmap( xResolution, yResolution )

    let SomeColor (color: Color) (shading: float) =
        let shade x = int( shading * float(x)) 
        Color.FromArgb(255, shade color.R, shade color.G, shade color.B )

    let scene = [  new Sphere( Matrix.Translate(0., 1., 0. ) ) :> IShape; new Plane( t) :> IShape]
    let CastRay x y = 
        let ray = GetCameraRay x y
        let intersections = scene |> List.map( fun s -> (s.Intersection ray) )
        let nearestShape = intersections |> List.reduce ( fun acc intersection-> 
            match acc with
            | None -> intersection
            | Some(time, normal, color) ->
                match intersection with
                | Some(intersectionTime, intersectionNormal, intersectionColor) when intersectionTime < time -> 
                    Some(intersectionTime, intersectionNormal, intersectionColor)
                | _ -> acc
            )       
    
        match nearestShape with
        | None -> Color.Black
        | Some(time, n, color) -> 
            let p = ray.Origin + ray.Direction * time
            let diffuse = n.Normalize() * ( Vector3( 0. - p.X, 7. - p.Y, -5. - p.Z ) ) .Normalize()
            let diffuse = if diffuse < 0. then 0. else diffuse
            SomeColor color diffuse
    
    let startTime = System.DateTime.Now
    for y= 0 to yResolution-1 do
        for x = 0 to xResolution-1 do 
            bmp.SetPixel( x, y, (CastRay x y))

    let endTime = System.DateTime.Now
    let duration = (endTime - startTime).TotalSeconds
    printfn "Duration: %f" duration
    bmp.Save("test.bmp" )
    0 // return an integer exit code