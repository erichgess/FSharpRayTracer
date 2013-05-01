open Matrix
open Ray
open Point
open Vector
open Sphere
open Plane
open System.Drawing


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
    let t = Matrix.Translate( 0., -2.0, 0. )* Matrix.RotateY( 0. ) * Matrix.RotateZ( 0. ) * Matrix.Scale( 5., 1., 1. )
    let sp = new Plane( t )

    let bmp = new Bitmap( xResolution, yResolution )

    let SomeColor a = 
        Color.FromArgb(255, int( a * 200. ), 0, 0 )

    let rec CastRay x y = 
        let ray = GetCameraRay x y
        match sp.Intersection ray with
        | None -> Color.Black
        | Some(p, n) -> 
            let diffuse = n.Normalize() * ( Vector3( 0. - p.X, 7. - p.Y, -5. - p.Z ) ) .Normalize()
            let diffuse = if diffuse < 0. then 0. else diffuse
            SomeColor diffuse
    
    for y= 0 to yResolution-1 do
        for x = 0 to xResolution-1 do
            bmp.SetPixel( x, y, (CastRay x y))

    bmp.Save("test.bmp" )
    0 // return an integer exit code