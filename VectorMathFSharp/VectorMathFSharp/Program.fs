open Matrix
open Ray
open Point
open Vector
open Sphere
open System.Drawing


let xResolution = 64
let yResolution = 64

let GetCameraRay (u: int) (v: int ) =
    let center = Vector3( 0., 0., -10. )
    let xmin = -10
    let xmax = 10
    let ymin = -10
    let ymax = 10

    let xDelta = float( xmax - xmin ) / float(xResolution )
    let yDelta = float( ymax - ymin ) / float( yResolution )
    let xPos = float(xmin) + float(u) * xDelta
    let yPos = float(ymin) + float(v) * yDelta
    let viewPoint = Vector3( xPos, yPos, 0. )
    Ray( Point3( 0., 0., -10. ), (viewPoint - center).Normalize() )

[<EntryPoint>]
let main argv = 
    let t = Matrix.Translate( 0., 0., 50. )
    let sp = new Sphere( t )

    let bmp = new Bitmap( xResolution, yResolution )

    let SomeColor a = 
        Color.FromArgb(255, int( a * 200. ), 0, 0 )

    let CastRay x y = 
        let ray = GetCameraRay x y
        match sp.Intersection ray with
        | None -> bmp.SetPixel( x, y, Color.Black )
        | Some(t) -> 
            printf "%s ===" (ray.Print())
            printfn "%s" (t.Print())
            bmp.SetPixel( x, y, Color.Red )
    
    for y= 0 to yResolution-1 do
        for x = 0 to xResolution-1 do
            CastRay x y

    bmp.Save("test.bmp" )
    0 // return an integer exit code