open Matrix
open Ray
open Point
open Vector
open Sphere
open System.Drawing

let GetCameraRay (u: int) (v: int ) =
    let center = Vector3( 0., 0., -10. )
    let xmin = -10
    let xmax = 10
    let ymin = -10
    let ymax = 10
    let xResolution = 256
    let yResolution = 256

    let xDelta = float( xmax - xmin ) / float(xResolution )
    let yDelta = float( ymax - ymin ) / float( yResolution )
    let xPos = float(xmin) + float(u) * xDelta
    let yPos = float(ymin) + float(v) * yDelta
    let viewPoint = Vector3( xPos, yPos, 0. )
    Ray( Point3( 0., 0., -3. ), viewPoint - center)

[<EntryPoint>]
let main argv = 
    let t = Matrix.Translate( 0., 0., 5. ) * Matrix.RotateY( 40. ) * Matrix.Scale( 4., 1., 1. )
    let sp = new Sphere( t )
    let r = new Ray( Point3(0., 0., 0.), Vector3(0., 1.,  0. ) )
    let hit = sp.Intersection r
    printfn "%b" hit

    let bmp = new Bitmap( 256, 256 )
    for y= 0 to 255 do
        for x = 0 to 255 do
            let ray = GetCameraRay x y
            if sp.Intersection ray then
                bmp.SetPixel( x, y, Color.Red )
            else
                bmp.SetPixel( x, y, Color.Black )
    bmp.Save("test.bmp" )
    0 // return an integer exit code