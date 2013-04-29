open Matrix
open Ray
open Point
open Vector
open Sphere

let GetCameraRay (u: int) (v: int ) =
    let center = Vector3( 0., 0., -10. )
    let xmin = -10
    let xmax = 10
    let ymin = -10
    let ymax = 10
    let xResolution = 100
    let yResolution = 100

    let xDelta = float( xmax - xmin ) / float(xResolution )
    let yDelta = float( ymax - ymin ) / float( yResolution )
    let xPos = float(xmin) + float(u) * xDelta
    let yPos = float(ymin) + float(v) * yDelta
    let viewPoint = Vector3( xPos, yPos, 0. )
    Ray( Point3( 0., 0., -10. ), viewPoint - center)

[<EntryPoint>]
let main argv = 
    let t = Matrix.Translate( 0., 0., -15. );
    let sp = new Sphere( t )
    let r = new Ray( Point3(0., 0., 0.), Vector3(0., 1.,  0. ) );
    let hit = sp.Intersection r
    printfn "%b" hit

    let getray = GetCameraRay 0
    for y = 0 to 100 do
        let ray = getray y
        let hit = sp.Intersection ray
        printf "%b" hit
    0 // return an integer exit code