open Matrix
open Ray
open Point
open Vector
open Sphere

[<EntryPoint>]
let main argv = 
    let t = Matrix.Translate( 0., 0., 10. );
    let sp = new Sphere( t )
    let r = new Ray( Point3(0., 0., 0.), Vector3(0., 1.,  0. ) );
    let hit = sp.Intersection r
    printfn "%b" hit
    0 // return an integer exit code