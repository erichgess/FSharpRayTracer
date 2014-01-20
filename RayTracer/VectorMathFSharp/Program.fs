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
open BRDF
open Scene
open System.Threading
open System.Threading.Tasks
open System.Timers
open RayTracer


let xResolution = 512
let yResolution = 512
let colors = Color.ByName
let black = colors.["Black"]

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

let CreateRingOfSpheres numberOfSpheres =
    let angleBetweenSpheres = 2. * System.Math.PI / float(numberOfSpheres)
    let angleBetweenSpheresInDegrees = 180. / System.Math.PI * angleBetweenSpheres
    let distanceFromCenter = 1. / System.Math.Sin ( angleBetweenSpheres / 2.0 )
    let translate = Matrix.Translate( 0., distanceFromCenter, 0. )

    let cookTorranceMaterial = new MaterialFactory( Lambertian, CookTorrance 0.1 2.1 )
    List.init numberOfSpheres ( fun i -> new Sphere( Matrix.Scale( 0.3, 0.3, 0.3 ) * Matrix.RotateX( 120.0 ) * Matrix.RotateZ(angleBetweenSpheresInDegrees * float(i)) * translate, cookTorranceMaterial.CreateMaterial( colors.["Red"], 
                                                      colors.["CornflowerBlue"], 0.3, 1.76 ) ) :> IShape )

[<EntryPoint>]
let main argv = 
    
    let l = new Light(Point3( -4., 8., -3. ), colors.["White"] )
    let l2 = new Light(Point3( 1., 2., -7. ), colors.["Aquamarine"] )
    let lightSet = [ l; l2 ]

    let cookTorranceMaterial = new MaterialFactory( Lambertian, CookTorrance 0.1 2.1 )
    let phong20Material = new MaterialFactory( Lambertian, Phong 20. )
    let phong150Material = new MaterialFactory( Lambertian, Phong 150.0 )
    let phong400Material = new MaterialFactory( Lambertian, Phong 400.0 )
    let phong600Material = new MaterialFactory( Lambertian, Phong 600.0 )

    let shapes = [   new Sphere( Matrix.Scale( 1., 1., 1. ) * Matrix.Translate( 0., 0.0, 0.0 ), 
                                cookTorranceMaterial.CreateMaterial( 0.8 * colors.["CornflowerBlue"], 
                                 colors.["Red"], 0.2, 1.01 )) :> IShape;

                    new Plane( Matrix.Translate( 0., -1., 0.) * Matrix.Scale( 50., 50., 50. ), 
                               phong400Material.CreateMaterial( colors.["Green"], 
                                colors.["Green"], 0.2, 0. ) ) :> IShape;

                    new Plane(  Matrix.RotateY(45.0) * Matrix.Translate( 0., 0., 5.) * Matrix.Scale( 2., 2., 2. ) * Matrix.RotateX( -90.0 ), 
                                phong600Material.CreateMaterial( colors.["Blue"], 
                                 colors.["Blue"], 1., 0.) ) :> IShape 
                ]
    let shapes = List.append shapes (CreateRingOfSpheres 15)
    let scene = new Scene( lightSet, shapes )
   
    let ColorPixel u v =
        let ray = GetCameraRay u v
        CalculateTotalIlluminationTail (fun x -> x) (BuildLightRayTree scene 5 ray)

    
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

    let bmp = new System.Drawing.Bitmap( xResolution, yResolution )
    !pixelColors |> List.iter ( fun pl -> pl |> List.iter ( fun p -> let (u, v, color) = p 
                                                                     bmp.SetPixel(u, v, color.GetSystemColor() ) ) )

    System.Console.Write "Save File Name: "
    let fileName = System.Console.ReadLine ()
    bmp.Save( fileName )
    let endTime = System.DateTime.Now
    let duration = (endTime - startTime).TotalSeconds
    printfn "Parallel Duration: %f" duration

    
    0 // return an integer exit code