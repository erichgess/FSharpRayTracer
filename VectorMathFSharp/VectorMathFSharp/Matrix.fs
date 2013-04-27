module Matrix
    open Vector
    open Point
    open Ray
    open System

    let sum i0 i1 f =
        let mutable t = 0.0
        for i in i0..i1 do
            t <- t + f i
        t

    type Matrix4 (xs: float [,]) =
        let xs = xs

        member this.Item(i, j) = xs.[i, j]

        member this.Transpose () =
            Matrix4.init ( fun i j -> this.[ j, i ])

        static member init f = Matrix4(Array2D.init 4 4 f)

        static member RotateX (angle: float) =
            let angle = angle / 180.0 * Math.PI
            Matrix4( array2D[   [1.0; 0.0;            0.0;              0.0];
                                [0.0; Math.Cos angle; -Math.Sin angle;  0.0];
                                [0.0; Math.Sin angle; Math.Cos angle;   0.0];
                                [0.0; 0.0;            0.0;              1.0]] )

        static member RotateY (angle: float) =
            let angle = angle / 180.0 * Math.PI
            Matrix4( array2D[   [Math.Cos angle;  0.0; Math.Sin angle;  0.0];
                                [0.0;             1.0; 0.0;             0.0];
                                [-Math.Sin angle; 0.0; Math.Cos angle;  0.0];
                                [0.0; 0.0;            0.0;              1.0]] )

        static member RotateZ (angle: float) =
            let angle = angle / 180.0 * Math.PI
            Matrix4( array2D[   [Math.Cos angle; -Math.Sin angle; 0.0;  0.0];
                                [Math.Sin angle; Math.Cos angle;  0.0;  0.0];
                                [0.0;            0.0;             1.0;  0.0];
                                [0.0; 0.0;            0.0;              1.0]] )

        static member Scale (x: float, y: float, z: float ) =
            Matrix4( array2D [  [  x; 0.0; 0.0; 0.0];
                                [0.0;   y; 0.0; 0.0];
                                [0.0; 0.0; z;   0.0];
                                [0.0; 0.0; 0.0; 1.0] ] )

        static member (*) (m: Matrix4, v: Vector4 ) =
            Vector4.init (fun i -> v.X*m.[i,0] + v.Y*m.[i,1] + v.Z*m.[i,2] + v.W*m.[i,3])

        static member (*) (m: Matrix4, p: Point4 ) =
            Point4.init (fun i -> p.X*m.[i,0] + p.Y*m.[i,1] + p.Z*m.[i,2] + p.W*m.[i,3])

        static member (*) (m: Matrix4, r: Ray ) =
            Ray( m * r.Origin, m * r.Direction )
            
        static member (*) (m: Matrix4, n: Matrix4 ) =
            Matrix4.init ( fun i j -> 
            sum 0 3 (fun k -> m.[i, k] * n.[k, j]))

        member this.Print () =
            sprintf "%A" xs


    [<EntryPoint>]
    let main argv = 
        let testM = Matrix4.init ( fun i j -> float(i*10 + j) )
        testM.Print ()
        0 // return an integer exit code