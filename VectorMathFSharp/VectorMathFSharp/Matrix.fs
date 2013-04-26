module Matrix
    open Vector
    open System

    let sum i0 i1 f =
        let mutable t = 0.0
        for i in i0..i1 do
            t <- t + f i
        t

    type Matrix4 (xs: float [,]) =
        let xs = xs

        member this.Item(i, j) = xs.[i, j]

        static member init f = Matrix4(Array2D.init 4 4 f)

        static member RotateX (angle: float) =
            Matrix4( array2D[   [1.0; 0.0;            0.0;              0.0];
                                [0.0; Math.Cos angle; -Math.Sin angle;  0.0];
                                [0.0; Math.Sin angle; Math.Cos angle;   0.0];
                                [0.0; 0.0;            0.0;              1.0]] )

        static member RotateY (angle: float) =
            Matrix4( array2D[   [Math.Cos angle;  0.0; Math.Sin angle;  0.0];
                                [0.0;             1.0; 0.0;             0.0];
                                [-Math.Sin angle; 0.0; Math.Cos angle;  0.0];
                                [0.0; 0.0;            0.0;              1.0]] )

        static member RotateZ (angle: float) =
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

        static member (*) (m: Matrix4, n: Matrix4 ) =
            Matrix4.init ( fun i j -> 
            sum 0 3 (fun k -> m.[i, k] * n.[k, j]))


    [<EntryPoint>]
    let main argv = 
        let rx90 = Matrix4.RotateX 90.0
        let ry45 = Matrix4.RotateY 45.0

        let combo = rx90 * ry45
        0 // return an integer exit code