module Matrix
    open Vector
    open System

    type Matrix4 (xs: float [,]) =
        let xs = xs

        member this.Item(i, j) = xs.[i, j]

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