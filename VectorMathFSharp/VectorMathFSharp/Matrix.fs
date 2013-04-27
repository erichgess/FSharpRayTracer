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

    type Matrix ( xs: float [,] ) =
        let xs = xs

        member this.Item( i, j ) = xs.[i, j]

        member this.Dim = xs.GetLength 0

        static member init n f =
            Matrix( Array2D.init n n f )

        static member (*) ( a: float, m: Matrix ) =
            Matrix.init m.Dim ( fun i j -> a * m.[i, j] )

        static member (/) ( m: Matrix, a: float ) =
            Matrix.init m.Dim ( fun i j -> m.[i, j] / a )

        static member (*) (m: Matrix, n: Matrix ) =
            Matrix.init m.Dim ( fun i j -> 
                sum 0 3 (fun k -> m.[i, k] * n.[k, j]))

        static member Scale (x: float, y: float, z: float ) =
            Matrix( array2D [  [  x; 0.0; 0.0; 0.0];
                                [0.0;   y; 0.0; 0.0];
                                [0.0; 0.0; z;   0.0];
                                [0.0; 0.0; 0.0; 1.0] ] )

        static member Translate (x: float, y: float, z: float ) =
            Matrix( array2D [  [ 1.0; 0.0; 0.0; x];
                                [0.0; 1.0; 0.0; y];
                                [0.0; 0.0; 1.0; z];
                                [0.0; 0.0; 0.0; 1.0] ] )

        member this.SubtractRowAndColumn (row: int, column: int ) =
            Matrix.init (this.Dim - 1) (
                fun i j -> 
                    let sourceRow = if i < row then i else i + 1
                    let sourceColumn = if j < column then j else j + 1
                        
                    this.[sourceRow, sourceColumn])

        member this.Determinate () =
            if this.Dim = 1 then
                this.[0,0]
            else
                let mutable determinate = 0.0
                let mutable sign = 1.0
                for column = 0 to this.Dim-1 do
                    determinate <- determinate + sign * this.[ 0, column ] * this.SubtractRowAndColumn(0, column).Determinate()
                    sign <- -1.0 * sign
                determinate

        member this.Invert () =
            let determinate = this.Determinate ()
            let m = Matrix.init this.Dim ( fun i j -> this.SubtractRowAndColumn(j, i).Determinate() )
            m / determinate

        member this.Print () =
            sprintf "%A" xs

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

        static member (*) ( v: Vector4, m: Matrix4 ) =
            Vector4.init (fun i -> v.X*m.[0,i] + v.Y*m.[1,i] + v.Z*m.[2,i] + v.W*m.[3,i])

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