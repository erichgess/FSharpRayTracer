module Matrix
    open System

    let sum i0 i1 f =
        let mutable t = 0.0
        for i in i0..i1 do
            t <- t + f i
        t

    let Sign (i: int ) =
        if i%2 = 0 then
            1.0
        else
            -1.0

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

        static member RotateX (angle: float) =
            let angle = angle / 180.0 * Math.PI
            Matrix( array2D[   [1.0; 0.0;            0.0;              0.0];
                                [0.0; Math.Cos angle; -Math.Sin angle;  0.0];
                                [0.0; Math.Sin angle; Math.Cos angle;   0.0];
                                [0.0; 0.0;            0.0;              1.0]] )

        static member RotateY (angle: float) =
            let angle = angle / 180.0 * Math.PI
            Matrix( array2D[   [Math.Cos angle;  0.0; Math.Sin angle;  0.0];
                                [0.0;             1.0; 0.0;             0.0];
                                [-Math.Sin angle; 0.0; Math.Cos angle;  0.0];
                                [0.0; 0.0;            0.0;              1.0]] )

        static member RotateZ (angle: float) =
            let angle = angle / 180.0 * Math.PI
            Matrix( array2D[   [Math.Cos angle; -Math.Sin angle; 0.0;  0.0];
                                [Math.Sin angle; Math.Cos angle;  0.0;  0.0];
                                [0.0;            0.0;             1.0;  0.0];
                                [0.0; 0.0;            0.0;              1.0]] )
            

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
                for column = 0 to this.Dim-1 do
                    determinate <- determinate + (Sign column ) * this.[ 0, column ] * this.SubtractRowAndColumn(0, column).Determinate()
                determinate

        member this.Invert () =
            let determinate = this.Determinate ()
            let m = Matrix.init this.Dim ( fun i j -> Sign (i + j) * this.SubtractRowAndColumn(j, i).Determinate() )
            m / determinate

        member this.Transpose () =
            Matrix.init this.Dim ( fun i j -> this.[j, i] )

        member this.Print () =
            sprintf "%A" xs