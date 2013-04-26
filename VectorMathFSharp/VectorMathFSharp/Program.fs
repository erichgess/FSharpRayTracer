
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Test

    let (+) (x:float option) (y: float option) =
        match (x,y) with
        | (Some(x),Some(y)) -> Some(x+y)
        | _ -> None

    let (-) (x:float option) (y: float option) =
        match (x,y) with
        | (Some(x),Some(y)) -> Some(x-y)
        | _ -> None

    let (*) (x:float option) (y: float option) =
        match (x,y) with
        | (Some(x),Some(y)) -> Some(x*y)
        | _ -> None

    let (/) (x:float option) (y: float option) =
        match (x,y) with
        | (Some(x),Some(y)) -> Some(x/y)
        | _ -> None

    let (~-) (x:float option)=
        match x with
        | Some(x) -> Some(-x)
        | _ -> None

    let sum i0 i1 f =
        let mutable t = Some(0.0)
        for i in i0..i1 do
            t <- t + f i
        t

    type Point(x: float, y: float, z: float) =
        let x = x
        let y = y
        let z = z

        member this.Dim = 3

        member this.Item i = 
            match i with
                | 0 -> Some(x)
                | 1 -> Some(y)
                | 2 -> Some(z)
                | _ -> None

        member this.Distance( u: Point ) =
            let s = sum 0 2 (fun i -> (u.[i] - this.[i]) * (u.[i] - this.[i]) ) 
            if s = None then
                None
            else
                Some(System.Math.Sqrt s.Value)

        override this.ToString() =
            sprintf "(%f, %f, %f)" x y z

        static member init n f =
            let a = Array.init n f
            if (a.[0]+a.[1]+a.[2])=None then
                None
            else
                Some( Point( a.[0].Value, a.[1].Value, a.[2].Value ) )

        static member (~-) ( u: Point ) =
            Point.init u.Dim ( fun i -> -u.[i] )

        static member (*) ( a: float, u: Point ) =
            Point.init u.Dim ( fun i -> Some(a) * u.[i] )

        static member (/) ( u: Point, a: float ) =
            Point.init u.Dim ( fun i -> u.[i] / Some(a) )

    type Vector3(x: float, y: float, z: float) =
        let x = x
        let y = y
        let z = z

        member this.Dim = 3

        member this.Item i = 
            match i with 
            | 0 -> Some(x)
            | 1 -> Some(y)
            | 2 -> Some(z)
            | _ -> None

        member this.LengthSquared () =
            sum 0 this.Dim ( fun i -> this.[i] * this.[i])
    
        member this.Length () =
            let l = this.LengthSquared ()
            if l.IsSome then
                Some(System.Math.Sqrt l.Value)
            else
                None

        member this.Normalize () =
            let l = this.Length ()
            Vector3.init (fun i -> this.[i] / l )

        member this.Cross ( u: Vector3 ) =
            let cx = this.[1]*u.[2] - u.[1]*this.[2]
            let cy = -(this.[0]*u.[2] - u.[0]*this.[2])
            let cz = this.[0]*u.[1] - u.[0]*this.[1]

            if (cx.IsNone ||cy.IsNone ||cz.IsNone ) then
                None
            else
                Some( Point( cx.Value, cy.Value, cz.Value ) )

        member this.HasNones () =
            this.[0].IsNone || this.[1].IsNone || this.[2].IsNone

        override this.ToString() =
            sprintf "(%f, %f, %f)" x y z

        static member init f =
            let a = Array.init 3 f
            if (a.[0]+a.[1]+a.[2])=None then
                None
            else
                Some(Vector3(a.[0].Value, a.[1].Value, a.[2].Value))

        static member (~-) (u: Vector3) =
            Vector3.init (fun i -> -u.[i])

        static member (+) (u: Vector3, v: Vector3) =
            Vector3.init (fun i -> u.[i] + v.[i])

        static member (-) (u: Vector3, v: Vector3) =
            Vector3.init (fun i -> u.[i] - v.[i])

        static member (*) (u: Vector3, v: Vector3) =
            sum 0 2 (fun i -> u.[i] * v.[i])

        // Point Calculations
        static member (-) ( u: Point, v: Point ) =
            Vector3.init ( fun i -> u.[i] - v.[i] )

    type Vector4( x: float, y: float, z: float, w: float ) =
        let x = x
        let y = y
        let z = z
        let w = w

        member this.Dim = 4

        static member Init (v3: Vector3) =
            if v3.HasNones () then
                None
            else
            Some( Vector4( v3.[0].Value, v3.[1].Value, v3.[2].Value, 1.0 ) )

    type Matrix(xs: float [,]) =
      let xs = Array2D.copy xs

      member __.Rows = xs.GetLength 0

      member __.Columns = xs.GetLength 1

      member this.Item(i, j) = xs.[i, j]

      override this.ToString() = sprintf "%A" xs

      static member init m n f = Matrix(Array2D.init m n f)

    //  static member (~-) (a: Matrix) =
    //    Matrix.init a.Rows a.Columns (fun i j -> (-a.[i, j]))
    //
    //  static member (+) (a: Matrix, b: Matrix) =
    //    Matrix.init a.Rows a.Columns (fun i j -> a.[i, j] + b.[i, j])
    //
    //  static member (-) (a: Matrix, b: Matrix) =
    //    Matrix.init a.Rows a.Columns (fun i j -> a.[i, j] - b.[i, j])
    //
    //  static member (*) (a: Matrix, b: Matrix) =
    //    Matrix.init a.Rows b.Columns (fun i j ->
    //      sum 0 (b.Rows-1) (fun k -> a.[i, k] * b.[k, j]))
    //
    //[<EntryPoint>]
    //let main argv = 
    //    printfn "%A" argv
    //    0 // return an integer exit code
