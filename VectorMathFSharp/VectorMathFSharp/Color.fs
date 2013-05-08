module Color
    type BoundedMath () =
            member this.Bind(input, rest ) =
                match input with
                | input when input < 0. -> rest 0.
                | input when input > 1. -> rest 1.
                | _ -> rest input 

            member this.Return(x) =
                x

    type Color( r: float, g: float, b: float ) =
        member this.R = r
        member this.G = g
        member this.B = b

        static member (*) ( c: Color, d: Color ) =
            let calculateColor = new BoundedMath()
            calculateColor{
               let! r = c.R * d.R
               let! g = c.G * d.G
               let! b = c.B * d.B
               return Color( r, g, b )
            }

        static member (+) ( c: Color, d: Color ) =
            let calculateColor = new BoundedMath()
            calculateColor{
               let! r = c.R + d.R
               let! g = c.G + d.G
               let! b = c.B + d.B
               return Color( r, g, b )
            }

        static member (*) ( a: float, c: Color ) =
            let calculateColor = new BoundedMath()
            calculateColor{
               let! r = a * c.R
               let! g = a * c.G
               let! b = a * c.B
               return Color( r, g, b )
            }

        static member (*) ( c: Color, a: float ) =
            a * c