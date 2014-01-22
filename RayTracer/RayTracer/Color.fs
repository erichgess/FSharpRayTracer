module Color
    open System

    type BoundedMath () =
            member this.Bind(input, rest ) =
                match input with
                | input when input < 0. -> rest 0.
                | input when input > 1. -> rest 1.
                | _ -> rest input 

            member this.Return(x) =
                x

    type Color =
        { R: float; G: float; B: float }

        static member ByName =
                let ctype = Drawing.Color.Black.GetType()
                let props = ctype.GetProperties()
                props |> Array.filter ( fun p -> 
                                        let value = p.GetValue(Drawing.Color.Black, null) 
                                        value.GetType() = Drawing.Color.Black.GetType())
                       |> Array.map ( fun p -> p.GetValue(Drawing.Color.Black) :?> Drawing.Color )
                       |> Array.map ( fun c -> c.Name, Color.init c )
                       |> Map.ofArray
                       
        member this.GetSystemColor() =
            let byteFromFloat f = 
                let r = int(255. * f)
                if r < 0 then 0 else if r > 255 then 255 else r

            System.Drawing.Color.FromArgb( 255,byteFromFloat this.R, byteFromFloat this.G, byteFromFloat this.B )

        static member init (c:System.Drawing.Color) =
            { R = float(c.R)/255.0; G = float(c.G)/255.0; B = float(c.B)/255.0 }

        static member (*) ( c, d ) =
            let calculateColor = new BoundedMath()
            calculateColor{
               let! r = c.R * d.R
               let! g = c.G * d.G
               let! b = c.B * d.B
               return { R = r; G = g; B = b }
            }

        static member (+) ( c, d ) =
            let calculateColor = new BoundedMath()
            calculateColor{
               let! r = c.R + d.R
               let! g = c.G + d.G
               let! b = c.B + d.B
               return { R = r; G = g; B = b }
            }

        static member (+) ( c, d: System.Drawing.Color ) =
            c + Color.init d

        static member (*) ( a: float, c ) =
            let calculateColor = new BoundedMath()
            calculateColor{
               let! r = a * c.R
               let! g = a * c.G
               let! b = a * c.B
               return { R = r; G = g; B = b }
            }

        static member (*) ( c: Color, a: float ) =
            a * c