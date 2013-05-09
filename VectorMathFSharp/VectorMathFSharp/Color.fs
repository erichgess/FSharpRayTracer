﻿module Color
    open System

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
            System.Drawing.Color.FromArgb( 255, int(255. * r), int(255. * g), int(255. * b) )

        static member init (c:System.Drawing.Color) =
            Color( float(c.R)/255.0, float(c.G)/255.0, float(c.B)/255.0 )

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

        static member (+) ( c: Color, d: System.Drawing.Color ) =
            c + Color.init d

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