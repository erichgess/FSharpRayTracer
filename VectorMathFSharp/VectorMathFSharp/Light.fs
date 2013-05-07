module Light
    open Point
    open Vector
    open System.Drawing

    let AddColors (c1: Color) (c2: Color) =
        let byteAdder a b = 
            let sum = int(a) + int(b)
            if sum > 255 then 255 else sum

        let r = byteAdder c1.R c2.R
        let g = byteAdder c1.G c2.G
        let b = byteAdder c1.B c2.B

        Color.FromArgb(255, r, g, b )

    let MultiplyColors (c1: Color) ( c2: Color ) =
        let byteMultiplier a b =
            let a = float(a) / 255.
            let b = float(b) / 255.
            let product = a * b
            int( product * 255. )

        let r = byteMultiplier c1.R c2.R
        let g = byteMultiplier c1.G c2.G
        let b = byteMultiplier c1.B c2.B

        Color.FromArgb( 255, r, g, b )

    let ScaleColor (a: float) (color: Color) =
        let boundedScale x = 
                let product = int( a * float(x))
                if product > 255 then 255 else if product < 0 then 0 else product
        Color.FromArgb(255, boundedScale color.R, boundedScale color.G, boundedScale color.B )

    type Light ( position: Point3, color: Color ) =
        member this.Position = position
        member this.Color = color