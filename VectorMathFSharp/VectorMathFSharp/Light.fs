module Light
    open Point
    open System.Drawing

    type Light ( position: Point3, color: Color ) =
        member this.Position = position
        member this.Color = color

    let AddColors (c1: Color) (c2: Color) =
        let byteAdder a b = 
            let sum = int(a) + int(b)
            if sum > 255 then 255 else sum

        let r = byteAdder c1.R c2.R
        let g = byteAdder c1.G c2.G
        let b = byteAdder c1.B c2.B

        Color.FromArgb(255, r, g, b )
