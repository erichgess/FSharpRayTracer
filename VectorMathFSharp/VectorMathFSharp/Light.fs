module Light
    open Point
    open Vector
    open Material
    open System.Drawing
    open System

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
        let boundedScale x = int( a * float(x)) 
        Color.FromArgb(255, boundedScale color.R, boundedScale color.G, boundedScale color.B )

    let Phong (eyeDirection: Vector3) (lightDirection: Vector3 ) (normal: Vector3) (power: float )=
        let h = ( eyeDirection.Normalize() + lightDirection.Normalize() ).Normalize()
        let mDotH = normal * h

        match mDotH with
        | _ when mDotH < 0. -> 0.
        | _ -> Math.Pow( mDotH, power )

    type Light ( position: Point3, color: Color ) =
        member this.Position = position
        member this.Color = color
        member this.CalculateSurfaceInteration (eyeDirection: Vector3) (lightDirection: Vector3) (normal: Vector3) (surfaceMaterial: Material) =
            let normal = normal.Normalize()
            let diffuse = normal * lightDirection
            let diffuse = if diffuse < 0. then 0. else diffuse
            let specular = Phong eyeDirection lightDirection normal 200.
            let specularColor = ScaleColor specular this.Color
            AddColors specularColor (ScaleColor diffuse surfaceMaterial.Color)