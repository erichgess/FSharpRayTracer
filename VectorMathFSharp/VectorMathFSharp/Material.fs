module Material
    open Vector
    open Light
    open Color
    open System

    
    let Phong (eyeDirection: Vector3) (lightDirection: Vector3 ) (normal: Vector3) (power: float )=
        let h = ( eyeDirection.Normalize() + lightDirection.Normalize() ).Normalize()
        let mDotH = normal * h

        match mDotH with
        | _ when mDotH < 0. -> 0.
        | _ -> Math.Pow( mDotH, power )

    type Material( color: Color, reflectivity: float, refractionIndex: float ) =
        member this.Color = color
        member this.Reflectivity = reflectivity
        member this.RefractionIndex = refractionIndex

        member this.CalculateLightInteraction  (eyeDirection: Vector3) (lightDirection: Vector3) (normal: Vector3) (light: Light) =
            let normal = normal.Normalize()
            let diffuse = normal * lightDirection
            let diffuse = if diffuse < 0. then 0. else diffuse
            let specular = Phong eyeDirection lightDirection normal 200.

            let surfaceLightColor = light.Color * this.Color
            let specularColor = specular * surfaceLightColor
            let diffuseColor = diffuse * surfaceLightColor
            specularColor + diffuseColor