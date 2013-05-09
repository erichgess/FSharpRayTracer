module Material
    open Vector
    open Light
    open Color
    open System
    open Ray

    
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
            let normal = normal
            let diffuse = normal * lightDirection
            let diffuse = if diffuse < 0. then 0. else diffuse
            let specular = Phong eyeDirection lightDirection normal 200.

            let surfaceLightColor = light.Color * this.Color
            let specularColor = specular * surfaceLightColor
            let diffuseColor = diffuse * surfaceLightColor
            specularColor + diffuseColor

        member this.ReflectRay (time: float, ray: Ray, normal: Vector3 ) =
            let reflectedDirection = -ray.Direction.ReflectAbout normal
            new Ray( time * ray + reflectedDirection * 0.0001, reflectedDirection )

        member this.RefractRay ( time: float, ray: Ray, normal: Vector3, isEntering: bool ) =
            let eyeDir = ray.Direction.Normalize()

            let ( firstMediumIndex, secondMediumIndex) = if isEntering then (1.0, this.RefractionIndex) else ( this.RefractionIndex, 1.0 )

            match eyeDir.RefractThrough( normal, firstMediumIndex, secondMediumIndex ) with
            | None -> None
            | Some(refractedDirection)-> Some(new Ray( time * ray + refractedDirection * 0.0001, refractedDirection ))