module Material
    open Vector
    open Light
    open Color
    open System
    open Ray

    
    let Phong (power: float ) (eyeDirection: Vector3) (lightDirection: Vector3 ) (normal: Vector3) =
        let h = ( eyeDirection.Normalize() + lightDirection.Normalize() ).Normalize()
        let mDotH = normal * h

        match mDotH with
        | _ when mDotH < 0. -> 0.
        | _ -> Math.Pow( mDotH, power )

    let Lambertian (eyeDirection: Vector3) (lightDirection: Vector3 ) (normal: Vector3)=
        let diffuse = normal * lightDirection
        if diffuse > 0. then 
            diffuse 
        else 
            0.

    type Material( diffuseFunction: Vector3 -> Vector3 -> Vector3 -> float, specularFunction: Vector3 -> Vector3 -> Vector3 -> float, 
                        diffuseColor: Color, specularColor: Color,
                        reflectivity: float, refractionIndex: float ) =
        member this.DiffuseColor = diffuseColor
        member this.SpecularColor = specularColor
        member this.Reflectivity = reflectivity
        member this.RefractionIndex = refractionIndex

        member this.DiffuseFunction = diffuseFunction
        member this.SpecularFunction = specularFunction

        member this.CalculateLightInteraction  (eyeDirection: Vector3) (lightDirection: Vector3) (normal: Vector3) (light: Light) =
            let diffuse = this.DiffuseFunction eyeDirection lightDirection normal
            let diffuseColor = diffuse * light.Color * this.DiffuseColor

            let specular = this.SpecularFunction eyeDirection lightDirection normal 
            let specularColor = specular * light.Color * this.SpecularColor
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


    type MaterialFactory( diffuseFunction: Vector3 -> Vector3 -> Vector3 -> float, specularFunction: Vector3 -> Vector3 -> Vector3 -> float ) =

        member this.CreateMaterial (diffuseColor: Color, specularColor: Color, reflectivity: float, refractionIndex: float) =
            new Material( diffuseFunction, specularFunction, diffuseColor, specularColor, reflectivity, refractionIndex )