module Material
    open Vector
    open Light
    open Color
    open System
    open Ray

    type Material = {   DiffuseFunction: Vector3 -> Vector3 -> Vector3 -> float; 
                        SpecularFunction: Vector3 -> Vector3 -> Vector3 -> float;
                        DiffuseColor: Color; 
                        SpecularColor: Color;
                        Reflectivity: float; 
                        RefractionIndex: float }

    let CalculateLightIllumination material (eyeDirection: Vector3) (lightDirection: Vector3) (normal: Vector3) light =
        let diffuse = material.DiffuseFunction eyeDirection lightDirection normal
        let diffuseColor = diffuse * light.Color * material.DiffuseColor

        let specular = material.SpecularFunction eyeDirection lightDirection normal 
        let specularColor = specular * light.Color * material.SpecularColor
        specularColor + diffuseColor

    let ReflectRay (time: float, ray: Ray, normal: Vector3 ) =
        let reflectedDirection = -ray.Direction.ReflectAbout normal
        new Ray( time * ray + reflectedDirection * 0.0001, reflectedDirection )

    let RefractRay material ( time: float, ray: Ray, normal: Vector3, isEntering: bool ) =
        let eyeDir = ray.Direction.Normalize()

        let ( firstMediumIndex, secondMediumIndex) = if isEntering then 
                                                        (1.0, material.RefractionIndex) 
                                                        else 
                                                        ( material.RefractionIndex, 1.0 )

        match eyeDir.RefractThrough( normal, firstMediumIndex, secondMediumIndex ) with
        | None -> None
        | Some(refractedDirection)-> Some(new Ray( time * ray + refractedDirection * 0.0001, refractedDirection ))