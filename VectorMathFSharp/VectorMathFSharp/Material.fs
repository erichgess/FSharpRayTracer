module Material
    open System.Drawing

    type Material( color: Color, reflectivity: float, refractionIndex: float ) =
        member this.Color = color
        member this.Reflectivity = reflectivity
        member this.RefractionIndex = refractionIndex