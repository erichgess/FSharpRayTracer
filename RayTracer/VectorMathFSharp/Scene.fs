module Scene
    open Light
    open Shape

    type Scene( lights: Light list, shapes: IShape list )=
        member this.Lights = lights
        member this.Shapes = shapes