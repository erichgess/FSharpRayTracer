module Scene
    open Light
    open Shape

    type Scene( lights: Light list, shapes: Shape list )=
        member this.Lights = lights
        member this.Shapes = shapes