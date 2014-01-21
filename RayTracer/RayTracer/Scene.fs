module Scene
    open Light
    open Shape

    type Scene = { Lights: Light list; Shapes: Shape list }