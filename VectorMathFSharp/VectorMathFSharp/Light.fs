module Light
    open Point
    open Vector
    open Color

    type Light ( position: Point3, color: Color ) =
        member this.Position = position
        member this.Color = color