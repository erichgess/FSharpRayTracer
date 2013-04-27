module Program
    open Matrix

    [<EntryPoint>]
    let main argv = 
        let testM = Matrix.init 4 ( fun i j -> float(i*10 + j) )
        testM.Print ()
        0 // return an integer exit code