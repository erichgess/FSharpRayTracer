module CookTorrance
    open Vector
    open System

    let CookTorranceDistribution (angle: float) (roughness: float ) =
        let a = Math.Tan angle / roughness
        let cs = Math.Cos angle
        let b = 4. * roughness * roughness * cs * cs * cs * cs

        ( Math.Exp (- a * a ) )/ b

    let CookTorranceFesnel (nDotL: float) ( refractionIndex: float ) =
        let c = nDotL * nDotL
        let g_sqr = refractionIndex * refractionIndex + nDotL * nDotL - 1.
        let g = Math.Sqrt g_sqr

        let g_min_c = g - c
        let g_plus_c = g + c

        let a = (g_min_c * g_min_c)/( g_plus_c * g_plus_c )/2.
        let b1 = c * g_plus_c - 1.
        let b2 = c * g_min_c + 1.
        let b = (b1*b1)/(b2*b2)

        a * ( 1. + b )

    let CookTorrance (roughness: float ) (refractionIndex: float) (eyeDirection: Vector3) (lightDirection: Vector3 ) (normal: Vector3) =
        let h = ( eyeDirection + lightDirection ).Normalize()
        let nDotE = normal * eyeDirection
        let nDotH = normal * h
        let nDotL = normal * lightDirection
        let hDotL = h * lightDirection

        let gm = 2. * nDotH * nDotL / hDotL
        let gs = 2. * nDotH * nDotE / hDotL

        let G = match gm, gs with
                | gm,gs when gm < 1. && gm < gs -> gm
                | gm,gs when gs < 1. && gs < gm -> gs
                | _ -> 1.

        let F = CookTorranceFesnel nDotL refractionIndex

        let delta = Math.Acos nDotH
        let D = CookTorranceDistribution delta roughness

        (F * D * G ) / nDotE