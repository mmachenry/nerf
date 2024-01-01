module RingCatchTrigger where

import Graphics.Implicit
import Pvc

pistonSupport housingPipe pistonPipe =
    let skinnyLength = 18
        wideLength = 10
    in difference [
           union [
               cylinder (passThroughJoint housingPipe / 2) wideLength,
               translate (0,0,wideLength)
                   $ cylinder (slideInJoint housingPipe / 2) skinnyLength
               ],
           cylinder (12.5/2) -- (slideOver pistonPipe / 2)
                    -- above should using slideOver but I don't have CPVC
                    -- sizes yet. Testing with 12.5 for 1/2" CPVC
                    (wideLength+skinnyLength)
           ]

main = writeSTL 1 "test.stl" (pistonSupport (Pvc 40 1.0) (Pvc 40 0.5))

