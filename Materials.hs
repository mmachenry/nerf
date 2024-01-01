module Materials where 

data UCup = UCup {
    uCupOD :: Float,
    uCupID ::  Float,
    uCupHeight :: Float
    }

type PlungerRod = Float

ucups = [
    -- mcmastercarr, part 9691K62, good for 2" PVC Sched 40 plunger tubes.
    -- Not tested.
    UCup (inches 2) (inches (1+3/8)) (inches 5/16),
    -- theoringstore.com part 6226-17, good for 1-1/4" PVC Sched 40 plunger
    -- tubes. Not tested.
    UCup (inches 1+5/16) (inches 13/16) (inches 1/4)
    ]

plungerRods = [
    -- 1/2" Nylon Rod, mcmastercarr.com, good for K25 spring
    (inches 1/2),
    -- 3/8" Nylon rod, mcmastercarr.com, good for K26 spring
    (inches 3/8)
    ]

inches = (*25.4)

