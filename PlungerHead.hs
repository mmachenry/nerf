import Debug.Trace
import Materials
import Graphics.Implicit

wrap x = trace (show x) x
main = writeSTL 1 "plungerHead.stl" (plungerHead (ucups!!0) (plungerRods!!0))

plungerHead :: UCup -> PlungerRod -> SymbolicObj3
plungerHead ucup rod =
    let rodHole = rod+0.2
        supportHeight = uCupHeight ucup + 0.2
        backStopD = (uCupOD ucup + wrap (uCupID ucup)) / 2
        retainerD = uCupID ucup + 2
        retainerHeight = uCupHeight ucup / 2
        inf = 200
    in difference [
          union [
              cylinder (backStopD/2) retainerHeight,
              translate (0,0,retainerHeight) $
                  cylinder ((uCupID ucup) / 2) supportHeight,
              translate (0,0,retainerHeight+supportHeight) $
                  cylinder (retainerD / 2) retainerHeight
          ],
          translate (-1 * rodHole / 2, -1 * rodHole / 2, 0) $
              rect3R 0 (0,0,0) (rodHole, rodHole, inf)
      ]

