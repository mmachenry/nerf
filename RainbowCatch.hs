import Graphics.Implicit
import Pvc

main = writeSTL 1 "rainbowcatchshell.stl" obj
inchesToMM = (* 25.4)

plungerTube = Pvc 40 1.25
tubeR = (insideDiameter plungerTube) / 2
rodWidth = inchesToMM (3/8)

holeWidth = rodWidth + 0.5
catchWidth = holeWidth * 2
catchSlotWidth = catchWidth + 0.5

obj = difference [
    cylinder tubeR 30,
    rect3R 0 (-1 * holeWidth/2,-1 * holeWidth/2,-1)
             (holeWidth/2,holeWidth/2,30+1),
    rect3R 0 (-1*catchSlotWidth/2, -1*tubeR, 10)
             (catchSlotWidth/2, tubeR, 20)
    ]

