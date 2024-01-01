import Graphics.Implicit
import Graphics.Implicit.Primitives

inches2mm = (*25.4)
cube = rect3R 0 (0,0,0)

main = writeSTL 1 "magwell.stl" part

partWidth = inches2mm 1.5
partHeight = inches2mm 2
partLength = 30 + 80.88 --clipLength

part :: SymbolicObj3
part = difference [
    cube (partWidth, partHeight, partLength)
    -- clip,
    , boltHoles
    -- chamberPort,
    -- ramRodChannel
    ]

clip = undefined

boltHoles :: SymbolicObj3
boltHoles = union [
    boltHole top left,
    boltHole top right,
    boltHole bottom left,
    boltHole bottom right
    ]
    where boltHole :: Float -> Float -> SymbolicObj3
          boltHole y x =
              translate (-1*partWidth/2, x, y)
              $ rotate3 (0,90,0)
              $ cylinder partWidth boltHoleDiameter
          top = partLength - standoff
          bottom = standoff
          left = partHeight/2 - standoff
          right = -1*partHeight/2 + standoff
          standoff = boltHoleDiameter * 1.6
          boltHoleDiameter = inches2mm 0.1495

chamberPort = undefined

ramRodChannel = undefined
