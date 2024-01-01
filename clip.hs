import Graphics.Implicit

main = do
    writeSTL 0 "clip/clip.stl" clip

clipX = 30
clipY = 5
holderX = clipX + 5
holderY = clipY + 5 
holeX = clipX + 1
holeY = clipY + 1
holeZ = holderZ - 2
holderZ = 10

clip = unionR 0 [
    rect3R 0 (0,0,0) (holderX, holderY, holderZ),
    cylinder 1 1
    ]
{-
difference [
    --translate (0,0,0) $ rect3R 0 (0,0,0) (holeX, holeY, holeZ)
    ]

-}
