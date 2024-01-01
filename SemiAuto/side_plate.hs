import SVGDraw

fromInches = (*25.4)

-- from magwell
boltHoleD = fromInches 0.1495
standOff = boltHoleD * 1.5
clipLength = 80.88
frontWallWidth = 15
backWallWidth = 15
partWidth = clipLength + frontWallWidth + backWallWidth
partHeight =  fromInches 2

-- other measurments
cylinderPlateX = partWidth + strokeLength - backWallWidth + cylinderPlateWidth/2
totalWidth = cylinderPlateX + standOff
totalHeight = partHeight
strokeLength = fromInches 4
cylinderPlateWidth = fromInches 0.25

main = putStrLn $ renderSvg $ svgDoc (totalWidth,totalHeight) $ do
    rectangle (0,0) (totalWidth, totalHeight)
    boltHole (left, top)
    boltHole (right, top)
    boltHole (left,bottom)
    boltHole (right,bottom)
    boltHole (cylinderPlateX, top)
    boltHole (cylinderPlateX, bottom)
    where top = standOff
          bottom = partHeight - standOff
          left = frontWallWidth / 2
          right = frontWallWidth + clipLength + backWallWidth/2
          boltHole = circle (boltHoleD/2)
          
