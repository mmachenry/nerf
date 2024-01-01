import Graphics.Implicit
import Graphics.Implicit.Primitives
import System.Directory
import Pvc

main = do
    createDirectoryIfMissing False "rainbow"
    let res = 0.5 
    --writeSTL res "rainbow/backCap.stl" backCap
    --writeSTL res "rainbow/frontCap.stl" frontCap
    --writeSTL res "rainbow/catchSupport.stl" catchSupport
    -- writeSTL res "rainbow/catchSupportNegative.stl" catchSupportNegative
    --writeSTL res "rainbow/catch.stl" catch
    --writeSTL res "rainbow/backCap.stl" backCap
    --writeSTL res "rainbow/trigger.stl" trigger3D
    --writeSTL res "rainbow/plungerHead.stl" plungerHead
    --writeSTL res "rainbow/barrelStop.stl" barrelStop
    --writeSTL res "rainbow/barrelSupport.stl" barrelSupport
    --writeSVG 1 "rainbow/manycatches.svg" manyCatches
    --writeSVG 1 "rainbow/manycatcheslaser.svg" manyCatchesLaser
    --writeSVG 1 "rainbow/handle.svg" handle
    --writeSVG 1 "rainbow/laserwood.svg" laserWood
    --writeSVG 1 "rainbow/trigger.svg" trigger
    writeSVG 1 "rainbow/support2d.svg" support2D

----------------------------------------------------------------------
-- Global
----------------------------------------------------------------------
fromInches = (*25.4)
epsilon = 0.01
rectangle w h (x,y) = rectR 0 (x-w/2, y-h/2) (x+w/2, y+h/2)

----------------------------------------------------------------------
-- Measurements of non-printed hardware
----------------------------------------------------------------------
plungerTube = Pvc 40 1.25
barrel = Cpvc 40 0.5
plungerRod = fromInches $ 3/8
boltHeadWidth = fromInches $ (0.235 + 0.265)/2
boltHeadDiag = fromInches $ (0.268 + 0.298)/2
boltHeadHeight = fromInches $ (0.070 + 0.100)/2
drillHoleR = fromInches $ 0.1495 / 2
rivetRadius = fromInches 0.189 / 2 -- #12 drill size
rivetLength = fromInches 0.438
rivetFlangeRadius = fromInches 0.325 / 2
rivetFlangeThickness = fromInches 0.032
pressFitGap = 0.15 -- http://www.javelin-tech.com/blog/2012/08/how-to-set-up-the-correct-tolerances-in-3d-printed-static-assemblies/

----------------------------------------------------------------------
-- Derived constants.
----------------------------------------------------------------------
plungerRodHoleWidth = plungerRod + 2
pressInR = (insideDiameter plungerTube - pressFitGap) / 2
catchZ = 10
catchX = 18.3 -- guess
catchY = pressInR*2-plungerRod/2

----------------------------------------------------------------------
-- Parts.
----------------------------------------------------------------------

boltTargetZ = boltHeadDiag+2
supportFrontZ = boltTargetZ
supportSlotX = catchX+1
supportSlotZ = catchZ+1
supportBackZ = 5
totalZ = supportFrontZ+supportSlotZ+supportBackZ


----------------------------------------------------------------------
-- Sub parts
----------------------------------------------------------------------

catchSupportBody = cylinder pressInR totalZ

plungerRodHole =
    rect3R 0 (-1*plungerRodHoleWidth/2,-1*plungerRodHoleWidth/2,-1*epsilon)
             (plungerRodHoleWidth/2, plungerRodHoleWidth/2, totalZ+epsilon)

catchSlot =
    rect3R 0 (-1*supportSlotX/2, -1*pressInR, supportBackZ)
             (supportSlotX/2, pressInR, supportBackZ+supportSlotZ)

----------------------------------------------------------------------
-- Helper parts
----------------------------------------------------------------------

nutSlots bottomZ topZ = union $ map (nutSlot bottomZ topZ) [0..3]

nutSlot bottomZ topZ n =
    rotate3 (0,0,n*pi/2) $
    translate (pressInR-nutWall*2-boltHeadHeight+1,
               -1*(boltHeadDiag+1)/2,
               bottomZ) $
    rect3R 0 (0,0,0) (boltHeadHeight+1, boltHeadDiag+1, topZ)
    where nutWall = 2

boltHoles h = union $ map (drillHole h) [0..3]
          --take off two to avoid going into the plunger rod hole
drillHole h n =
    rotate3 (0,0,n*pi/2) $
    translate (0, pressInR-drillHoleLength, h - boltTargetZ/2) $
    rotate3 (-1*pi/2,0,0) $
        cylinder drillHoleR drillHoleLength
    where drillHoleLength = pressInR-plungerRodHoleWidth/2-2

rivetHole h n =
    rotate3 (0,0,n*pi/2) $
    translate (0, pressInR-rivetHoleLength, h) $
    rotate3 (-1*pi/2,0,0) $
        union [
            cylinder (rivetRadius+pressFitGap) rivetLength,
            translate (0,0,rivetLength) $
                cylinder (rivetFlangeRadius+pressFitGap) rivetFlangeThickness
            ]
    where rivetHoleLength = rivetLength + rivetFlangeThickness

-- For McMaster-Carr part 92394A113.
threadedInsertHole h n =
    rotate3 (0,0,n*pi/2) $
    translate (0, pressInR - holeLength, h) $
    rotate3 (-1*pi/2,0,0) $
        cylinder holeRadius rivetLength
    where holeLength = fromInches (1/4)
          holeRadius = fromInches (3/16) / 2

----------------------------------------------------------------------
-- Parts.
----------------------------------------------------------------------

catchSupport = difference [
    catchSupportBody,
    plungerRodHole,
    catchSlot,
    threadedInsertHole (totalZ-(fromInches (3/16))) 0,
    threadedInsertHole (totalZ-(fromInches (3/16))) 1,
    threadedInsertHole (totalZ-(fromInches (3/16))) 2,
    threadedInsertHole (totalZ-(fromInches (3/16))) 3
    ]

catchSupportNegative = intersect [
    catchSupportBody,
    union [
        difference [catchSlot, plungerRodHole],
        difference [boltHoles totalZ,
                    nutSlots (totalZ-supportFrontZ) supportFrontZ]
        ]
    ]

catch = intersect [
    difference [
        rect3R 0 (-1*catchX/2,-1*pressInR,0) (catchX/2,pressInR,catchZ),
        plungerRodHole,
        translate (0,plungerRod/2,-1*epsilon) $ nutSlot 0 (catchZ+2*epsilon) 3,
        translate (0,plungerRod/2,0) $ drillHole catchZ 2
        ],
    catchSupportBody,
    translate (0, plungerRod/2,0) catchSupportBody
    ]
      
backCap =
    union [
        translate (0,0,insertedLength) $
            cylinder (outsideDiameter plungerTube / 2) (fromInches 0.25),
        difference [
            cylinder pressInR insertedLength,
            translate (pressInR,0,insertedLength/2) $
                rotate3 (0,-1*pi/2,0) $
                    cylinder drillHoleR (pressInR*2)
        ]
    ]
    where insertedLength = fromInches 0.25

-- Add a lip to catch the outside of the plunger tube.
barrelSupport = difference [
    cylinder pressInR boltTargetZ,
    translate (0,0,-1*epsilon) $
        cylinder ((outsideDiameter barrel)/2) (10+epsilon*2),
    boltHoles boltTargetZ,
    nutSlots 0 boltTargetZ
    ]

barrelStop =
    difference [
        union [
            cylinder (outsideDiameter plungerTube / 2) lipWidth,
            translate (0,0,lipWidth) $ cylinder pressInR insertedLength
        ],
        cylinder ((barrelOD/2)+1) (lipWidth+insertedLength-backStop),
        cylinder (barrelID/2) (lipWidth+insertedLength),
        rivetHole (lipWidth+insertedLength/2) 0,
        rivetHole (lipWidth+insertedLength/2) 2
    ]
    where insertedLength = fromInches (3/8)
          lipWidth = fromInches 0.25
          barrelOD = fromInches 0.625
          barrelID = fromInches 0.485
          backStop = fromInches (1/8)

----------------------------------------------------------------------
-- 2d helpers
----------------------------------------------------------------------
boltCircle :: Integer -> Float -> (Float,Float) -> Float -> Float -> SymbolicObj2
boltCircle numHoles bitRadius (x,y) radius initTheta =
    translate (x,y)
    $ union
    $ map (\n->rotate ((fromInteger n)*theta) oneBoltHole) [0..numHoles]
    where theta = 2*pi/(fromInteger numHoles)
          oneBoltHole =
              rotate initTheta
              $ translate (radius,0)
              $ circle bitRadius

----------------------------------------------------------------------
-- 2D for milling and laser cutting
----------------------------------------------------------------------
catch2D = intersect [
    difference [rectangle catchX (pressInR*2) (0,0), plungerRodHole2D],
    circle pressInR,
    translate (0, plungerRod/2) $ circle pressInR
    ]

plungerRodHole2D = rectangle plungerRodHoleWidth plungerRodHoleWidth (0,0)

support2D = difference [
    circle pressInR,
    plungerRodHole2D,
    boltCircle 2 drillHoleR (0,0) (pressInR-drillHoleR*2) 0
    ]

boltHole = translate (pressInR - drillHoleR*2, 0) $ circle drillHoleR

sideBar = difference [
    rectR 0 (0,0) (sideBarX, sideBarY),
    union $ map (\offset-> translate (offset, sideBarY/2)
                  $ circle (fromInches (5/32) /2))
            [fromInches (5/8),
             fromInches 1,
             sideBarX/2,
             sideBarX-fromInches 1,
             sideBarX-fromInches (5/8)
             ],
    difference [
        rectR 0 (0,0) (sideBarY/2,sideBarY),
        translate (sideBarY/2,sideBarY/2) $ circle (sideBarY/2)
        ]
    ]

sideBarX = fromInches $ 4+7/8
sideBarY = fromInches $ 3/4

manyCatches = union [
    {-
    translate (fromInches 6,0) (circle 1),
    translate (0,fromInches 6) (circle 1),
    translate (fromInches 6,fromInches 6) (circle 1),
    -}

    translate (pressInR, fromInches 6 - pressInR) support2D,
    translate (pressInR*3+bitWidth, fromInches 6 - pressInR) support2D,
    translate (pressInR*3+bitWidth,fromInches 6-pressInR*3-bitWidth) support2D,
    translate (pressInR, fromInches 6 - pressInR*3 - bitWidth) support2D,

    translate (pressInR*4+catchX/2+bitWidth*2,fromInches 6-pressInR) catch2D,
    translate (pressInR*4+catchX/2+bitWidth*2,fromInches 6-pressInR*3-bitWidth) catch2D,

    union $ map (\n->translate(0,n*sideBarY+n*bitWidth) sideBar) [0..2],
    translate (fromInches 6, 0) $ rotate (pi/2) sideBar
    ]
    where bitWidth = fromInches $ 1/4

manyCatchesLaser = union [
    rowOfSupports,
    translate (0,-1*(pressInR*2+bitWidth)) rowOfSupports,
    
    union $ map (\n->translate(fromInches 6 - catchX/2,
                     catchY/2 + n*(catchY+bitWidth))
                     catch2D) [0..3],

    union $ map (\n->translate(0,n*sideBarY+n*bitWidth) sideBar) [0..2]
    ]
    where 
          rowOfSupports =
              union $ map
              (\n->translate (pressInR+n*(pressInR*2+bitWidth),
                              fromInches 6 - pressInR) support2D)
              [0..2]
          bitWidth = 6 

handle = difference [
    polygon $ map (\(x,y)->(fromInches x, fromInches y)) [
        (1+3/16, 0),
        (0,3),
        (7/16,3),
        (7/16,4+7/8),
        (2,4+7/8),
        (2,3),
        (3+1/2,0)
        ],
    translate (fromInches 1, fromInches (4+1/2))
        $ circle (fromInches (7/64)/2),
    translate (fromInches (2-5/8), fromInches (4+1/2))
        $ circle (fromInches (7/64)/2)
    ]

handleSupport = difference [
    rectR 0 (0,0) (fromInches (1+9/16),fromInches (3/4)),
    translate (fromInches (5/8), fromInches (3/8))
        $ circle (fromInches (7/64)/2),
    translate (fromInches 1, fromInches (3/8))
        $ circle (fromInches (7/64)/2)
    ]

trigger3D = extrudeR 0 trigger (fromInches 0.25)

trigger = union [
    intersect [
        translate (fromInches (5/8), fromInches (5/8))
            $ circle (fromInches (5/8)),
        rectR 0 (0,0) (fromInches (5/8), fromInches (5/8))
        ],
    difference [
        polygon $ map (\(x,y)->(fromInches x, fromInches y)) [
            (0, 5/8),
            (0, 1+15/32),
            (9/32, 1+15/32),
            (5/8, 1+13/16),
            (15/16, 1+13/16),
            (1+1/8, 1+15/32),
            (1+1/8, 1+15/32-3/8),
            (7/8, 1+15/32-3/8),
            (5/8, 15/16),
            (5/8, 5/8)
            ],
        -- Hole for the 1/4" OD nylon threaded standoff for 6-32 bolts.
        translate (fromInches (5/8), fromInches (1+15/32))
            $ circle (fromInches (1/4) / 2 + 0.4)
        ]
    ]

laserWood = union [
    handle,
    translate (fromInches 2+bitWidth, fromInches (4+7/8 - 3/4)) handleSupport,
    translate (fromInches 2.5, fromInches 2) trigger
    ]
    where bitWidth = 2
