module Pvc (
    Pipe(..),
    outsideDiameter,
    wallThickness,
    insideDiameter,
    slideInJoint,
    passThroughJoint,
    pressFitJoint,
    slideOver
) where

-- http://en.wikipedia.org/wiki/Nominal_Pipe_Size
-- www.glynwedasia.com/.../Plastic%20Pipes%20Dimension%20(2).pdf

type Schedule = Int
type NominalSize = Float
data Pipe = Pvc Schedule NominalSize | Cpvc Schedule NominalSize

-- The outside diameter of the pipe.
outsideDiameter (Pvc _ 0.5) = 21.30
outsideDiameter (Pvc _ 0.75) = 26.67
outsideDiameter (Pvc _ 1.0) = 33.40
outsideDiameter (Pvc _ 1.25) = 42.12
outsideDiameter (Cpvc 40 0.5) = 15.875 -- 5/8" by rumor on nerf haven

-- The wall thickness of the pipe.
wallThickness (Pvc 40 0.5) = 2.76
wallThickness (Pvc 40 0.75) = 2.87
wallThickness (Pvc 40 1.0) = 3.38
wallThickness (Pvc 40 1.25) = 3.8

wallThickness (Pvc 80 0.5) = 3.74
wallThickness (Pvc 80 0.75) = 3.92
wallThickness (Pvc 80 1.0) = 4.55

-- The inside diameter of the pipe.
insideDiameter pipe = outsideDiameter pipe - 2 * wallThickness pipe

-- The outer diameter of a cylinder that will slide into a PVC joint and easily
-- slide out with gravity.
slideInJoint (Pvc 40 1.0) = 33.2 -- tested with maker bot
slideInJoint pipe = outsideDiameter pipe - 0.2

-- The outer diameter of a cylinder that will pass through the inner lip
-- of a PVC joint.
passThroughJoint pipe = outsideDiameter pipe - 0.5

-- The inside diameter of a joint ment to press fit around the pipe.
pressFitJoint pipe = outsideDiameter pipe + 0.15

-- The inside diameter of a cylinder that the pipe will slide freely within.
slideOver pipe = outsideDiameter pipe + 0.2

pressIn d = d - 0.2
pressOver d = d + 0.2
slideIn d = d + 0.4
--slideOver d = d + 0.4
fittingID pipe = pressOver (outsideDiameter pipe)

