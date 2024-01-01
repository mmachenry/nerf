{-# Language OverloadedStrings #-}

import Text.Blaze.Svg11
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Data.String

main = putStrLn $ renderSvg svgDoc

lineDrawing x = x ! A.stroke "black" ! A.fill "white"
move (x,y) elem = elem ! A.transform (translate x y)

mkCircle r (x,y) = lineDrawing $ S.circle
    ! A.r (fromString (show r))
    ! A.cx (fromString (show x))
    ! A.cy (fromString (show y))

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "130mm" ! A.height "117mm" 
   ! A.viewbox "0 -4 130 121" $ do
   --lineDrawing $ S.rect ! A.width "130" ! A.height "117"
   lineDrawing $ S.path ! A.d grip -- ! A.transform (rotate "90")
   mkCircle bindingPostR (handleConnectorWidth/2, barHeight/2)
   mkCircle bindingPostR (barWidth-handleConnectorWidth/2, barHeight/2)
   mkCircle (drillHoleD/2) triggerBoltPos
   mkCircle (drillHoleD/2) (bx+drillHoleD*1.5, by)
   mkCircle (drillHoleD/2) (bx+gripWidth-drillHoleD*0.2, by)
   {-
   move (70,0) $ lineDrawing $ S.path ! A.d trigger
   mkCircle bindingPostR triggerBoltPos
   -}

fromInches = (* 25.4)
fromDegrees = (* (pi/180))

bindingPostR = fromInches 0.2660 / 2 -- clearance drill hole for 1/4"
drillHoleD = fromInches 0.144

-- import from a file that defines the handle connector after a refactoring
-- but added 0.2mm for space to fit
handleConnectorWidth = fromInches(1/2) + drillHoleD * 2
handleConnectorSpace = handleConnectorWidth + 0.2 + fromInches (1/16)
sidePanelRecessHeight = 3 -- imported from handleConnector.scad

barHeight = fromInches (3/4) -- from original Rainbow tutorial
barWidth = 130
triggerBoltPos = (86,fromInches(17/64)) -- depends on trigger
barRounding = 4
thenarArc = 30
gripAngle = fromDegrees 17
gripHeight = 110
gripWidth = 40
thenarInset = 5

grip :: S.AttributeValue
grip = mkPath $ do
    m 0 0
    l 0 (barHeight - barRounding)
    qr 0 barRounding barRounding barRounding
    lr thenarInset 0
    q ax ay px py
    ar 200 200 0 False False (bx-px) (by-py)
    qr 0 barRounding barRounding barRounding
    ar 80 90 0 False False (gripWidth-barRounding) 0
    qr barRounding 0 barRounding (-barRounding)
    ar 200 200 0 False True (px-bx) (py+barRounding-by)
    qr barRounding (-barRounding) 0 (-2*barRounding)
    ar 20 20 0 False True 0 (barHeight-py+barRounding)
    l (barWidth-barRounding) barHeight
    qr barRounding 0 barRounding (-barRounding)
    l barWidth 0
    -- BEGIN fudge for new piece
    lr (-handleConnectorWidth) 0
    lr 0 (-sidePanelRecessHeight)
    l handleConnectorWidth (-sidePanelRecessHeight)
    lr 0 sidePanelRecessHeight
    -- END
    z

(ax,ay) = (barRounding + thenarInset + thenarArc, barHeight)
(px,py) = (ax - thenarArc * sin gripAngle,
           barHeight + thenarArc * cos gripAngle)
(bx,by) = (ax - (gripHeight-barHeight) * tan gripAngle, gripHeight)

trigger :: S.AttributeValue
trigger = mkPath $ do
    m 0 (fromInches (1+3/16))
    mapM_ inchLine [
        (0.0,1+3/16),
        (0.0,11/32),
        (0.28125,0.34375),
        (5/8,0.0),
        (0.9375,0.0),
        (1.125,0.34375),
        (1.125,0.71875),
        (0.875,0.71875),
        (5/8,0.875),
        (5/8,1.1875),
        (5/8,1+13/16)
        ]
    --ar 50 50 1 0 0 (-5/8) (1+3/16)
    z
    where inchLine (x,y) = l (fromInches x) (fromInches y)

