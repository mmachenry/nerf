import SVGDraw

w = 130
h = 117

main = putStrLn $ renderSvg $ svgDoc (w,h) $ do
   path grip
   circle bindingPostR (handleConnectorWidth/2, barHeight/2)
   circle bindingPostR (barWidth-handleConnectorWidth/2, barHeight/2)
   circle (drillHoleD/2) triggerBoltPos
   circle (drillHoleD/2) (bx+drillHoleD*1.5, by)
   circle (drillHoleD/2) (bx+gripWidth-drillHoleD*0.2, by)

fromInches = (*25.4)

bindingPostR = fromInches 0.2660 / 2 -- clearance drill hole for 1/4"
drillHoleD = fromInches 0.144
-- import from a file that defines the handle connector after a refactoring
handleConnectorWidth = fromInches(1/2) + drillHoleD * 2

barHeight = fromInches (3/4) -- from original Rainbow tutorial
barWidth = 130
triggerBoltPos = (86,fromInches(17/64)) -- depends on trigger
barRounding = 4
thenarArc = 30
gripAngle = 17 * pi / 180 -- 17 degrees in radians
gripHeight = 110
gripWidth = 40
thenarInset = 5

grip = do
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
    z

(ax,ay) = (barRounding + thenarInset + thenarArc, barHeight)
(px,py) = (ax - thenarArc * sin gripAngle,
           barHeight + thenarArc * cos gripAngle)
(bx,by) = (ax - (gripHeight-barHeight) * tan gripAngle, gripHeight)

trigger = do
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

