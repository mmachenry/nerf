import SVGDraw

fromInches = (*25.4)
w = fromInches 1.5
h = fromInches 2
d = fromInches 0.3970 -- Close fit for 3/8 measured as the cylinders bolt
nutWidth = fromInches 0.321295 + 0.4 -- measured with caplipurs corner to corner

main = putStrLn $ renderSvg $ svgDoc (w,h) $ do
    rectangle (0,0) (w,h)
    --regularPolygon 6 nutWidth (w/2, h/2)
    circle (d/2) (w/2, h/2)

