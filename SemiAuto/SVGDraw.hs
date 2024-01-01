{-# Language OverloadedStrings #-}

module SVGDraw (
    rectangle,
    SVGDraw.circle,
    regularPolygon,
    move,
    svgDoc,
    renderSvg,

    l,
    z,
    m,
    qr,
    ar,
    lr,
    q,
    SVGDraw.path
    ) where

import Text.Blaze.Svg11
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Data.String

lineDrawing x = x ! A.stroke "black" ! A.fill "white"
move (x,y) elem = elem ! A.transform (translate x y)

path p = lineDrawing $ S.path ! A.d (mkPath p)

rectangle (x,y) (dx, dy) = lineDrawing $ S.path ! A.d lines
    where lines = mkPath $ do
              m x y
              lr dx 0
              lr 0 dy
              lr (-dx) 0
              z

circle r (x, y) = lineDrawing $ S.path ! A.d lines
    where lines = mkPath $ do
              m (x+r) y
              ar r r 0 True False 0 0.001
              z

regularPolygon :: Int -> Float -> (Float, Float) -> S.Svg
regularPolygon sides r (x, y) = lineDrawing $ S.path ! A.d lines
    where lines = mkPath $ do
              m (fst (head points)) (snd (head points))
              mapM_ (\(x',y')-> l x' y') (tail points)
              z
          points = [
              (x + r*cos θ, y + r*sin θ )
              | θ <- [2*pi*n/s | n <- [0.0 .. s - 1.0]]]
          s = fromIntegral sides
          
-- FIXME: defualt to mm, also requires to give the w/h when I want it auto detected
svgDoc :: (Float, Float) -> S.Svg -> S.Svg
svgDoc (width, height) commands = S.docTypeSvg
    ! A.version (fromString "1.1")
    ! A.width (fromString $ (show width) ++ "mm")
    ! A.height (fromString $ (show height) ++ "mm")
    ! A.viewbox (fromString $ "0 0 " ++ show width ++ " " ++ show height)
    $ commands
