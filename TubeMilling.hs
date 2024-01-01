import Control.Monad

-- Units in inches

data Instruction = Instruction String Float
instance Show Instruction where
    show (Instruction str n) = str ++ ": " ++ show n

(~>) :: String -> Float -> Instruction
(~>) s d = Instruction s d
infix 5 ~>

spring = 5.5
draw = 3.5

returnSpringCompressed = 4 -- FIXME not calculated, this number is a guess
frontCapWidth = 3/8
backCapWidth = 0.25
plungerHeadWidth = 5/8
catchSupportWidth = 0.75
catchSupport = backCapWidth + draw + catchSupportWidth
trigger = catchSupport - 0.25
backStopWidth = 0.25
backStop = catchSupport + returnSpringCompressed + draw + backStopWidth

instructions = [
  "Back cap drill hole #6" ~> backCapWidth / 2,
  "Catch support #6" ~> trigger + 1/4,
  "Trigger #6" ~> trigger,
  "Screw+handle" ~> trigger + 1.5,
  "Screw+handle" ~> trigger + 2.5,
  "Screw+handle" ~> trigger - 0.75,
  "Screw+handle" ~> trigger - 1.75,
  "Mill start" ~> backStop - backCapWidth,
  "Mill end" ~> backStop - backCapWidth - draw,
  "Back stop #6" ~> backStop - backStopWidth / 2,
  "Front cap" ~> backStop + spring + plungerHeadWidth + frontCapWidth / 2,
  "Cut" ~> backStop + spring + plungerHeadWidth + frontCapWidth
  ]

main = mapM_ (putStrLn . show) instructions

