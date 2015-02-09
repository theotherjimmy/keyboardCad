#!/usr/bin/ghci
import Graphics.Implicit.Primitives
import Graphics.Implicit.Export
import Control.Applicative
import Data.Monoid
import System.Directory
import Options

  {-- begin argument parsing --}

data MainOptions = MainOptions {
  optHandAngle     :: Float,
  optThumbAngle    :: Float,
  optHandSep       :: Float,
  optKeySep        :: Float,
  optFingerSep     :: Float,
  optBorderWidth   :: Float,
  optBoltHoleRad   :: Float,
  optPointerHeight :: Float,
  optRingHeight    :: Float,
  optPinkyHeight   :: Float,
  optSVG           :: Bool,
  optPNG           :: Bool,
  optDir           :: String
  } 
                   
showLine thing = show thing ++ "\n"

instance Show MainOptions where
  show opts = "Options:\n" ++
              "    hand-angle " ++      showLine (optHandAngle opts) ++ 
              "    thumb-angle " ++     showLine (optThumbAngle opts) ++
              "    hand-sep "  ++       showLine (optHandSep opts) ++ 
              "    key-sep "  ++        showLine (optKeySep opts) ++ 
              "    finger-sep "  ++     showLine (optFingerSep opts) ++ 
              "    border-width "  ++   showLine (optBorderWidth opts) ++ 
              "    pointer-height "  ++ showLine (optPointerHeight opts) ++ 
              "    ring-height "  ++    showLine (optRingHeight opts) ++   
              "    pinky-height "  ++   showLine (optPinkyHeight opts) ++ 
              "    svg "  ++            showLine (optSVG opts) ++ 
              "    png " ++             showLine (optPNG opts)

instance Options MainOptions where
  defineOptions = pure MainOptions
                  <*> simpleOption "hand-angle" (pi/6) "Angle between X axis and finger keys"
                  <*> simpleOption "thumb-angle" (pi/6) "Angle between thunb keys and finger keys"
                  <*> simpleOption "hand-sep" (60) "Distance between hands in mm"
                  <*> simpleOption "key-sep" (6) "Distance between keys in a matrix in mm"
                  <*> simpleOption "finger-sep" (11) "Distance between columns associated with fingers in mm"
                  <*> simpleOption "border-width" (10) "Border arround the key-matricies in mm"
                  <*> simpleOption "bolt-hole-radius" (1) "Border arround the key-matricies in mm"
                  <*> simpleOption "pointer-height" (-15) "Height of pointer finger relative to middle finger in mm"
                  <*> simpleOption "ring-height" (-5) "Height of ring finger relative to middle finger in mm"
                  <*> simpleOption "pinky-height" (-30) "Height of pinky finger relative to middle finger in mm"
                  <*> simpleOption "svg" (False) "An svg file dump of the current design"
                  <*> simpleOption "png" (False) "An png file dump of the current design"
                  <*> simpleOption "dir" "keyboard-cad" "The location to store the output files."

  {-- end argument parsing --}

key _ = rectR 0.1 (-7,-7) (7,7)
keyOutline enterP = if enterP
                    then rectR 0.1 (-10,-20) (10,20) 
                    else rectR 0.1 (-10,-10) (10,10)

keySpace opts = optKeySep opts + 14
fingerSpace opts = optFingerSep opts + 14
minHeight opts = foldl1 min [0, optPointerHeight opts, optRingHeight opts, optPinkyHeight opts]

keyColumn key opts = union $ map (flip translate $ key False) [(0, 0 - keySpace opts), (0, 0), (0, keySpace opts)]

thumbKeys key opts = union [ translate (keySpace opts, 0 - keySpace opts / 2) $ key True,
                             translate (0, 0 - keySpace opts / 2) $ key True,
                             translate (0, keySpace opts) $ key False,
                             translate (0 - keySpace opts, 0) $ keyColumn key opts]

fingerKeys key opts = union $ map (flip translate $ keyColumn key  opts) [(0, optPointerHeight opts),
                                                                         (keySpace opts, optPointerHeight opts),
                                                                         (keySpace opts + fingerSpace opts, 0),
                                                                         (keySpace opts + 2 * fingerSpace opts, optRingHeight opts),
                                                                         (keySpace opts + 3 * fingerSpace opts, optPinkyHeight opts)]

translateThumbs opts = translate (0, -55 - 1.5 * keySpace opts) . rotate ( optThumbAngle opts )

rightFragment rad frag opts = translate (optHandSep opts, 0) $ unionR rad $ map (rotate $ optHandAngle opts) frag 
leftFragment  rad frag opts = scale (-1,1) $ unionR rad $ map (rotate $ optHandAngle opts) frag 

defrag rad opts lst = union [rightFragment rad lst opts, leftFragment rad lst opts]

backplaneFragment opts =  [ rectR 5 (0 - 0.5 * keySpace opts ,
                                     0 - 1.5 * keySpace opts + minHeight opts )
                                    (1.5 * keySpace opts + 3 * fingerSpace opts ,
                                     1.5 * keySpace opts ),
                            translateThumbs opts $ rectR 5 (0 - 1.5 * keySpace opts ,
                                                           0 - 1.5 * keySpace opts )
                                                          (1.5 * keySpace opts ,
                                                           1.5 * keySpace opts )]

boltHoleFragment opts = [ translate (1.5 * keySpace opts + 3 * fingerSpace opts ,
                                     1.5 * keySpace opts ) $ circle $ optBoltHoleRad opts ,
                          translate (1.5 * keySpace opts + 3 * fingerSpace opts,
                                     0 - 1.5 * keySpace opts + minHeight opts) $ circle $ optBoltHoleRad opts,
                          translateThumbs opts $ union [ translate (0 - 1.5 * keySpace opts,
                                                                    0 - 1.5 * keySpace opts) $ circle $ optBoltHoleRad opts,
                                                         translate (1.5 * keySpace opts,
                                                                    0 - 1.5 * keySpace opts) $ circle $ optBoltHoleRad opts ]]

keyMatrixFragment key opts =  [ fingerKeys key opts, translateThumbs opts $ thumbKeys key opts]
                              
keyMatrix key opts = defrag 0 opts $ keyMatrixFragment key opts 
backplane opts = unionR 5 [defrag 5 opts $ backplaneFragment opts,
                           rectR 0 (0 - keySpace opts, -90) (optHandSep opts  + keySpace opts, 25),
                           rectR 0 (0 - 2 * keySpace opts, -90) (optHandSep opts +  2 * keySpace opts, 0)]

boltHoles opts = defrag 0 opts $ boltHoleFragment opts

{-- make Figures and outFiles are zipped below, to match models with filenames. There sizes must match --}

makeFigures opts = [ difference [union [shell (optBorderWidth opts) $ backplane opts, backplane opts], keyMatrix key opts, boltHoles opts], 
                     difference [union [shell (optBorderWidth opts) $ backplane opts, backplane opts], keyMatrix keyOutline opts, boltHoles opts], 
                     difference [shell (optBorderWidth opts) $ backplane opts, boltHoles opts] ,
                     difference [union [shell (optBorderWidth opts) $ backplane opts, backplane opts], boltHoles opts] ]

outFiles = ["top", "dust-guard", "shell", "bottom"]

makeFiles outFunc extension opts =
  sequence_ $ map (\(x,y) -> outFunc 1 x y) $ zip (map (\x->optDir opts ++ "/" ++ x ++ extension) outFiles ) (makeFigures opts)

main = runCommand $ \opts args -> do
  print opts
  existsP <- doesDirectoryExist $ optDir opts
  if (optSVG opts || optPNG opts) && not existsP
    then createDirectory $ optDir opts
    else return ()
  if optSVG opts
    then makeFiles writeSVG ".svg" opts
    else return ()
  if optPNG opts
    then makeFiles writePNG ".png" opts
    else return ()
