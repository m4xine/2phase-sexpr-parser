module Main where

import            Text.Megaparsec (errorBundlePretty)
import qualified  Phase1
import qualified  Phase2
import Data.Function ((&))

main :: IO ()
main = 
  Phase1.parseString "<no-path>" "(a (b c))" & \case 
    Left e -> print $ errorBundlePretty e
    Right xs -> Phase2.parseXObjs xs & \case
      Left e -> print e
      Right es -> print es 