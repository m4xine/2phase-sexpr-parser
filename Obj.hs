module Obj 
  ( Obj(..)
  , XObj(..)
  ) where

import Text.Megaparsec (SourcePos)

data Obj
  = Sym String 
  | Lst [XObj]
  deriving Show 

data XObj = MkXObj 
  { xobjPos :: SourcePos
  , unXObj  :: Obj
  } deriving Show
