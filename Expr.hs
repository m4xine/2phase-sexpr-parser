module Expr 
  ( ExprF(..)
  , Expr
  ) where

import Control.Comonad.Cofree (Cofree)
import Text.Show.Deriving     (deriveShow1)

data ExprF a
  = Var     String
  | Invoke  a [a]
  | ListL   [a]
  deriving Functor

deriveShow1 ''ExprF

type Expr x = Cofree ExprF x