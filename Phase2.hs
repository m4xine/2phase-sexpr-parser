module Phase2 
  ( P2Error(..)
  , parseXObjs
  ) where

import Control.Applicative      ((<|>))
import Control.Monad            (forM)
import Control.Monad.Cont       (lift)
import Control.Monad.Loops      (untilM)
import Control.Monad.Reader     (Reader, runReader, asks)
import Control.Monad.Except     (ExceptT, runExceptT, MonadError (..), liftEither)
import Control.Monad.State.Lazy (StateT, evalStateT, MonadState (..), gets)
import Text.Megaparsec          (SourcePos, choice)
import Control.Comonad.Cofree   (Cofree(..))
import Obj                      (XObj(..), Obj(..))
import Expr                     (Expr, ExprF(..))

data P2Error 
  = NoP2Error
  | MkP2Error
    { p2Pos :: SourcePos
    , p2Msg :: String 
    }
  deriving Show 

instance Semigroup P2Error where
  NoP2Error <> r = r
  l         <> _ = l 

instance Monoid P2Error where
  mempty = NoP2Error

type Phase2 = ExceptT P2Error (Reader XObj)

runPhase2 :: Phase2 a -> XObj -> Either P2Error a
runPhase2 p = runReader $ runExceptT p

p2fail :: String -> Phase2 a
p2fail msg = do
  pos <- asks xobjPos
  throwError $ MkP2Error pos msg

match :: (Obj -> Phase2 a) -> Phase2 a
match = (asks unXObj >>=)

copos 
  :: Phase2 (f (Cofree f SourcePos)) 
  -> Phase2 (Cofree f SourcePos)
copos p = do
  pos <- asks xobjPos
  (pos :<) <$> p

type Chomp = StateT [XObj] Phase2

runChomp :: Chomp a -> [XObj] -> Phase2 a 
runChomp = evalStateT

chomp :: Phase2 a -> Chomp a
chomp p = get >>= \case
  x:xs -> do
    put xs
    liftEither $ runPhase2 p x
  [] -> lift $ p2fail "Expected object"

chomps :: Phase2 a -> Chomp [a]
chomps p = untilM (chomp p) (gets null)

matchLst :: Chomp a -> Phase2 a
matchLst p = match $ \case
  Lst xs -> runChomp p xs
  _      -> p2fail "Expected list"

var :: Phase2 (Expr SourcePos)
var = copos . match $ \case
  Sym xs  -> pure $ Var xs
  _       -> p2fail "Expected variable"

invoke :: Phase2 (Expr SourcePos)
invoke = copos . matchLst $ Invoke <$> chomp expr <*> chomps expr

expr :: Phase2 (Expr SourcePos)
expr = choice [var, invoke] <|> p2fail "Expected expression"

parseXObjs :: [XObj] -> Either P2Error [Expr SourcePos]
parseXObjs xs = forM xs $ runPhase2 expr