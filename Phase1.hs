module Phase1 
  ( parseString
  ) where

import Control.Applicative        ((<|>), Alternative(..))
import Data.Void                  (Void)
import Data.Composition           ((.:))
import Text.Megaparsec            (Parsec, oneOf, between, getSourcePos, choice, ParseErrorBundle, parse)
import Text.Megaparsec.Char       (space, letterChar, digitChar, char)
import Text.Megaparsec.Char.Lexer (lexeme)
import Obj                        (Obj(..), XObj(..))

type Phase1 = Parsec Void String

l :: Phase1 a -> Phase1 a
l = lexeme space

sym :: Phase1 Obj
sym =
    (Sym .: (:)) <$> head' <*> many tail'
  where
    head' = letterChar <|> char '_'
    tail' = head' <|> digitChar <|> oneOf ['\'', '-']

lst :: Phase1 Obj
lst = Lst <$> between (l $ char '(') (char ')') (many xobj)

xobj :: Phase1 XObj
xobj = MkXObj <$> getSourcePos <*> l (choice [sym, lst])

parseString
  :: FilePath 
  -> String 
  -> Either (ParseErrorBundle String Void) [XObj]
parseString = parse $ many xobj