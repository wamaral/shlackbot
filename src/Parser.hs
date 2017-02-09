{-|
Module      : Parser
Description : Lib's parser module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Parser where

import           Data.Text            hiding (filter, null)
import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Types

commandTokens :: [Char]
commandTokens = "!?>"

commandParser :: Parser Command
commandParser = Command <$> cmdTokenP <*> cmdP <*> argsP

cmdTokenP :: Parser Char
cmdTokenP = oneOf' commandTokens <?> "command token"

cmdP :: Parser Text
cmdP = pack <$> some wordCharP

argsP :: Parser [Text]
argsP = fmap pack <$> filter (not . null) <$> (many wordCharP) `sepBy` spaceChar

wordCharP :: Parser Char
wordCharP = alphaNumChar <|> punctuationChar <|> symbolChar
