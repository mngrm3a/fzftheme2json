module Parser
  ( module Text.Parsec,
    Option (..),
    parseOptionsSkipFirstLine,
    parseOptions,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (ParseError, Parsec)
import qualified Text.Parsec as P

type Parser = Parsec Text ()

parseOptionsSkipFirstLine :: Text -> Either ParseError [Option]
parseOptionsSkipFirstLine = P.runParser (go >> options) () "<stdin>"
  where
    go = P.manyTill P.anyChar P.endOfLine

parseOptions :: Text -> Either ParseError [Option]
parseOptions = P.runParser options () "<stdin>"

data Option
  = Color ![(Text, Text)]
  | Quoted !Text !Text
  deriving (Eq, Show)

options :: Parser [Option]
options = P.many1 $ do
  P.spaces
  _ <- P.string "--"
  oName <- concat <$> ((:) <$> name <*> P.many ((:) <$> P.char '-' <*> name))
  _ <- P.char '='
  case oName of
    "color" -> Color <$> colors
    quotedOption -> Quoted (T.pack quotedOption) <$> quoted

quoted :: Parser Text
quoted =
  T.pack <$> P.between (P.char '"') (P.char '"') (P.many (P.satisfy (/= '"')))

colors :: Parser [(Text, Text)]
colors = (`P.sepBy1` P.char ',') $ do
  cName <- (<>) <$> name <*> P.option mempty (P.string "+")
  _ <- P.char ':'
  cValue <- (:) <$> P.char '#' <*> P.count 6 P.hexDigit
  pure (T.pack cName, T.pack cValue)

name :: Parser String
name = P.many1 P.letter
