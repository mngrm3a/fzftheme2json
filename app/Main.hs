module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.Text as T (lines, unlines)
import qualified Data.Text.Encoding as T
-- TODO: use Data.Text.IO.Utf8 with text >= 2.1
import qualified Data.Text.IO as T (getContents, putStrLn)
import Parser (parseOptions)
import Theme (themeFromOptions)

main :: IO ()
main = do
  rawContents <- T.getContents
  contents <- case T.lines rawContents of
    _ : contents' -> pure $ T.unlines contents'
    invalidInput -> error $ "invalid input of length " <> show (length invalidInput)
  case parseOptions contents of
    Right options ->
      themeFromOptions options
        & A.encode
        & BS.toStrict
        & T.decodeUtf8Lenient
        & T.putStrLn
    Left parserError -> error $ show parserError
