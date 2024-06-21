module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T (putStrLn)
import Parser (parseOptionsSkipFirstLine)
import Theme (themeFromOptions)

main :: IO ()
main = do
  contents <- T.decodeUtf8 <$> BS.getContents
  case parseOptionsSkipFirstLine contents of
    Right options ->
      themeFromOptions options
        & A.encode
        & BS.toStrict
        & T.decodeUtf8
        & T.putStrLn
    Left parserError -> error $ show parserError
