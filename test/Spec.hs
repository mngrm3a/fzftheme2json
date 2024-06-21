import Control.Monad ((<=<))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Parser (parseOptionsSkipFirstLine)
import Paths_fzftheme (getDataDir, getDataFileName)
import System.FilePath ((<.>))
import System.FilePath.Posix ((</>))
import System.IO (IOMode (ReadMode), withFile)
import Test.Hspec (Spec, around, describe, hspec, it)
import Test.Hspec.Golden (Golden (..))
import Theme (themeFromOptions)

main :: IO ()
main = hspec spec

spec :: Spec
spec = around withTestEnv $ do
  describe "parseOptionsSkipFirstLine" $
    it "parses all options after skipping the first line" $
      \(Env input goldenText) -> do
        let result = parseOptionsSkipFirstLine input
        goldenText "parseOptionsSkipFirstLine" $ eitherShowWith showText result

  describe "themeFromOptions" $
    it "converts a list of options to a theme" $
      \(Env input goldenText) -> do
        let result = parseOptionsSkipFirstLine input >>= Right . themeFromOptions
        goldenText "themeFromOptions" $ eitherShowWith showText result

  describe "encode" $
    it "encodes the expected json structure" $
      \(Env input goldenText) -> do
        let result =
              parseOptionsSkipFirstLine input
                >>= Right
                  . T.decodeUtf8
                  . BS.toStrict
                  . A.encode
                  . themeFromOptions
        goldenText "encode" $ eitherShowWith id result

showText :: (Show a) => a -> Text
showText = T.pack . show

eitherShowWith :: (Show a) => (t -> Text) -> Either a t -> Text
eitherShowWith f (Right r) = f r
eitherShowWith _ (Left e) = T.pack $ show e

data Env = Env
  { envInput :: !Text,
    envGolden :: String -> Text -> Golden Text
  }

withTestEnv :: (Env -> IO a) -> IO a
withTestEnv a = do
  inputFileName <- getDataFileName "test/input.txt"
  goldenDir <- getDataDir <&> (</> "test/golden")
  withFile inputFileName ReadMode $
    a . flip Env (mkGoldenText goldenDir)
      <=< T.hGetContents

mkGoldenText :: String -> String -> Text -> Golden Text
mkGoldenText goldenDir name actualOutput =
  Golden
    { output = actualOutput,
      encodePretty = T.unpack,
      writeToFile = (. T.encodeUtf8) . BS.writeFile,
      readFromFile = fmap T.decodeUtf8 . BS.readFile,
      goldenFile = goldenDir </> name <.> "golden",
      actualFile = Just $ goldenDir </> name <.> "actual",
      failFirstTime = False
    }