{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Theme (Theme (..), ThemeColors (..), themeFromOptions) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import Data.Foldable (foldl')
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Parser (Option (..))

data Theme = Theme
  { themeColors :: !ThemeColors,
    themeBorder :: !(Maybe Text),
    themeBorderLabel :: !(Maybe Text),
    themePreviewWindow :: !(Maybe Text),
    themePrompt :: !(Maybe Text),
    themeMarker :: !(Maybe Text),
    themePointer :: !(Maybe Text),
    themeSeparator :: !(Maybe Text),
    themeScrollbar :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

data ThemeColors = ThemeColors
  { colorFg :: !(Maybe Text),
    colorFgPlus :: !(Maybe Text),
    colorBg :: !(Maybe Text),
    colorBgPlus :: !(Maybe Text),
    colorHl :: !(Maybe Text),
    colorHlPlus :: !(Maybe Text),
    colorInfo :: !(Maybe Text),
    colorMarker :: !(Maybe Text),
    colorPrompt :: !(Maybe Text), --
    colorSpinner :: !(Maybe Text),
    colorPointer :: !(Maybe Text),
    colorHeader :: !(Maybe Text),
    colorBorder :: !(Maybe Text),
    colorLabel :: !(Maybe Text),
    colorQuery :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

themeFromOptions :: [Option] -> Theme
themeFromOptions = foldl' mergeOption defaultTheme . flattenColors
  where
    mergeOption os (Color cs) = os {themeColors = themeColorsFromOption cs}
    mergeOption os (Quoted "border" v) = os {themeBorder = Just v}
    mergeOption os (Quoted "border-label" v) = os {themeBorderLabel = Just v}
    mergeOption os (Quoted "preview-window" v) = os {themePreviewWindow = Just v}
    mergeOption os (Quoted "prompt" v) = os {themePrompt = Just v}
    mergeOption os (Quoted "marker" v) = os {themeMarker = Just v}
    mergeOption os (Quoted "pointer" v) = os {themePointer = Just v}
    mergeOption os (Quoted "separator" v) = os {themeSeparator = Just v}
    mergeOption os (Quoted "scrollbar" v) = os {themeScrollbar = Just v}
    mergeOption os _ = os
    flattenColors os =
      let (os', cs') = foldl' go ([], []) os
       in Color (concat cs') : os'
      where
        go (os', cs') (Color c) = (os', c : cs')
        go (os', cs') o = (o : os', cs')

themeColorsFromOption :: [(Text, Text)] -> ThemeColors
themeColorsFromOption = foldl' mergeColor defaultThemeColors
  where
    mergeColor cs ("fg", v) = cs {colorFg = Just v}
    mergeColor cs ("fg+", v) = cs {colorFgPlus = Just v}
    mergeColor cs ("bg", v) = cs {colorBg = Just v}
    mergeColor cs ("bg+", v) = cs {colorBgPlus = Just v}
    mergeColor cs ("hl", v) = cs {colorHl = Just v}
    mergeColor cs ("hl+", v) = cs {colorHlPlus = Just v}
    mergeColor cs ("info", v) = cs {colorInfo = Just v}
    mergeColor cs ("marker", v) = cs {colorMarker = Just v}
    mergeColor cs ("prompt", v) = cs {colorPrompt = Just v}
    mergeColor cs ("spinner", v) = cs {colorSpinner = Just v}
    mergeColor cs ("pointer", v) = cs {colorPointer = Just v}
    mergeColor cs ("header", v) = cs {colorHeader = Just v}
    mergeColor cs ("border", v) = cs {colorBorder = Just v}
    mergeColor cs ("label", v) = cs {colorLabel = Just v}
    mergeColor cs ("query", v) = cs {colorQuery = Just v}
    mergeColor cs _ = cs

instance ToJSON Theme where
  toEncoding =
    A.genericToEncoding
      A.defaultOptions
        { A.omitNothingFields = True,
          A.fieldLabelModifier =
            mapFieldName "colors" "color"
              . A.camelTo2 '-'
              . stripRecordPrefix "theme"
        }

instance ToJSON ThemeColors where
  toEncoding =
    A.genericToEncoding
      A.defaultOptions
        { A.omitNothingFields = True,
          A.fieldLabelModifier =
            A.camelTo2 '-' . replacePlus . stripRecordPrefix "color"
        }

mapFieldName :: String -> String -> String -> String
mapFieldName m r s
  | s == m = r
  | otherwise = s

replacePlus :: [Char] -> [Char]
replacePlus s
  | l > 4 && suffix == "Plus" = prefix <> "+"
  | otherwise = s
  where
    l = L.length s
    (prefix, suffix) = L.splitAt (l - 4) s

stripRecordPrefix :: String -> String -> String
stripRecordPrefix p s = fromMaybe s $ L.stripPrefix p s

defaultTheme :: Theme
defaultTheme =
  Theme
    defaultThemeColors
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

defaultThemeColors :: ThemeColors
defaultThemeColors =
  ThemeColors
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
