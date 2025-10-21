module Util.Arabic where

import Prelude
import Data.Char (toCharCode)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.Array (filter)
import Data.Array as Array

-- | Check if a character is Arabic
isArabicChar :: Char -> Boolean
isArabicChar c =
  let code = toCharCode c
  in (code >= 0x0600 && code <= 0x06FF) || -- Arabic block
     (code >= 0x0750 && code <= 0x077F) || -- Arabic Supplement
     (code >= 0xFB50 && code <= 0xFDFF) || -- Arabic Presentation Forms-A
     (code >= 0xFE70 && code <= 0xFEFF)    -- Arabic Presentation Forms-B

-- | Check if a character is a tashkeel (diacritic)
isTashkeel :: Char -> Boolean
isTashkeel c =
  let code = toCharCode c
  in (code >= 0x064B && code <= 0x065F) || -- Arabic diacritics
     code == 0x0640 ||                      -- Tatweel
     code == 0x0670 ||                      -- Superscript Alef
     (code >= 0x06D6 && code <= 0x06ED)    -- Additional diacritics

-- | Remove tashkeel from a string
removeTashkeel :: String -> String
removeTashkeel str =
  let chars = toCharArray str
      filtered = filter (not <<< isTashkeel) chars
  in fromCharArray filtered

-- | Filter to keep only Arabic characters, spaces, and diacritics
filterArabicOnly :: String -> String
filterArabicOnly str =
  let chars = toCharArray str
      filtered = filter isAllowedChar chars
  in fromCharArray filtered
  where
    isAllowedChar c = isArabicChar c || c == ' '

-- | Check if a string contains any Arabic characters
hasArabicChars :: String -> Boolean
hasArabicChars str =
  let chars = toCharArray str
  in filter isArabicChar chars /= []

-- | Split text into tokens: each token is either an Arabic word or non-Arabic text
-- | This preserves all formatting (newlines, punctuation, etc.)
data TextToken = ArabicWord String | NonArabic String

splitIntoTokens :: String -> Array TextToken
splitIntoTokens str =
  let chars = toCharArray str
  in tokenize chars
  where
    tokenize :: Array Char -> Array TextToken
    tokenize [] = []
    tokenize cs =
      case Array.head cs of
        Nothing -> []
        Just c ->
          if isArabicChar c || isTashkeel c
          then
            -- Take Arabic characters (including diacritics)
            let arabicChars = Array.takeWhile (\ch -> isArabicChar ch || isTashkeel ch) cs
                remaining = Array.dropWhile (\ch -> isArabicChar ch || isTashkeel ch) cs
                token = ArabicWord (fromCharArray arabicChars)
            in Array.cons token (tokenize remaining)
          else
            -- Take non-Arabic characters
            let nonArabicChars = Array.takeWhile (\ch -> not (isArabicChar ch || isTashkeel ch)) cs
                remaining = Array.dropWhile (\ch -> not (isArabicChar ch || isTashkeel ch)) cs
                token = NonArabic (fromCharArray nonArabicChars)
            in Array.cons token (tokenize remaining)
