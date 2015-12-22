module BitArray.Parser where

import BitArray.Prelude
import Text.ParserCombinators.ReadP
import Data.Char


digits :: ReadP [Char]
digits = munch isDigit

bitIndexes :: ReadP [Int]
bitIndexes = do
  digitsChars <- digits
  sequence $ do
    (index, char) <- zip [0..] (reverse digitsChars)
    case char of
      '0' -> empty
      '1' -> return (return index)
      _   -> return empty

bits :: (FiniteBits a) => ReadP a
bits = foldr (.|.) zeroBits . map bit <$> bitIndexes

