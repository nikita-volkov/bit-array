module BitArray where

import BitArray.Prelude hiding (map, toList, traverse_, foldr)
import qualified BitArray.Parser as Parser
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Foldable as Foldable
import qualified NumericQQ


-- |
-- A @newtype@ wrapper which provides an array-like interface to a type,
-- which has instances of 'Bits', 'FiniteBits' and 'Num'.
--
-- You can construct bit arrays by wrapping numeric values:
--
-- >>> BitArray (7 :: Int8)
-- [qq|00000111|]
--
-- or directly from numeric literals:
--
-- >>> 7 :: BitArray Int8
-- [qq|00000111|]
--
-- or using a binary notation quasi-quoter,
-- assuming you have the @QuasiQuotes@ pragma turned on:
--
-- >>> [qq|0111|] :: BitArray Int8
-- [qq|00000111|]
--
-- @BitArray@ derives the 'Bits' and 'FiniteBits' instances from the base type,
-- so it supports all the standard bitwise operations for fixed-size integral
-- types.
newtype BitArray a = BitArray a
  deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Real, Ix, Generic,
            Typeable, Bits, FiniteBits)

-- |
-- Produces a literal of zeros and ones.
--
-- >>> show (BitArray (5 :: Int8))
-- "[qq|00000101|]"
instance (FiniteBits a) => Show (BitArray a) where
  show = wrap . toString
    where
      wrap = ("[qq|" ++) . (++ "|]")

-- |
-- Parses a literal of zeros and ones.
--
-- >>> read "[qq|1110|]" :: BitArray Int8
-- [qq|00001110|]
--
-- >>> unwrap (read "[qq|1110|]") :: Int
-- 14
instance (FiniteBits a) => Read (BitArray a) where
  readsPrec = const $ ReadP.readP_to_S $ parser
    where
      parser =
        BitArray <$> ReadP.string "[qq|" *> Parser.bits <* ReadP.string "|]"

instance (FiniteBits a) => IsString (BitArray a) where
  fromString =
    fromMaybe (error "Unparsable bit array string") . parseString

-- * Constructors and converters
-------------------------

-- |
-- A binary number quasi-quoter.
-- Produces a numeric literal at compile time.
-- Can be used to construct both bit arrays and integral numbers.
--
-- >>> [qq|011|] :: Int
-- 3
--
-- >>> [qq|011|] :: BitArray Int8
-- [qq|00000011|]
qq = NumericQQ.bin

-- | Unwrap the underlying value of a bit array.
unwrap :: BitArray a -> a
unwrap (BitArray a) = a

-- ** Strings
-------------------------

-- |
-- Convert into a binary notation string.
--
-- >>> toString (BitArray (5 :: Int8))
-- "00000101"
toString :: (FiniteBits a) => BitArray a -> String
toString = fmap (\case True -> '1'; False -> '0') . reverse . toBoolList

-- |
-- Parse a binary notation string.
--
-- >>> parseString "123" :: Maybe (BitArray Int8)
-- Nothing
--
-- >>> parseString "101" :: Maybe (BitArray Int8)
-- Just [qq|00000101|]
parseString :: (FiniteBits a) => String -> Maybe (BitArray a)
parseString = fmap fst . listToMaybe . ReadP.readP_to_S Parser.bits

-- ** Lists
-------------------------

-- |
-- Convert into a list of set bits.
--
-- The list is ordered from least significant to most significant bit.
{-# INLINABLE toList #-}
toList :: (FiniteBits a) => BitArray a -> [a]
toList (BitArray w) =
  processIndexes [0 .. (pred . finiteBitSize) w]
  where
    processIndexes = filter (\w' -> w .&. w' /= zeroBits) . fmap bit

-- | Construct from a list of set bits.
{-# INLINABLE fromList #-}
fromList :: (FiniteBits a) => [a] -> BitArray a
fromList = BitArray . inline Foldable.foldr (.|.) zeroBits

-- |
-- Convert into a list of boolean values,
-- which represent the \"set\" flags of each bit.
--
-- The list is ordered from least significant to most significant bit.
{-# INLINABLE toBoolList #-}
toBoolList :: (FiniteBits a) => BitArray a -> [Bool]
toBoolList (BitArray w) = testBit w <$> [0 .. (pred . finiteBitSize) w]

-- |
-- Construct from a list of boolean flags for the "set" status of each bit.
--
-- The list must be ordered from least significant to most significant bit.
{-# INLINABLE fromBoolList #-}
fromBoolList :: (FiniteBits a) => [Bool] -> BitArray a
fromBoolList = inline fromList . fmap (bit . fst) . filter snd . zip [zeroBits..]

-- * Utils
-------------------------

-- | Map over the set bits.
{-# INLINABLE map #-}
map :: (FiniteBits a, FiniteBits b) => (a -> b) -> BitArray a -> BitArray b
map f = inline fromList . fmap f . inline toList

-- | Perform a right-associative fold over the set bits.
{-# INLINABLE foldr #-}
foldr :: (FiniteBits a) => (a -> b -> b) -> b -> BitArray a -> b
foldr step init = inline Foldable.foldr step init . inline toList

-- | Traverse thru set bits.
{-# INLINABLE mapM_ #-}
mapM_ :: (FiniteBits a, Monad m) => (a -> m b) -> BitArray a -> m ()
mapM_ f = inline Foldable.mapM_ f . inline toList

-- | Traverse thru set bits.
{-# INLINABLE traverse_ #-}
traverse_ :: (FiniteBits a, Applicative f) => (a -> f b) -> BitArray a -> f ()
traverse_ f = inline Foldable.traverse_ f . inline toList
