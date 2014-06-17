module BitArray where

import BitArray.Prelude hiding (map, toList, traverse_, foldr)
import qualified BitArray.Parser as Parser
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Foldable as Foldable
import qualified NumericQQ


-- |
-- A @newtype@ wrapper which provides an array-like interface to a type, 
-- which has instances of 'Bits' and 'Num'.
-- 
-- You can construct bit arrays by wrapping numeric values:
-- 
-- >>> BitArray (7 :: Int8)
-- 00000111
-- 
-- or directly from numeric literals:
-- 
-- >>> 7 :: BitArray Int8
-- 00000111
-- 
-- or using a binary notation quasi-quoter, 
-- assuming you have the @QuasiQuotes@ pragma turned on:
-- 
-- >>> [qq|0111|] :: BitArray Int8
-- 00000111
-- 
-- @BitArray@ derives the 'Bits' instance from the base type,
-- so it supports all the standard bitwise operations as well.
newtype BitArray a = BitArray a
  deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Real, Ix, Generic, 
            Typeable, Bits)

-- | 
-- Produces a string of zeros and ones.
-- 
-- >>> show (BitArray (5 :: Int8))
-- "00000101"
instance (Bits a) => Show (BitArray a) where
  show = fmap (\case True -> '1'; False -> '0') . reverse . toBoolList

-- | 
-- Parses a string of zeros and ones.
-- 
-- >>> read "1110" :: BitArray Int8
-- 00001110
-- 
-- >>> unwrap (read "1110") :: Int
-- 14
instance (Bits a, Num a) => Read (BitArray a) where
  readsPrec = const $ ReadP.readP_to_S $ BitArray <$> Parser.bits

instance (Bits a, Num a) => IsString (BitArray a) where
  fromString = read

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
-- 00000011
qq = NumericQQ.bin

-- | Unwrap the underlying value of a bit array.
unwrap :: BitArray a -> a
unwrap (BitArray a) = a

-- | 
-- Convert into a list of set bits.
-- 
-- The list is ordered from least significant to most significant bit.
{-# INLINABLE toList #-}
toList :: (Bits a, Num a) => BitArray a -> [a]
toList (BitArray w) = 
  processIndexes [0 .. (pred . bitSize) w]
  where
    processIndexes = filter (\w' -> w .&. w' /= 0) . fmap bit

-- | Construct from a list of set bits.
{-# INLINABLE fromList #-}
fromList :: (Bits a, Num a) => [a] -> BitArray a
fromList = BitArray . inline Foldable.foldr (.|.) 0

-- | 
-- Convert into a list of boolean values,
-- which represent the \"set\" flags of each bit.
-- 
-- The list is ordered from least significant to most significant bit.
{-# INLINABLE toBoolList #-}
toBoolList :: (Bits a) => BitArray a -> [Bool]
toBoolList (BitArray w) = testBit w <$> [0 .. (pred . bitSize) w]

-- | 
-- Construct from a list of boolean flags for the "set" status of each bit.
-- 
-- The list must be ordered from least significant to most significant bit.
{-# INLINABLE fromBoolList #-}
fromBoolList :: (Bits a, Num a) => [Bool] -> BitArray a
fromBoolList = fromList . fmap (bit . fst) . filter snd . zip [0..]

-- * Utils
-------------------------

-- | Map over the set bits.
{-# INLINABLE map #-}
map :: (Bits a, Num a, Bits b, Num b) => (a -> b) -> BitArray a -> BitArray b
map f = fromList . fmap f . inline toList

-- | Perform a right-associative fold over the set bits.
{-# INLINABLE foldr #-}
foldr :: (Bits a, Num a) => (a -> b -> b) -> b -> BitArray a -> b
foldr step init = inline Foldable.foldr step init . inline toList

-- | Traverse thru set bits.
{-# INLINABLE mapM_ #-}
mapM_ :: (Bits a, Num a, Monad m) => (a -> m b) -> BitArray a -> m ()
mapM_ f = inline Foldable.mapM_ f . inline toList

-- | Traverse thru set bits.
{-# INLINABLE traverse_ #-}
traverse_ :: (Bits a, Num a, Applicative f) => (a -> f b) -> BitArray a -> f ()
traverse_ f = inline Foldable.traverse_ f . inline toList
