{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |

Copyright   : © 2020 Alex Washburn
License     : BSD-3-Clause
Maintainer  : github@recursion.ninja
Stability   : Stable

Little-endian bit vectors of dependantly-typed length which are isomorphic to a @[Bool]@ with the /least/ significant bit at the head of the list and the /most/ significant bit at the end of the list. Consequently, the endianness of a bit vector affects the semantics of the following typeclasses:

  * Bits
  * FiniteBits
  * Num
-}
module Data.BitVector.Sized (
    -- * Data-type
    BitVector (..),
    KnownNat,

    -- * Construction
    fromBits,
    fromNumber,

    -- * Conversion
    toBits,
    toBytes,
    toWords,
    toNumber,

    -- * Queries
    dimension,

    -- ** Conversions

    -- * Rendering
    renderBin,
    renderDec,
    renderHex,
) where

import Control.DeepSeq
import Data.Bit (Bit (..))
import Data.Bit qualified as VB
import Data.Bits
import Data.Data
import Data.Foldable hiding (toList)
import Data.Foldable qualified as Fold
import Data.Hashable
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Sized (Vector)
import Data.Vector.Unboxed.Sized qualified as VUS
import Data.Word
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import GHC.Natural
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)


{- |
A little-endian bit vector of non-negative dimension.
-}
newtype BitVector (w ∷ Nat) = BV (Vector w Bit)


type role BitVector phantom


-- | @since 0.1.0
deriving stock instance Generic (BitVector w)


-- | @since 0.1.0
deriving anyclass instance NFData (BitVector w)


-- | @since 0.1.0
deriving newtype instance Eq (BitVector w)


-- | @since 0.1.0
deriving newtype instance (KnownNat w) ⇒ Bits (BitVector w)


-- | @since 0.1.0
deriving newtype instance (KnownNat w) ⇒ FiniteBits (BitVector w)


-- | @since 0.1.0
deriving newtype instance Ord (BitVector w)


-- | @since 0.1.0
deriving newtype instance (KnownNat w) ⇒ Read (BitVector w)


-- | @since 0.1.0
instance Hashable (BitVector d) where
    hash = hashWithSalt 0


    hashWithSalt salt (BV v) =
        let int2Word ∷ Int → Word
            int2Word = unsafeCoerce
            word2Int ∷ Word → Int
            word2Int = unsafeCoerce
        in  word2Int . VU.foldr xor (int2Word salt) . VB.cloneToWords $ VUS.fromSized v


-- | @since 0.1.0
instance (KnownNat w) ⇒ Num (BitVector w) where
    {-# INLINE (+) #-}
    (+) = binaryOperationModulo (+)


    {-# INLINE (*) #-}
    (*) = binaryOperationModulo (*)


    {-# INLINE (-) #-}
    (-) x (BV v) =
        let m = naturalBV x

            compute ∷ Proxy w → BitVector w
            compute pxy =
                let base ∷ Int
                    base = fromEnum $ natVal pxy

                    modulus ∷ Natural
                    modulus = 1 `shiftL` base

                    performOperation ∷ VU.Vector Bit → VU.Vector Bit
                    performOperation w =
                        let n = naturalVU . VB.cloneToWords8 $ w
                            naturalValue ∷ Natural
                            naturalValue = case m `compare` n of
                                LT → modulus - (n - m)
                                _ → m - n
                        in  VU.take base . VB.castFromWords8 $ natBytes naturalValue
                in  BV $ VUS.withVectorUnsafe performOperation v
        in  VUS.knownLength' v compute


    {-# INLINE abs #-}
    abs = id


    {-# INLINE signum #-}
    signum = const zeroBits


    fromInteger = undefined


    {-# INLINE negate #-}
    negate = id


-- | @since 0.1.0
instance (KnownNat w) ⇒ Show (BitVector w) where
    show bv@(BV v) =
        let render ∷ Proxy w → String
            render pxy =
                let n ∷ Natural
                    n = natVal pxy
                    prefix = renderField n
                    format = case n of
                        08 → renderHex
                        16 → renderHex
                        32 → renderHex
                        dim | dim >= 64 → renderHex
                        _ → renderBin
                in  unwords [prefix, format bv]
        in  VUS.knownLength' v render


{- |
/Time:/ \(\, \mathcal{O} \left( 1 \right) \)

/Since: 0.1.0/

Get the dimension of a bit-vector. Preferable to 'finiteBitSize' as it
returns a type which cannot represent a non-negative value and a bit-vector
must have a non-negative dimension.

==== __Examples__

>>> dimension $ 𝙱𝚅₃ 010
2

>>> dimension $ 𝙱𝚅₄ 0011
4
-}
dimension ∷ BitVector w → Word
dimension (BV w) = toEnum . VU.length $ VUS.fromSized w


{- |
/Time:/ \(\, \mathcal{O} \left( n \right) \)

/Since: 0.1.0/

Create a bit-vector of length \( w \) from a /little-endian/ list of "bits."

All 'Enum' values within the input list are treated as @True@ unless the following holds for a value @val@:
>>> toEnum 0 == val

The following properties will hold:

> length . takeWhile not === countLeadingZeros . fromBits
> length . takeWhile not . reverse === countTrailingZeros . fromBits

==== __Examples__

>>> fromBits [True, False, False]
𝙱𝚅₃ 100
-}
{-# INLINEABLE fromBits #-}
fromBits ∷ ∀ b f w. (Eq b, Foldable f, Enum b, KnownNat w) ⇒ f b → BitVector w
fromBits input =
    let pxy ∷ Proxy w
        pxy = Proxy

        base ∷ Int
        base = fromEnum $ natVal pxy

        valZero ∷ b
        valZero = toEnum 0

        toBit ∷ b → Bit
        toBit = Bit . (/= valZero)

        align ∷ VU.Vector Bit → VU.Vector Bit
        align xs =
            let less = VU.take base xs
                len = VU.length less
                off
                    | base > len = base - len
                    | otherwise = 0
                more = VU.replicate off $ Bit False
            in  less <> more

        valBits ∷ VU.Vector Bit
        valBits = align . VU.fromList . fmap toBit $ Fold.toList input

        valOnes ∷ VUS.Vector w Bit
        valOnes = VUS.replicate' pxy $ Bit True
    in  BV $ VUS.withVectorUnsafe (const valBits) valOnes


fromNumber ∷ ∀ i w. (Integral i, KnownNat w) ⇒ i → BitVector w
fromNumber input =
    let pxy ∷ Proxy w
        pxy = Proxy

        base ∷ Int
        base = fromEnum $ natVal pxy

        numBytes ∷ Int
        numBytes = case base `divMod` finiteBitSize (0 ∷ Word) of
            (q, 0) → q
            (q, _) → q + 1

        modulus ∷ Integer
        modulus = 1 `shiftL` base

        padding ∷ VU.Vector Word → VU.Vector Word
        padding vec =
            let len = VU.length vec
                off
                    | numBytes > len = numBytes - len
                    | otherwise = 0
                pad = VU.replicate off 0
            in  vec <> pad

        valInt ∷ Integer
        valInt = toInteger input

        valNat ∷ Natural
        valNat = fromIntegral $ valInt `mod` modulus

        valBytes ∷ VU.Vector Word
        valBytes = natWords valNat

        valBits ∷ VU.Vector Bit
        valBits = VU.take base . VB.castFromWords $ padding valBytes

        valOnes ∷ VUS.Vector w Bit
        valOnes = VUS.replicate' pxy $ Bit True
    in  BV $ VUS.withVectorUnsafe (const valBits) valOnes


{- |
/Time:/ \(\, \mathcal{O} \left( n \right) \)

/Since:/ 0.1.0

Create a /little-endian/ list of bits from a bit vector.

The following will hold:

> length . takeWhile not . toBits === countLeadingZeros
> length . takeWhile not . reverse . toBits === countTrailingZeros

==== __Examples__

>>> toBits [4]11
𝙱𝚅₄ 1101
-}
{-# INLINEABLE toBits #-}
toBits ∷ ∀ b t w. (Enum b, IsList t, Item t ~ b) ⇒ BitVector w → t
toBits (BV v) =
    let convertBit ∷ Bit → b
        convertBit (Bit bitIsTrue)
            | bitIsTrue = toEnum 1
            | otherwise = toEnum 0
    in  fromList $ convertBit <$> VUS.toList v


{- |
/Time:/ \(\, \mathcal{O} \left( \frac{w}{64} \right) \)

/Since: 0.1.0/

Unsigned value of a bit-vector.
-}
{-# INLINEABLE toNumber #-}
toNumber ∷ ∀ i w. (Integral i) ⇒ BitVector w → i
toNumber (BV v) =
    let width ∷ Int
        width = finiteBitSize (0 ∷ Word)

        addUp ∷ Word → (Natural, Natural) → (Natural, Natural)
        addUp val (s, b) = (fromIntegral val * b + s, b `shiftL` width)
    in  fromIntegral . fst . VU.foldr addUp (0, 1) $ getWords v


{- |
/Time:/ \(\, \mathcal{O} \left( \frac{w}{8} \right) \)

/Since: 0.1.0/

The bytes of a bit-vector.
-}
toBytes ∷ ∀ t w. (IsList t, Item t ~ Word8) ⇒ BitVector w → t
toBytes (BV v) = fromList . VU.toList $ getBytes v


{- |
/Time:/ \(\, \mathcal{O} \left( \frac{w}{64} \right) \)

/Since: 0.1.0/

The words of a bit-vector.
-}
toWords ∷ ∀ t w. (IsList t, Item t ~ Word) ⇒ BitVector w → t
toWords (BV v) = fromList . VU.toList $ getWords v


binaryOperationModulo
    ∷ ∀ w
     . (KnownNat w)
    ⇒ (Natural → Natural → Natural)
    → BitVector w
    → BitVector w
    → BitVector w
binaryOperationModulo op x (BV v) =
    let m = naturalBV x

        compute ∷ Proxy w → BitVector w
        compute pxy =
            let base ∷ Int
                base = fromEnum $ natVal pxy

                modulus ∷ Natural
                modulus = 1 `shiftL` base

                performOperation ∷ VU.Vector Bit → VU.Vector Bit
                performOperation w =
                    let n = naturalVU . VB.cloneToWords8 $ w
                        naturalValue ∷ Natural
                        naturalValue = (m `op` n) `mod` modulus
                    in  VU.take base . VB.castFromWords8 $ natBytes naturalValue
            in  BV $ VUS.withVectorUnsafe performOperation v
    in  VUS.knownLength' v compute


getBytes ∷ Vector w Bit → VU.Vector Word8
getBytes = VB.cloneToWords8 . VUS.fromSized


getWords ∷ Vector w Bit → VU.Vector Word
getWords = VB.cloneToWords . VUS.fromSized


natBytes ∷ Natural → VU.Vector Word8
natBytes input =
    let mask ∷ Natural
        mask = fromIntegral (maxBound ∷ Word8)

        go ∷ Natural → Maybe (Word8, Natural)
        go = \case
            0 → Nothing
            n →
                let byte = fromIntegral $ mask .&. n
                    next = n `shiftR` 8
                in  Just (byte, next)
    in  VU.unfoldr go input


natWords ∷ Natural → VU.Vector Word
natWords input =
    let mask ∷ Natural
        mask = fromIntegral (maxBound ∷ Word)

        size ∷ Int
        size = finiteBitSize (0 ∷ Word)

        go ∷ Natural → Maybe (Word, Natural)
        go = \case
            0 → Nothing
            n →
                let byte = fromIntegral $ mask .&. n
                    next = n `shiftR` size
                in  Just (byte, next)
    in  VU.unfoldr go input


naturalBV ∷ BitVector w → Natural
naturalBV (BV v) = naturalVU $ getBytes v


naturalVU ∷ VU.Vector Word8 → Natural
naturalVU =
    let addUp ∷ Word8 → (Natural, Natural) → (Natural, Natural)
        addUp val (s, b) = (fromIntegral val * b + s, b `shiftL` 8)
    in  fst . VU.foldr addUp (0, 1)


renderField ∷ Natural → String
renderField dim =
    let makeSubscript ∷ Char → Char
        makeSubscript = \case
            '0' → '₀'
            '1' → '₁'
            '2' → '₂'
            '3' → '₃'
            '4' → '₄'
            '5' → '₅'
            '6' → '₆'
            '7' → '₇'
            '8' → '₈'
            '9' → '₉'
            c → c
    in  fold ["𝙱𝚅", makeSubscript <$> show dim]


renderBin ∷ BitVector w → String
renderBin (BV v) =
    let getBit ∷ Bit → String → String
        getBit (Bit bitIsTrue)
            | bitIsTrue = ('1' :)
            | otherwise = ('0' :)
    in  VUS.foldr getBit mempty v


renderDec ∷ BitVector w → String
renderDec = show . naturalBV


renderHex ∷ ∀ w. (KnownNat w) ⇒ BitVector w → String
renderHex (BV v) =
    let getNibbles ∷ Word8 → String
        getNibbles byte =
            let lo = byte .&. 0x0F
                hi = (byte .&. 0xF0) `shiftR` 4
                go = \case
                    00 → '0'
                    01 → '1'
                    02 → '2'
                    03 → '3'
                    04 → '4'
                    05 → '5'
                    06 → '6'
                    07 → '7'
                    08 → '8'
                    09 → '9'
                    10 → 'A'
                    11 → 'B'
                    12 → 'C'
                    13 → 'D'
                    14 → 'E'
                    _ → 'F'
            in  [go lo, go hi]

        render ∷ Proxy w → String
        render pxy =
            let n ∷ Natural
                n = natVal pxy
                mask
                    | pred n `mod` 8 < 4 = snip
                    | otherwise = id

                snip ∷ String → String
                snip = \case
                    [] → []
                    [x] → [x]
                    [x, _] → [x]
                    x : y : zs → x : snip (y : zs)
            in  mask . VU.foldMap getNibbles $ getBytes v
    in  VUS.knownLength' v render
