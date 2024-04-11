{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ModularBitField (
    ModularBitField,
    modularBitField,
) where

import Abstract.Interpretation (Interpretation (..), getVariable)
import Abstract.Machine (Operation (..))
import Control.DeepSeq (NFData)
import Data.Bits
import Data.Data (Data)
import Data.Semigroup (Last (..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeNats
import Numeric.Natural (Natural)


newtype ModularBitField a = ModBits (Last a)
    deriving (Bits, Bounded, Enum, FiniteBits, Integral, Num, Real, Show) via a


type role ModularBitField representational


deriving stock instance (Data a) ⇒ Data (ModularBitField a)


deriving newtype instance (Eq a) ⇒ Eq (ModularBitField a)


deriving stock instance Generic (ModularBitField a)


deriving stock instance Generic1 ModularBitField


deriving anyclass instance (NFData a) ⇒ NFData (ModularBitField a)


deriving newtype instance (Ord a) ⇒ Ord (ModularBitField a)


deriving newtype instance Semigroup (ModularBitField a)


modularBitField ∷ (Bits o, Enum o, Integral o, Num o) ⇒ Interpretation i o
modularBitField = Interpretation $ \vars →
    let run = \case
            ADD x y → run x + run y
            SUB x y → run x - run y
            MUL x y → run x * run y
            AND x y → run x .&. run y
            OR x y → run x .|. run y
            XOR x y → run x `xor` run y
            SFTL x y → run y `shiftL` fromEnum (run x)
            SFTR x y → run y `shiftR` fromEnum (run x)
            ROTL x y → run y `rotateL` fromEnum (run x)
            ROTR x y → run y `rotateR` fromEnum (run x)
            NEG x → complement $ run x
            LIT x → fromIntegral x
            VAR ref → getVariable vars ref
    in  uncurry $ const run
