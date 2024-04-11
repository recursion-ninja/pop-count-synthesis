{-# LANGUAGE DeriveAnyClass #-}

module Abstract.Machine.Operation (
    Operation (..),
) where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)


data Operation a where
    -- Arithmetic Operators
    ADD ∷ Operation a → Operation a → Operation a
    SUB ∷ Operation a → Operation a → Operation a
    MUL ∷ Operation a → Operation a → Operation a
    -- Bit-wise Operators
    AND ∷ Operation a → Operation a → Operation a
    OR ∷ Operation a → Operation a → Operation a
    XOR ∷ Operation a → Operation a → Operation a
    NEG ∷ Operation a → Operation a
    -- Bit-shift Operators
    -- Index X Value --> Output
    SFTL ∷ Operation a → Operation a → Operation a
    SFTR ∷ Operation a → Operation a → Operation a
    ROTL ∷ Operation a → Operation a → Operation a
    ROTR ∷ Operation a → Operation a → Operation a
    --
    LIT ∷ Natural → Operation a
    VAR ∷ Word → Operation a


deriving stock instance (Data a) ⇒ Data (Operation a)


deriving stock instance Generic (Operation a)


deriving anyclass instance (NFData a) ⇒ NFData (Operation a)


instance Functor Operation where
    fmap f = \case
        ADD x y → ADD (f <$> x) (f <$> y)
        SUB x y → SUB (f <$> x) (f <$> y)
        MUL x y → MUL (f <$> x) (f <$> y)
        AND x y → AND (f <$> x) (f <$> y)
        OR x y → OR (f <$> x) (f <$> y)
        XOR x y → XOR (f <$> x) (f <$> y)
        SFTL x y → SFTL (f <$> x) (f <$> y)
        SFTR x y → SFTR (f <$> x) (f <$> y)
        ROTL x y → ROTL (f <$> x) (f <$> y)
        ROTR x y → ROTR (f <$> x) (f <$> y)
        NEG x → NEG (f <$> x)
        LIT x → LIT x
        VAR ref → VAR ref

{-

Binary Operators: BV -> BV -> BV

( + ) Addition       modulo \(\mathbb{Z}^{d}\)
( - ) Subtraction    modulo \(\mathbb{Z}^{d}\)
( * ) Multiplication modulo \(\mathbb{Z}^{d}\)
( / ) Division       modulo \(\mathbb{Z}^{d}\)
(.&.) Bitwise AND
(.|.) Bitwise OR
-}

{-

Unary Operatiors: BV -> BV

complement Bitwise NEG

-}

{-

Nullary Operators: BV

Hexadecimal literals \(\in \mathbb{Z}^{d}\)

-}
