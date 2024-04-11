module Synthesis.PopCount (
    synthesizePopCount,
) where

import Abstract.Machine
import Data.BitVector.Sized
import Data.Bits


synthesizePopCount ∷ (KnownNat w) ⇒ Word → Algorithm (BitVector w)
synthesizePopCount dimmension = undefined

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
