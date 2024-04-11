{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language DerivingVia #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language QuantifiedConstraints #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}


module Data.ModularBitField
    ( ModularBitField
    , modularBitField
    -- * Constructor
    , ofDimension
    ) where


import Abstract.Machine(Operation(..))
import Abstract.Interpretation(Interpretation(..), getVariable)
import Data.BitVector.Sized

import Control.DeepSeq (NFData)
import Data.Bits
import Data.Data (Data)
import Data.Semigroup (Last(..))
import GHC.Generics (Generic, Generic1)
import Numeric.Natural (Natural)


newtype ModularBitField a = ModBits (Last a)
    deriving (Bits, Enum, Num, Show) via a


deriving stock    instance Data a => Data (ModularBitField a)


deriving newtype  instance Eq a => Eq (ModularBitField a)


deriving stock    instance Generic (ModularBitField a)


deriving stock    instance Generic1 ModularBitField


deriving anyclass instance NFData a => NFData (ModularBitField a)


deriving newtype  instance Ord a => Ord (ModularBitField a)


deriving newtype  instance Semigroup (ModularBitField a)


ofDimension :: (Integral v, Integral d) => v -> d -> ModularBitField (BitVector w)
ofDimension val dim =
    let dim' :: Word
        dim' = fromIntegral $ abs dim `mod` fromIntegral (maxBound :: Word)

        val' :: Natural
        val' = fromIntegral (abs val) `mod` (2 ^ dim')
        
    in  ModBits . Last $ fromNumber dim' val'

  
modularBitField :: (Bits o, Enum o, Num o) => Interpretation i o
modularBitField = Interpretation $ \vars ->
    let run = \case
            ADD  x y -> run x   +   run y
            SUB  x y -> run x   -   run y
            MUL  x y -> run x   *   run y
            AND  x y -> run x  .&.  run y
            OR   x y -> run x  .|.  run y
            XOR  x y -> run x `xor` run y
            SFTL x y -> run y  `shiftL` fromEnum (run x)
            SFTR x y -> run y  `shiftR` fromEnum (run x)
            ROTL x y -> run y `rotateL` fromEnum (run x)
            ROTR x y -> run y `rotateR` fromEnum (run x)
            NEG  x   -> complement $ run x
            LIT  x   -> toEnum $ fromIntegral x
            VAR  ref -> getVariable vars ref
    in  uncurry $ const run
