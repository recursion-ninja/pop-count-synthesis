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


module Data.PseudocodeRendering
    ( -- * Interpretor
      -- ** Data-type
      PseudocodeRendering()
    , getPseudocodeRendering
    , languageC
    ) where


import Abstract.Interpretation (Interpretation(..))
import Abstract.Machine.Operation (Operation(..))
import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Numeric (showHex)


newtype PseudocodeRendering = PseudocodeRendering String


deriving stock    instance Data PseudocodeRendering


deriving newtype  instance Eq PseudocodeRendering


deriving stock    instance Generic PseudocodeRendering


deriving newtype  instance IsString PseudocodeRendering


deriving anyclass instance NFData PseudocodeRendering


deriving newtype  instance Ord PseudocodeRendering


deriving newtype  instance Semigroup PseudocodeRendering


deriving newtype  instance Show PseudocodeRendering


getPseudocodeRendering :: IsString s => PseudocodeRendering -> s
getPseudocodeRendering (PseudocodeRendering str) = fromString str


languageC :: (Integral i, IsString s, Semigroup s) => Interpretation i s
languageC = Interpretation $ \_ -> 
    let renderAll :: (Integral i, IsString s, Semigroup s) => Operation i -> s
        renderAll = \case
              ADD  x y -> renderBin "+" x y
              SUB  x y -> renderBin "-" x y
              MUL  x y -> renderBin "*" x y
              AND  x y -> renderBin "&" x y
              OR   x y -> renderBin "|" x y
              XOR  x y -> renderBin "^" x y
              SFTL x y -> renderBin "<<" x y
              SFTR x y -> renderBin ">>" x y
              ROTL x y -> renderBin "<|" x y
              ROTR x y -> renderBin "|>" x y
              otherOp  -> renderSub otherOp

        renderBin :: (Integral i, IsString s, Semigroup s) => s -> Operation i -> Operation i -> s
        renderBin op lhs rhs = renderSub lhs <> " " <> op <> " " <> renderSub rhs

        renderHex :: (Integral i, IsString s) => i -> s
        renderHex = fromString . ("0x" <>) . (`showHex` "")

        renderSub :: (Integral i, IsString s, Semigroup s) => Operation i -> s
        renderSub (LIT val) = renderHex val
        renderSub (NEG val) = "~" <> renderSub val
        renderSub (VAR   0) = "input"
        renderSub (VAR ref) = renderVar ref
        renderSub otherOp   = "( " <> renderAll otherOp <> " )"

        renderVar :: (IsString s, Semigroup s) => Word -> s
        renderVar ref =
            let prefix
                    | ref < 10  = "var0"
                    | otherwise = "var"
            in  prefix <> fromString (show ref)

        run :: (Integral i, IsString s, Semigroup s) => (Word, Operation i) -> s
        run (note, op) = renderVar note <> " = " <> renderAll op <> "\n"
    in  run
