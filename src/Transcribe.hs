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


module Transcribe
    ( module Abstract.Interpretation
    , module Abstract.Machine
    , module Data.PseudocodeRendering
    , transcribe
    ) where


import Abstract.Interpretation
import Abstract.Machine
import Data.PseudocodeRendering
import Data.String(IsString(..))


transcribe :: (Integral i, IsString s) => Algorithm i -> s
transcribe = getPseudocodeRendering . interpret "" languageC
