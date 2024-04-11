{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Transcribe (
    module Abstract.Interpretation,
    module Abstract.Machine,
    module Data.PseudocodeRendering,
    transcribe,
) where

import Abstract.Interpretation
import Abstract.Machine
import Data.PseudocodeRendering
import Data.String (IsString (..))


transcribe ∷ (Integral i, IsString s) ⇒ Algorithm i → s
transcribe = getPseudocodeRendering . interpret "" languageC
