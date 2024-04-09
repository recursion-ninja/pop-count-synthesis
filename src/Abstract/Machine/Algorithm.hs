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


module Abstract.Machine.Algorithm
    ( Algorithm(..)
    , assignment
    ) where

import Abstract.Machine.Operation (Operation)
import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Exts (IsList(..))
import GHC.Generics (Generic,Generic1)


newtype Algorithm a = Algorithm (NonEmpty (Word, Operation a))


deriving stock    instance Data a => Data (Algorithm a)


deriving stock    instance Generic (Algorithm a)


deriving stock    instance Generic1 Algorithm


deriving newtype  instance NFData a => NFData (Algorithm a)


deriving newtype  instance Semigroup (Algorithm a)


instance IsList (Algorithm a) where

    type Item (Algorithm a) = (Word, Operation a)

    fromList    []  = error "The algorithm must have at least one operation!"
    fromList (x:xs) = Algorithm $ x :| xs

    toList (Algorithm ops) = toList ops


assignment :: Word -> Operation a -> Algorithm a
assignment = curry (Algorithm . pure)

