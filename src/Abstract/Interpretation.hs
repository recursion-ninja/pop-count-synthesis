{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Abstract.Interpretation (
    -- * Interpretor

    -- ** Data-type
    Interpretation (..),

    -- * Variable State

    -- ** Data-type
    VariableState (),

    -- ** Operations
    getVariable,
    setVariable,

    -- * Evaluator
    interpret,
) where

import Abstract.Machine.Algorithm (Algorithm (..))
import Abstract.Machine.Operation (Operation (..))
import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Foldable (foldl')
import Data.Functor.Classes (Eq1, Ord1)
import Data.IntMap.Strict (IntMap, insert, singleton, (!))
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic, Generic1)


newtype Interpretation i o = Interpretation {getInterpretor ∷ VariableState o → (Word, Operation i) → o}


newtype VariableState a = VarState (IntMap a)


deriving stock instance (Data a) ⇒ Data (VariableState a)


deriving newtype instance (Eq a) ⇒ Eq (VariableState a)


deriving newtype instance Eq1 VariableState


deriving stock instance Generic (Interpretation i o)


deriving stock instance Generic (VariableState a)


deriving stock instance Generic1 VariableState


deriving anyclass instance (NFData a) ⇒ NFData (VariableState a)


deriving newtype instance (Ord a) ⇒ Ord (VariableState a)


deriving newtype instance Ord1 VariableState


getVariable ∷ VariableState a → Word → a
getVariable (VarState mapping) ref = mapping ! (fromEnum ref)


setVariable ∷ Word → a → VariableState a → VariableState a
setVariable ref val (VarState mapping) = VarState $ insert (fromEnum ref) val mapping


interpret ∷ (Semigroup o) ⇒ o → Interpretation i o → Algorithm i → o
interpret seed interpretation (Algorithm (start :| steps)) =
    let go (!references, !result) = fmap (result <>) . run references

        interpretor = getInterpretor interpretation

        run references step@(note, _) =
            let result = interpretor references step
            in  (setVariable note result references, result)

        startState = run (initialInput seed) start
    in  snd $ foldl' go startState steps


initialInput ∷ a → VariableState a
initialInput = VarState . singleton 0
