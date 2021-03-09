module Constrains
    ( Constraint
    , applyAll
    ) where

import Data.Sequence
import Data.Map (Map)
import qualified Data.Map as M

newtype Constraint e s = Constraint
    { apply :: s -> Either e (s, Seq (Constraint e s) ) }

applyAll :: s -> [Constraint e s] -> Either e s
applyAll s cs = applyAll' s (fromList cs)

applyAll' :: s -> Seq (Constraint e s) -> Either e s
applyAll' s Empty = Right s
applyAll' s (c :<| cs) = do
    (s', cs') <- apply c s
    applyAll' s' (cs >< cs')

-- | Actual Action to try insert into constraints
type Action = (Int, [Planet])

-- | (id, max satisfiable power)
type Planet = (String, Int)

-- | Map indicating what planet has to satisfy what constraints
type ConstraintMap = Map Planet [GroupConstraint]

-- | Typical constraint concerning group bondage
type GroupConstraint = (Int, [Planet])

-- | Explicit Constraint type
type PlanetConstraint = Constraint () ConstraintMap

-- |
toConstraint :: Action -> PlanetConstraint
toConstraint p = Constraint $ \s -> Right (s, fromList $ map buildConstraint (s M.! p))


buildConstraint :: GroupConstraint -> PlanetConstraint
buildConstraint (amount, [(_, max)]) = Constraint $ \s -> if amount < max
    then Right (s, Empty)
    else Left ()
buildConstraint (amount, x:xs) = Constraint $ \s -> Left ()
