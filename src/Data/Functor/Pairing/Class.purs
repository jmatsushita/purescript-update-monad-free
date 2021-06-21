module Data.Functor.Pairing.Class where


import Prelude (class Functor, (<<<))
import Data.Tuple (Tuple, fst, snd, uncurry)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Control.Comonad.Cofree (Cofree, head, tail)
import Control.Monad.Free (Free, resume)

-- Make a type class or pairings
class (Functor f, Functor g) <= Pairing f g where 
  pair :: forall a b c. (a -> b -> c) -> f a -> g b -> c

instance identityPairing :: Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance functiontuplePairing :: Pairing ((->) a) (Tuple a) where
  pair p f = uncurry (p <<< f)

instance tuplefunctionPairing :: Pairing (Tuple a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

-- Oopsie. Since Free is not the simple Pure | Free for performance... 
-- instance freeCofreepairing :: Pairing f g => Pairing (Cofree f) (Free g) where
--   pair p (a :< _ ) (Pure x)  = p a x
--   pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

instance freeCofreepairing :: Pairing f g => Pairing (Cofree f) (Free g) where
  pair p c f  = case (resume f) of
    Right x ->  p (head c) x 
    Left gs ->  pair (pair p) (tail c) gs 