module Control.Monad.State.Free where
  
import Data.Tuple.Nested

import Control.Comonad.Cofree (Cofree, buildCofree)
import Control.Monad.Free (Free, liftF)
import Data.Tuple (Tuple(..), fst)  
import Effect (Effect)
import Effect.Console (logShow)
import Prelude (class Functor, Unit, bind, discard, identity, pure, unit, ($), (<<<), (>>=))

import Data.Functor.Pairing.Class (class Pairing, pair)
-- newtype State s a = State { runState :: s -> (a,s) }

-- https://www.reddit.com/r/haskell/comments/50zvyb/why_is_liststate_not_a_free_monad/
-- According to this thread, the below is not the State monad, it's something that is too
-- or more observable than the State monad,

-- Polysemy uses:
-- data State s m a where
--   Get :: State s m s
--   Put :: s -> State s m ()

-- spirit of free (actual implementation is more complex)
-- data Free f a = Pure a | Free (f (Free f a))
type State s = Free (StateF s)


-- This encoding makes put easier to write
data StateF s k
  = Get (s -> k)  -- there's a put/get adjunction going on. (->) -| (,)
  | Put (s /\ k) 

derive instance functorStateF :: Functor (StateF s)

-- liftF :: f a -> Free f a
get :: forall s. State s s
get = liftF $ Get identity 

put :: forall s. s -> State s Unit
put s = liftF $ Put (s /\ unit)

modify :: forall s. (s -> s) -> State s Unit
modify f = get >>= \s -> put (f s)

-- use this approach for interpreter http://dlaing.org/cofun/posts/free_and_cofree.html
-- same as the comonadic ui approach, so it should be usable for streaming syntax.

-- data AdderF k =
--     Add Int (Bool -> k)
--   | Clear k
--   | Total (Int -> k)

-- data CoAdderF k = CoAdderF {
--     addH   :: Int -> (Bool, k)
--   , clearH :: k
--   , totalH :: (Int, k)
--   }

-- coiter :: Functor f => (a -> f a) -> a -> Cofree f a
-- I think is the equivalent in purescript of
-- buildCofree :: forall f s a. Functor f => (s -> Tuple a (f s)) -> s -> Cofree f a
-- where the function we iterate with ignores the first part of the tuple.

coiter :: forall f a. Functor f => (a -> f a) -> a -> Cofree f a
coiter f a = buildCofree (\s -> Tuple s (f s)) a

-- data Cofree f a = a :< f (Cofree f a)
data CoStateF s k = CoStateF {
    getH :: s /\ k
  , putH :: s -> k -- could also be s -> (k /\ Unit) depending if we want to make
                   -- the pairing or the interpreter simpler
}

-- So this should be the store comonad.

derive instance functorCoStateF :: Functor (CoStateF s)

-- instance functorCoStateF :: Functor (CoStateF s) where
--   map f (CoStateF {getH: g, putH: p}) = CoStateF
--     { getH: map f g
--     , putH: map f p
--     }
 
coGet :: forall s. s -> (s /\ s) 
coGet s = (s /\ s)

coPut :: forall s. s -> s -> (s /\ Unit)
coPut _ s' = (s' /\ unit)



instance pairingState :: Pairing (CoStateF s) (StateF s) where
  pair f (CoStateF { getH }) (Get k) = pair f getH k
  pair f (CoStateF { putH }) (Put p) = pair f putH p


-- modify is a program in the Free DSL, so we can create the paired interpreter manually if we want
-- the point could be if we had a more efficient interpretation of that particular operation I think
-- modifyCo :: forall s. (s -> s) -> CoState s Unit -> Unit
-- modifyCo f w = pair (\_ b -> b) w (modify f)

type CoState s = Cofree (CoStateF s)

-- the interpreter needs to be for a concrete state?
mkCoState :: forall s. s -> CoState s s
mkCoState init = coiter next init
  where
    -- next :: forall f a. Functor f => s -> f s
    -- next :: s -> CoStateF s (s /\ a)
    next w = CoStateF {getH : coGet w, putH : fst <<< coPut w}

run :: forall s a. State s a -> CoState s a -> a
run s w = pair (\_ b -> b) w s

put15 :: State Int Int
put15 = do
  put 15
  x <- get
  pure x

main :: Effect Unit
main = do
  logShow $ run put15 $ mkCoState 1
  logShow $ run (put 10 >>= \x -> get) $ mkCoState 1
  logShow $ run get $ mkCoState 1


-- interpretState :: forall s a. StateF s a -> (s -> Tuple a s)
-- interpretState (Get s s) = Tuple s s
-- interpretState (Put s _)  = Tuple unit s

-- This encoding is the same as https://github.com/natefaubion/purescript-run/blob/v3.0.1/src/Run/State.purs#L32-L32
-- It makes modify easier to write
-- data StateF s a
--   = Get (s -> s)
--   | Put (s -> a)

-- get :: forall s. State s s
-- get = liftF $ Get identity

-- put :: forall s. s -> State s Unit
-- put s = liftF $ Put (\_ -> unit)

-- modify :: forall s. (s -> s) -> State s Unit
-- modify f = get >>= \s -> put (f s)

-- interpretState :: forall s a. StateF s a -> Tuple s a
-- interpretState (Put f) = Tuple s a
-- interpretState (Get f) = Tuple (f s) a

-- runState :: forall s a. State s a -> Tuple s a
-- runState = foldFree interpretState 

data UpdateF = UpdateF