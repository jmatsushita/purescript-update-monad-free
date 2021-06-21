module Control.Monad.Update.Free where

import Control.Monad.State (State, StateT(..), get, modify_)
import Control.Monad.Free
import Data.Tuple
import Data.Tuple.Nested
import Data.Monoid
import Data.Monoid.Action (class Action, act)
import Control.Monad.Update

--
-- Code below is copied from UpdateState
--


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



-- data Update w s a = Update (s -> (Tuple w a))
-- data State s a    = State  (s -> (Tuple s a))

-- data State s a    = State  (s -> (Tuple a s))
-- type State s = StateT s Identity
-- newtype StateT s m a = StateT (s -> m (Tuple a s))
-- newtype UpdateState w s a = UpdateState (StateT (Tuple w s) Identity)
-- newtype UpdateState w s a = UpdateState ((Tuple w s) -> Identity (Tuple a (Tuple w s)))
newtype UpdateState w s a = UpdateState (State (Tuple w s) a)

instance monadUpdatewithState :: Action w s => MonadUpdate (UpdateState w s) w s where
  -- putAction :: w -> m Unit
  -- modify_ :: forall s m. MonadState s m => (s -> s) -> m Unit
  putAction w' = modify_ (\(Tuple w s) -> Tuple (w <> w') (act w' s))
  -- getState :: m s
  getState = snd <$> get
  


-- import Data.Tuple.Nested

-- import Control.Comonad.Cofree (Cofree, buildCofree, head, tail)
-- import Control.Monad.Free (Free, liftF, resume)
-- import Data.Either (Either(..))
-- import Data.Identity (Identity(..))
-- import Data.Tuple (Tuple(..), fst, snd, uncurry)
-- import Effect (Effect)
-- import Effect.Console (logShow)
-- import Prelude (class Functor, Unit, bind, discard, identity, pure, unit, ($), (<<<), (>>=))


-- -- newtype State s a = State { runState :: s -> (a,s) }

-- -- https://www.reddit.com/r/haskell/comments/50zvyb/why_is_liststate_not_a_free_monad/
-- -- According to this thread, the below is not the State monad, it's something that is too
-- -- or more observable than the State monad,

-- -- Polysemy uses:
-- -- data State s m a where
-- --   Get :: State s m s
-- --   Put :: s -> State s m ()

-- -- spirit of free (actual implementation is more complex)
-- -- data Free f a = Pure a | Free (f (Free f a))
-- type State s = Free (StateF s)


-- -- This encoding makes put easier to write
-- data StateF s k
--   = Get (s -> k)  -- there's a put/get adjunction going on. (->) -| (,)
--   | Put (s /\ k) 

-- derive instance functorStateF :: Functor (StateF s)

-- -- liftF :: f a -> Free f a
-- get :: forall s. State s s
-- get = liftF $ Get identity 

-- put :: forall s. s -> State s Unit
-- put s = liftF $ Put (s /\ unit)

-- modify :: forall s. (s -> s) -> State s Unit
-- modify f = get >>= \s -> put (f s)

-- -- use this approach for interpreter http://dlaing.org/cofun/posts/free_and_cofree.html
-- -- same as the comonadic ui approach, so it should be usable for streaming syntax.

-- -- data AdderF k =
-- --     Add Int (Bool -> k)
-- --   | Clear k
-- --   | Total (Int -> k)

-- -- data CoAdderF k = CoAdderF {
-- --     addH   :: Int -> (Bool, k)
-- --   , clearH :: k
-- --   , totalH :: (Int, k)
-- --   }

-- -- coiter :: Functor f => (a -> f a) -> a -> Cofree f a
-- -- I think is the equivalent in purescript of
-- -- buildCofree :: forall f s a. Functor f => (s -> Tuple a (f s)) -> s -> Cofree f a
-- -- where the function we iterate with ignores the first part of the tuple.

-- coiter :: forall f a. Functor f => (a -> f a) -> a -> Cofree f a
-- coiter f a = buildCofree (\s -> Tuple s (f s)) a

-- -- data Cofree f a = a :< f (Cofree f a)
-- data CoStateF s k = CoStateF {
--     getH :: s /\ k
--   , putH :: s -> k -- could also be s -> (k /\ Unit) depending if we want to make
--                    -- the pairing or the interpreter simpler
-- }

-- derive instance functorCoStateF :: Functor (CoStateF s)

-- -- instance functorCoStateF :: Functor (CoStateF s) where
-- --   map f (CoStateF {getH: g, putH: p}) = CoStateF
-- --     { getH: map f g
-- --     , putH: map f p
-- --     }
 
-- coGet :: forall s. s -> (s /\ s) 
-- coGet s = (s /\ s)

-- coPut :: forall s. s -> s -> (s /\ Unit)
-- coPut _ s' = (s' /\ unit)

-- -- Make a type class or pairings
-- class (Functor f, Functor g) <= Pairing f g where 
--   pair :: forall a b c. (a -> b -> c) -> f a -> g b -> c

-- instance identityPairing :: Pairing Identity Identity where
--   pair f (Identity a) (Identity b) = f a b

-- instance functiontuplePairing :: Pairing ((->) a) (Tuple a) where
--   pair p f = uncurry (p <<< f)

-- instance tuplefunctionPairing :: Pairing (Tuple a) ((->) a) where
--   pair p f g = p (snd f) (g (fst f))

-- -- Oopsie. Since Free is not the simple Pure | Free for performance... 
-- -- instance freeCofreepairing :: Pairing f g => Pairing (Cofree f) (Free g) where
-- --   pair p (a :< _ ) (Pure x)  = p a x
-- --   pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

-- instance freeCofreepairing :: Pairing f g => Pairing (Cofree f) (Free g) where
--   pair p c f  = case (resume f) of
--     Right x ->  p (head c) x 
--     Left gs ->  pair (pair p) (tail c) gs 

-- instance pairingState :: Pairing (CoStateF s) (StateF s) where
--   pair f (CoStateF { getH }) (Get k) = pair f getH k
--   pair f (CoStateF { putH }) (Put p) = pair f putH p


-- -- modify is a program in the Free DSL, so we can create the paired interpreter manually if we want
-- -- the point could be if we had a more efficient interpretation of that particular operation I think
-- -- modifyCo :: forall s. (s -> s) -> CoState s Unit -> Unit
-- -- modifyCo f w = pair (\_ b -> b) w (modify f)

-- type CoState s = Cofree (CoStateF s)

-- -- the interpreter needs to be for a concrete state?
-- mkCoState :: forall s. s -> CoState s s
-- mkCoState init = coiter next init
--   where
--     -- next :: forall f a. Functor f => s -> f s
--     -- next :: s -> CoStateF s (s /\ a)
--     next w = CoStateF {getH : coGet w, putH : fst <<< coPut w}

-- run :: forall s a. State s a -> CoState s a -> a
-- run s w = pair (\_ b -> b) w s

-- put15 :: State Int Int
-- put15 = do
--   put 15
--   x <- get
--   pure x

-- main :: Effect Unit
-- main = do
--   logShow $ run put15 $ mkCoState 1
--   logShow $ run (put 10 >>= \x -> get) $ mkCoState 1
--   logShow $ run get $ mkCoState 1


-- -- interpretState :: forall s a. StateF s a -> (s -> Tuple a s)
-- -- interpretState (Get s s) = Tuple s s
-- -- interpretState (Put s _)  = Tuple unit s

-- -- This encoding is the same as https://github.com/natefaubion/purescript-run/blob/v3.0.1/src/Run/State.purs#L32-L32
-- -- It makes modify easier to write
-- -- data StateF s a
-- --   = Get (s -> s)
-- --   | Put (s -> a)

-- -- get :: forall s. State s s
-- -- get = liftF $ Get identity

-- -- put :: forall s. s -> State s Unit
-- -- put s = liftF $ Put (\_ -> unit)

-- -- modify :: forall s. (s -> s) -> State s Unit
-- -- modify f = get >>= \s -> put (f s)

-- -- interpretState :: forall s a. StateF s a -> Tuple s a
-- -- interpretState (Put f) = Tuple s a
-- -- interpretState (Get f) = Tuple (f s) a

-- -- runState :: forall s a. State s a -> Tuple s a
-- -- runState = foldFree interpretState 

-- data UpdateF = UpdateF