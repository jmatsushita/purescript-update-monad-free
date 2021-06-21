module Control.Monad.State.Run where
  
import Data.Tuple.Nested
import Run
import Prim.Row as Row
import Control.Comonad.Cofree (Cofree, buildCofree, head, tail)
import Control.Monad.Free (Free, liftF, resume)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Effect.Console (logShow)
import Prelude (class Functor, type (~>), Unit, bind, const, discard, identity, pure, unit, ($), (<$>), (<<<), (>>=))

-- This already exists here https://pursuit.purescript.org/packages/purescript-run/3.0.1/docs/Run.State
-- But I'm rewriting it with another encoding to:
--  - learn how to do it for the update monad.
--  - use the Pairing with Smash

-- type State s = Free (StateF s)

type STATE s = FProxy (StateF s)

_state = SProxy :: SProxy "state"

-- This encoding makes put easier to write
data StateF s k
  = Get (s -> k)  -- there's a put/get adjunction going on. (->) -| (,)
  | Put (s /\ k) 

derive instance functorStateF :: Functor (StateF s)

-- liftF :: f a -> Free f a
get :: forall s r. Run (state :: STATE s | r) s
get = lift _state $ Get identity 

put :: forall s r. s -> Run (state :: STATE s | r) Unit
put s = lift _state $ Put (s /\ unit)

modify :: forall s r. (s -> s) -> Run (state :: STATE s | r) Unit
modify f = get >>= \s -> put (f s)

-- Tried to rewrite the runState interpreter from https://github.com/natefaubion/purescript-run/blob/v3.0.1/src/Run/State.purs#L111
-- but I'm getting stuck. Probably why I'm looking into smash in the first place, so let's try.

-- runState :: 
--   ∀ s r q a
--   . Row.Cons "state" (STATE s) q r
--   => s 
--   -> Run r a 
--   -> Run q (Tuple s a)
-- runState = loop
--   where
--     handle = on _state Left Right
--     loop s r = case peel r of
--       Left variant -> case handle variant of
--         Left _ -> pure $ Tuple s unit
--         -- Left (Get return)       -> loop (return s) r
--       --   Left (Put (s' /\ next)) -> loop s' next
--         Right a'                -> pure $ Tuple s a
--       Right a -> pure $ Tuple s a

-- tweak :: forall s k. (s -> k) -> (s -> Tuple s k)
-- tweak return = \s -> s /\ return s 

-- Well actually Smash seems to focus on deriving the pairing from the comonad.
-- Whereas I think I want to define my own pairing... Well actually 


-- main = program # interpret (case_ # on _talk handleTalk)

-- use this approach for interpreter http://dlaing.org/cofun/posts/free_and_cofree.html
-- same as the comonadic ui approach, so it should be usable for streaming syntax.

-- It's strange that the Smash store functions are get and put... 
-- They look quite a lot like Run.State too...
-- https://github.com/paf31/purescript-smash/blob/v3.0.0/src/Data/Smash/Store.purs#L30-L35
-- https://github.com/natefaubion/purescript-run/blob/v3.0.1/src/Run/State.purs#L77-L86
-- So I guess you can say that, Smash derives the Co Comonad Store (i.e. the Monad State)
-- Then its interpreters are the comonads
-- https://github.com/paf31/purescript-smash/blob/v3.0.0/test/Main.purs#L32-L36

-- So if I take a look at Cofree.Store it should look like { getH: :: s /\ k , putH :: s -> k }
-- aka CoStateF

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

-- run :: forall s a r. Run (state :: STATE s) a -> CoState s a -> a
-- run s w = pair (\_ b -> b) w s

put15 :: forall r. Run (state :: STATE Int | r) Int
put15 = do
  put 15
  x <- get
  pure x

main :: Effect Unit
main = do
  -- logShow $ run put15 $ mkCoState 1
  -- logShow $ run (put 10 >>= \x -> get) $ mkCoState 1
  -- logShow $ run get $ mkCoState 1
  pure unit

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