module Control.Monad.Update.State where

import Control.Monad.State (State, StateT(..), get, modify_)
import Control.Monad.State.Class (class MonadState)
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..), snd)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, ($), (<$>), (<>))

import Data.Monoid.Action (class Action, act)
import Control.Monad.Update (class MonadUpdate)


-- data UpdateState w s a = UpdateState (State (Tuple w s) a)

-- runUpdateState :: forall m s a . UpdateState m s a -> (State (Tuple m s) a)
-- runUpdateState (UpdateState s) = s

-- derive instance functorUpdateState :: Functor (UpdateState w s)
-- instance applyUpdateState :: Apply (UpdateState w s) where 
--   apply (UpdateState f) (UpdateState a) = UpdateState $ apply f a 

-- instance applicativeUpdateState :: Applicative (UpdateState w s) where
--   pure = UpdateState <$> pure
  
-- instance bindUpdateState :: Bind (UpdateState w s) where
--   -- bind :: m a → (a → m b) → m b
--   bind (UpdateState a) f = UpdateState $
--     do
--       let 
--         -- g :: forall a w s. a -> State (Tuple w s) a
--         g x = do
--           (a :: ?t) <- f x 
--           pure a
--       pure $ g ?a

-- instance monadUpdateState :: Monad (UpdateState w s)

-- -- instance stateUpdateState :: MonadState s (UpdateState w s) where
-- --   -- state :: forall a. (s -> (Tuple a s)) -> m a
-- --   -- state :: forall a. ((Tuple w s) -> (Tuple a (Tuple w s))) -> UpdateState w s a
-- --   -- state :: forall a
-- --   --       .              ((Tuple w s) ->          (Tuple a (Tuple w s))) 
-- --   --       -> UpdateState ((Tuple w s) -> Identity (Tuple a (Tuple w s)))
-- --   state f = UpdateState $ StateT (\(Tuple w s) -> Identity $ tweak (f s) w)
-- --     where 
-- --       tweak (Tuple a s) w = Tuple a (Tuple w s)
-- --   -- state f = UpdateState $ StateT (\s -> Identity $ f s)

-- instance stateUpdateState :: MonadState (Tuple w s) (UpdateState w s) where
--   -- state :: forall a. (s -> (Tuple a s)) -> m a
--   -- state :: forall a. ((Tuple w s) -> (Tuple a (Tuple w s))) -> UpdateState w s a
--   -- state :: forall a
--   --       .              ((Tuple w s) ->          (Tuple a (Tuple w s))) 
--   --       -> UpdateState ((Tuple w s) -> Identity (Tuple a (Tuple w s)))
--   state f = UpdateState $ StateT (\s -> Identity $ (f s))
--     -- where 
--     --   tweak (Tuple a s) w = Tuple a (Tuple w s)
--   -- state f = UpdateState $ StateT (\s -> Identity $ f s)


-- -- g :: s -> Identity (Tuple a s)
-- -- g :: (Tuple w s) -> Identity (Tuple a (Tuple w s))
-- -- f :: s -> Tuple a s
-- -- f :: (Tuple w s) -> Tuple a (Tuple w s)

-- -- data Update w s a = Update (s -> (Tuple w a))
-- -- data State s a    = State  (s -> (Tuple s a))

-- -- get :: forall w s a. Update w s a
-- get = state \s -> Tuple w s
--   where 
--     state f = Update f

instance monadUpdatewithState :: Action w s => MonadUpdate (UpdateState w s) w s where
  -- putAction :: w -> m Unit
  -- modify_ :: forall s m. MonadState s m => (s -> s) -> m Unit
  -- putAction w' = modify_ (\(Tuple w s) -> Tuple (w <> w') (act w' s))
  putAction w' = modify_ (\(Tuple w s) -> Tuple (w <> w') (act w' s))
  -- getState :: m s
  getState = snd <$> get
  