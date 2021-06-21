# Work in Progress - Purescript Update Monad Free

Experiments with the Update Monad and Free functors.

Attempts to write the Update Monad as a Free monad of some base functor to be determined. Starting by getting familiar with the State monad as:

```purescript
data StateF s k
  = Get (s -> k)  -- there's a put/get adjunction going on. (->) -| (,)
  | Put (s /\ k) 

type State s = Free (StateF s)
```

Also trying the same with the `purescript-run` extensible effect approach.