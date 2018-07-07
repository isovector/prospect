# prospect

## Dedication

> It is absolutely necessary, for the peace and safety of mankind, that some of
> earth's dark, dead corners and unplumbed depths be let alone; lest sleeping
> abnormalities wake to resurgent life, and blasphemously surviving nightmares
> squirm and splash out of their black lairs to newer and wider conquests.
>
> H.P. Lovecraft


## Overview

`prospect` is a library that provides primitives for exploring functions, and by
extension, monads. As such, it allows for a best-attempt static analysis of free
monads. Such power, however, does not come for free; using `prospect` is an
implicit promise with the Eldrich horrors that you'll tread lightly. Feckless
wanderers into these depths will be rewarded with naught but terror, madness,
and runtime crashes.


## Usage

The library provides a function, `prospect :: Free f a -> (Maybe a, [f ()])`,
which can probe the depths of a free monad, finding as many `f` constructors as
it can before the monad branches dynamically.

Be careful when inspecting the `f ()`s, if any of them depend on variables bound
in the monad, they will leak exceptions when you are least expecting them. It's
a good idea to run your `f ()`s through `ensure :: Alternative m => a -> m a`
after you've scrutinized their constructors.


## Example

`prospect` can be used to perform a best-effort static analysis of a free monad:

```haskell
data Pattern a
  = Cont (Bool -> a)
  | Action Int a
  deriving (Functor, Generic1)


cont :: MonadFree Pattern m => m Bool
cont = liftF $ Cont id


action :: MonadFree Pattern m => Int -> m ()
action i = liftF $ Action i ()


success :: (Maybe String, [Pattern ()]
success = prospect $ do
  a <- cont
  action 1
  pure "success"
-- success = (Just "success", [Cont (const ()), Action 1 ()])


failure :: (Maybe String, [Pattern ()]
failure = prospect $ do
  a <- cont
  action 1
  if a  -- static analysis ends here, as it would require branching on the
        -- result of a monadic action
    then action 2
    else action 3
  action 4
  pure "failure"
-- failure = (Nothing, [Cont (const ()), Action 1 ()])
```

In these examples, we can continue analyzing a `Free Pattern` monad until
the result of its `Cont` continuation is forced.

