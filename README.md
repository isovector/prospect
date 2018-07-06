# accursed

## Dedication

> It is absolutely necessary, for the peace and safety of mankind, that some of
> earth's dark, dead corners and unplumbed depths be let alone; lest sleeping
> abnormalities wake to resurgent life, and blasphemously surviving nightmares
> squirm and splash out of their black lairs to newer and wider conquests.
>
> H.P. Lovecraft


## Overview

The `Accursed` monad is a thing of true horror. Upon those who are brave enough
to plumb its depths, it bestows the ability to statically explore functions, and
by extension, monads. Such power, however, does not come for free; using
`Accursed` is an implicit pact with the Eldrich horrors. Feckless wanderers into
this territory will be rewarded with naught but terror, madness, and runtime
crashes.


## Usage

`Accursed` is at its core, a free monad with an `Alternative` instance. It
introduces a single primitive, `curse :: Accursed f a`, the forcing of which
corresponds to an `empty` when evaluated in the context of a bind. Extreme care
should be used; if an unevaluated `curse` manages to escape from `Accursed`, you
will find yourself chasing exceptions at runtime.

In all other respects, `Accursed` is equivalent to the `Free` monad from `free`,
and has a corresponding interface.

