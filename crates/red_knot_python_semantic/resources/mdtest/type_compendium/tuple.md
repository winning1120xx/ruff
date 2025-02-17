# Tuples

## Tuples as product types

Tuples can be used to construct product types. Inhabitants of the type `tuple[P, Q]` are ordered
pairs `(p, q)` where `p` is an inhabitant of `P` and `q` is an inhabitant of `Q`, analogous to the
Cartesian product of sets.

```py
from typing_extensions import assert_type

class P: ...
class Q: ...

def _(p: P, q: Q):
    assert_type((p, q), tuple[P, Q])
```

## The empty tuple

The type of the empty tuple `()` is spelled `tuple[()]`. It is *not* a singleton type, as it can be
subclassed:

```py
from knot_extensions import static_assert, is_singleton, is_subtype_of, is_equivalent_to, is_assignable_to

static_assert(not is_singleton(tuple[()]))

# TODO: This should not be an error
# error: [invalid-base]
class AnotherEmptyTuple(tuple[()]): ...

static_assert(not is_equivalent_to(AnotherEmptyTuple, tuple[()]))

# TODO: These should not be errors
# error: [static-assert-error]
static_assert(is_subtype_of(AnotherEmptyTuple, tuple[()]))
# error: [static-assert-error]
static_assert(is_assignable_to(AnotherEmptyTuple, tuple[()]))
```
