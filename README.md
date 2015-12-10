# Odds [![Build Status][build image]][build link]

[build image]: https://travis-ci.org/odds-lang/odds.svg?branch=master
[build link]: https://travis-ci.org/odds-lang/odds

The repository for our PLT project, Fall 2015.

Contributors:
- Alexandra Medway (afm2134)
- Alex Kalicki (avk2116)
- Danny Echickson (dje2125)
- Lilly Wang (lfw2114)


Constrain System will not constrain:

1) Function parameters if they are passed to a function that is itself a
parameter prior to that function's args being constrained. For example:
```ocaml 
do call = (f, x) ->
  do f(x)
  do f(2)
  return (* ... *)
```
`x` will not be constrained to a `Num` even though it ought to be. The second 
parameter of `call` will be `Any`.

2) The elements of a list if they are all unconstrained when the list is 
instantiated (i.e. they are all parameters), and a constrained type is cons'd
to the list. For example:
```ocaml
do foo = (x, y) ->
    do bar = [x, y]
    do cons(42, bar)
    return (* ... *)
```
Neither `x` nor `y` will be constrained to `Num`. Both parameters of `foo` will
be `Any`.
