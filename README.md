# Haskell Demonstrations

A place to demonstrate concepts for training Haskell developers.

## TypeclassMock.hs

When you need to count on side effects and don't get a return, you may
decide to mock your collaborating typeclasses. This simple demo shows
a couple of ways to do it and hints at how to build up more complex
scenarios.

## FreeMock.hs

The Free type can be used to defer interpretation when you need to
mock or stub functions that are already wired to IO.

## Transformer.hs

You know about the Reader monad and the IO monad but what if you need
both in your program? Monad transormers are a popular way to compose
computational contexts. This demo shows how to make a monad transformer
from scratch.

## BigStack.hs

So you have a bunch of monad transformers in a stack. How do operate on
each context individually? This demo shows how to address any context
in your stack using the `lift` function from `MonadTrans`.
