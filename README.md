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
