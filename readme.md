# Building a bulletin board using Haskell, twain and friends

Check out the [blog post](https://gilmi.me/blog/post/2022/04/24/learn-twain-bulletin-app).

This version uses SQLite3 instead of STM!

## Run with


```sh
stack run
```

or

```sh
cabal run
```

## Static executable

To compile a static executable using docker, uncomment the relevant lines in the `stack.yaml` file,
and rebuild with `stack build`.
