# Building a bulletin board using Haskell, twain and friends

Check out the [blog post](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board) for the scotty tutorial.
This version uses [twain](https://hackage.haskell.org/package/twain) instead.

## Run with


```sh
stack build && stack run
```

## Static executable

To compile a static executable using docker, uncomment the relevant lines in the `stack.yaml` file,
and rebuild with `stack build`.
