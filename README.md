# Simple Hindley-Milner Type Inference

A port of [github.com/kritzcreek/fby19](https://github.com/kritzcreek/fby19) into Rust (with a small fix in the infer function).
It's not very efficient as it's pretty much a direct translation from Haskell (also to keep it simple).

## How to run

```shell
> cargo run -- file

# or 
> echo <expr> | cargo run

# like

> echo "let c = 1 in let f = \x -> x c in f" | cargo run
```
