# Troupe p2p relay in Go 

This example is adapted from libp2p's Go relay. 

## How to compile

1. Install Go 
2. Run `go build`

## How to run

This program accepts an optional key argument provided via `--key` flag. Sample usage

```
> ./troupe-p2p-runtime --key=mykey.priv
```

If no key argument is provided, a fresh key is auto-generated.
