# Pallet exoscale Provider

A [pallet](http://palletops.com/) provider for [exoscale](https://exoscale.ch/).

The exoscale provider uses the `:exoscale` key.

## Usage

Add the following to your dependencies:

```clj
[ch.exoscale/pallet-exoscale "0.1.0"]
```

## Caveats

This is an early release expect glitches.
Matching on template names need to be tested, so far converging clusters works
with hardcode `hardware-id` and `image-id` fields.

## Thanks

Considerable input from Hugo Duncan

## License

Copyright © 2013 exoscale

All rights reserved.
