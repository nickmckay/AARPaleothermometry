# Build a TOC-depth model for bacterial resetting

Creates a linear-interpolation function for total organic carbon (TOC)
as a function of depth. The interpolant is used inside
[`racemize_one_depth`](https://nickmckay.github.io/AARP/reference/racemize_one_depth.md)
to scale the bacterial resetting term at each burial depth during time
integration.

## Usage

``` r
make_toc_model(depth_cm, toc_pct)
```

## Arguments

- depth_cm:

  Numeric vector of measurement depths in cm. Must start at 0 and be
  monotonically increasing.

- toc_pct:

  Numeric vector of TOC values (percent dry weight) at each depth. Same
  length as `depth_cm`.

## Value

A function `f(z_cm)` that returns TOC (percent) at any depth via linear
interpolation. Values beyond the data range are held constant at the
nearest endpoint (rule = 2).

## See also

[`racemize_one_depth`](https://nickmckay.github.io/AARP/reference/racemize_one_depth.md)
for how `toc_model` and `f_bac` are used in the forward model.

## Examples

``` r
toc_fn <- make_toc_model(depth_cm = c(0, 100, 300, 500),
                         toc_pct  = c(5,   3,   1, 0.5))
toc_fn(150)  # interpolated TOC at 150 cm
#> [1] 2.5
```
