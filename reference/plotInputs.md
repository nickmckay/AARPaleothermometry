# Plot temperature inputs for a damped sediment record

Convenience plotting function for the output of
[`dampSedTemps`](https://nickmckay.github.io/AARP/reference/dampSedTemps.md).
Pivots the data frame to long format and draws air and sediment
temperature histories.

## Usage

``` r
plotInputs(dampened)
```

## Arguments

- dampened:

  Data frame returned by
  [`dampSedTemps`](https://nickmckay.github.io/AARP/reference/dampSedTemps.md).

## Value

A ggplot object.
