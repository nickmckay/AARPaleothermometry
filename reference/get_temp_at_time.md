# Return lake bottom water temperature at one or more times

Retrieves mean annual lake bottom water temperature (°C) at the
requested time(s). Accepts either a constant scalar or a time-varying
data frame.

## Usage

``` r
get_temp_at_time(t_ka, temp_input)
```

## Arguments

- t_ka:

  Numeric vector of times in ka BP at which to evaluate temperature.

- temp_input:

  Temperature specification. Either:

  scalar

  :   A single numeric value; temperature is constant through time.

  data.frame

  :   Columns `age_ka` (ka BP) and `temp_C` (°C). Values are linearly
      interpolated; flat extrapolation beyond the data range.

## Value

Numeric vector of temperatures (°C), same length as `t_ka`.

## Examples

``` r
get_temp_at_time(c(0, 5, 10), temp_input = 5)
#> [1] 5 5 5

temp_df <- data.frame(age_ka = c(0, 10), temp_C = c(5, 8))
get_temp_at_time(c(0, 5, 10), temp_input = temp_df)
#> [1] 5.0 6.5 8.0
```
