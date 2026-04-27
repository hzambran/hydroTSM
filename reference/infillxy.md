# Infills NA values

Infill all the missing values (NA) in `x` with the corresponding values
in `sim`.

## Usage

``` r
infillxy(x, ...)
# Default S3 method
infillxy(x, sim, ...)
# S3 method for class 'matrix'
infillxy(x, sim, ...)
# S3 method for class 'data.frame'
infillxy(x, sim, ...)
```

## Arguments

- x:

  numeric, data.frame or matrix in which some values are missing (NA).

- sim:

  numeric, data.frame or matrix, with the same dimension of `x`, which
  contains the values that will be used for infilling the missing (NA)
  values in `x`

- ...:

  further arguments passed to or from other methods.

## Details

It gives as a result an object of the same dimension of `x`, in which
all the NA values were infilled with the corresponding values of `sim`.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Examples

``` r
obs <- c(1, NA, 3, 4, NA, 5)
sim <- rep(2, 6)

## Filling in the missing values in 'x' with the corresponding values in 'sim'
infillxy(x=obs, sim)
#> [1] 1 2 3 4 2 5
```
