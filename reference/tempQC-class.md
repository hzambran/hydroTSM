# Air-temperature quality-control result

Structure, print method, and four-panel summary plot for objects
returned by
[`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md),
[`tempQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/tempQC_subdaily.md),
and [`tempQC`](https://hzambran.github.io/hydroTSM/reference/tempQC.md).

## Usage

``` r
# S3 method for class 'tempQC'
print(x, ...)

# S3 method for class 'tempQC'
plot(
  x, max.stations=20L,
  col=c("#2878B5", "#D55E00", "#E69F00"), ...
)
```

## Arguments

- x:

  An object inheriting from class `"tempQC"`.

- max.stations:

  Maximum number of stations in the affected-station panel.

- col:

  At least three plotting colours.

- ...:

  Additional arguments; the plot method passes them to its final
  time-series panel.

## Details

The plot summarizes station recommendations, missing and
confirmed-suspicious fractions, counts by active test, and the time
distribution of rejected observations. It is diagnostic and does not
alter the result.

## Value

Both methods return `x` invisibly. A `"tempQC"` object is a list with:

- `accepted.metadata` and `discarded.metadata`: station metadata plus
  recommendation diagnostics;

- `accepted.data`: original `zoo` series for stations recommended for
  acceptance;

- `accepted.corrected`: the same accepted stations after the requested
  correction policy;

- `suspicious`: one row per point flagged for review or rejection;

- `corrections`: an audit table of changed values;

- `flags`, `flag.count`, and `rejected`: individual logical flag
  objects, evidence counts, and confirmed decisions;

- `station.summary`: completeness, flag fractions, breakpoint
  diagnostics, recommendation, and reason;

- `breakpoint`: station homogeneity diagnostics;

- `spatial.estimate`, `spatial.score`, and `neighbours`: spatial-test
  diagnostics; and

- `settings`: resolved workflow settings.

## References

Brunet, M. et al. (2020). *Best Practice Guidelines for Climate Data and
Metadata Formatting, Quality Control and Submission*. Copernicus Climate
Change Service.

## See also

[`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md),
[`tempQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/tempQC_subdaily.md),
[`tempQC_tests`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md)
