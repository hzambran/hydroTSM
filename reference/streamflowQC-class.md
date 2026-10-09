# Streamflow quality-control result

Structure, print method, and four-panel summary plot for objects
returned by
[`streamflowQC_daily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_daily.md),
[`streamflowQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_subdaily.md),
and
[`streamflowQC`](https://hzambran.github.io/hydroTSM/reference/streamflowQC.md).

## Usage

``` r
# S3 method for class 'streamflowQC'
print(x, ...)

# S3 method for class 'streamflowQC'
plot(
  x, max.stations=20L,
  col=c("#2878B5", "#D55E00", "#E69F00"), ...
)
```

## Arguments

- x:

  An object inheriting from class `"streamflowQC"`.

- max.stations:

  Maximum number of stations in the affected-station panel.

- col:

  At least three plotting colours.

- ...:

  Additional arguments; the plot method passes them to its final
  time-series panel.

## Details

The plot summarizes station recommendations, missing/review/rejected
fractions, counts by active test, and the time distribution of confirmed
suspicious observations. Plotting is diagnostic and does not alter the
result or write a file unless the caller explicitly opens a file device.

## Value

Both methods return `x` invisibly. A `"streamflowQC"` object is a list
with:

- `accepted.metadata` and `discarded.metadata`: original station
  metadata plus completeness, evidence, homogeneity, recommendation, and
  reason fields;

- `accepted.data` and `discarded.data`: original `zoo` series split by
  the station recommendation;

- `accepted.corrected` and `discarded.corrected`: the same partitions
  after the requested correction policy;

- `suspicious`: one row per point flagged for review or rejection,
  including all contributing test names and any spatial estimate;

- `corrections`: an audit table containing every changed value;

- `flags`, `flag.count`, and `rejected`: individual logical flag
  objects, evidence counts, and confirmed decisions;

- `station.summary`: station completeness, review/rejected fractions,
  breakpoint diagnostics, recommendation, and reason;

- `breakpoint`: annual-indicator Pettitt-test diagnostics;

- `spatial.estimate`, `spatial.score`, `spatial.support`, and
  `neighbours`: spatial-test diagnostics;

- `highflow` and `return.period`: component high-flow flags, GEV
  parameters, and estimated return periods; and

- `settings`: all resolved workflow controls.

## References

Fileni, F. et al. (2026). UK-Flow15-QC: A quality control framework for
better river flow data in hydrological research. *EGUsphere preprint*.
doi:10.5194/egusphere-2026-277.

## See also

[`streamflowQC_daily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_daily.md),
[`streamflowQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_subdaily.md),
[`streamflowQC_tests`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md)
