# Temporal and spatial quality control in hydroTSM

## 1 Purpose and scope

This vignette summarises the quality-control (QC) tests implemented by
the frequency-aware
[`precipQC()`](https://hzambran.github.io/hydroTSM/reference/precipQC.md),
[`tempQC()`](https://hzambran.github.io/hydroTSM/reference/tempQC.md),
and
[`streamflowQC()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC.md)
workflows in **hydroTSM**. Each wrapper detects the input frequency with
[`sfreq()`](https://hzambran.github.io/hydroTSM/reference/sfreq.md) and
dispatches daily data to its `_daily()` function and minute or hourly
data to its `_subdaily()` function. The input must be a univariate or
multivariate numeric `zoo` object with one uniquely named station per
column.

The workflows are deliberately conservative. A statistical anomaly is
not automatically treated as an error: individual tests produce
auditable flags, the evidence-combination stage classifies observations
as **review** or **reject**, and a separate station-level decision
recommends **accept** or **discard**. Every test can be activated or
deactivated through the named `checks` argument. Spatial QC is enabled
by default only when `metadata` is provided, although users may
explicitly enable correlation-only spatial QC without coordinates.

> **In-memory contract**
>
> The QC functions do not write files. They return flags,
> suspicious-point tables, station summaries, accepted/discarded
> metadata, accepted `zoo` series, corrected accepted `zoo` series,
> spatial diagnostics when available, and all resolved settings.

## 2 Workflow map

In the following table, **Yes** means that the test is active by
default, **Metadata** means that it defaults to active only when station
metadata are supplied, **Adjunct** means that a separate function is
available but is not called automatically, and an em dash means that the
workflow does not apply that test.

| Test family | Precipitation daily | Precipitation sub-daily | Temperature daily | Temperature sub-daily | Streamflow daily | Streamflow sub-daily |
|----|---:|---:|---:|---:|---:|---:|
| Index, frequency, and station-name validation | Yes | Yes | Yes | Yes | Yes | Yes |
| Physical range | Yes | Yes | Yes | Yes | Yes | Yes |
| Copied month/year blocks | Yes | — | Yes | — | Yes | — |
| Clustered frequent values | Yes | — | — | — | — | — |
| Distribution gap | Yes | — | — | — | — | — |
| Missing-interval gap | — | — | — | — | Yes | Yes |
| Seasonal climatology | Yes | Yes | Yes | Yes | Yes | Yes |
| Persistence or flat line | Yes | Yes | Yes | Yes | Yes | Yes |
| Isolated spike/dip | — | — | Yes | Yes | Yes | Yes |
| Rate of change | — | — | Yes | Yes | Yes | Yes |
| Rapid fluctuation | — | — | — | — | — | Yes |
| Misplaced accumulation | Yes | Yes | — | — | — | — |
| Systematic weekday false zero | Yes | Yes | — | — | — | — |
| Extreme/high-flow diagnostics | Climatology | Climatology | — | — | Yes | Yes |
| Spatial consistency/regression | Metadata | Metadata | Metadata | Metadata | Metadata | Metadata |
| Spatially inconsistent dry spell | Yes | Yes | — | — | — | — |
| Homogeneity breakpoint | Yes | Yes | Yes | Yes | Yes | Yes |
| Paired Tmin/Tmax consistency | — | — | Adjunct | Adjunct | — | — |

## 3 Shared validation, metadata, and decision rules

### 3.1 Structural and temporal validation

All workflows reject non-numeric data, duplicated or non-increasing
timestamps, empty or duplicated station names, unsupported index
classes, and incompatible frequencies. Daily workflows accept `Date` or
`POSIXt` indices and calculate completeness across the calendar span.
Sub-daily workflows require a `POSIXt` index; gaps are allowed only when
their duration is an integer multiple of the modal interval. The
optional `from` and `to` arguments subset the in-memory record before
completeness, thresholds, spatial models, and station decisions are
calculated.

### 3.2 Station metadata and incomplete coordinates

Metadata are supplied as a data frame. Users identify the station,
longitude, latitude, and optional elevation fields with `station.id`,
`coords`, and `elevation`; streamflow may additionally identify a
catchment-area field with `area`. Rows are matched to `colnames(x)`, so
their incoming order is irrelevant. A station is geographically located
only when both longitude and latitude are finite. Missing either
coordinate does not disable QC:

- a located target applies the distance filter and prefers eligible
  located neighbours;
- an unlocated target uses deterministic correlation-based candidate
  screening;
- an unlocated candidate remains a conservative fallback for a located
  target;
- optional elevation similarity reduces the suitability and weight of
  stations with very different heights; and
- streamflow area metadata allow regressions in specific-discharge space
  before estimates are converted back to the target station’s units.

### 3.3 Spatial scalability

Spatial tests use compact target-specific candidate lists rather than
dense station-by-station distance matrices. `max.candidates=200L` bounds
the pool retained for exact selection, and `selection.samples=2048L`
bounds the deterministic, evenly spaced time sample used for initial
neighbour screening. Final estimates and flags always use the full
record. Setting either control to `Inf` requests the corresponding
exhaustive behaviour. Work can be distributed with `ncores`,
`parallel.type` (`"serial"`, `"socket"`, or `"fork"`), and optional
`cluster.nodes`.

### 3.4 Evidence and station recommendations

The workflows distinguish **hard** tests from tests that normally
require corroboration:

- precipitation daily treats range, copied blocks, clustered frequent
  values, distribution gaps, persistence, misplaced accumulation, and
  spatially corroborated dry spells as hard evidence; sub-daily
  precipitation treats range, persistence, daily/monthly accumulation,
  and dry spells as hard;
- temperature daily treats range, copied blocks, persistence, and spikes
  as hard; sub-daily temperature treats range, persistence, and spikes
  as hard;
- streamflow daily treats range and copied blocks as hard, while
  sub-daily streamflow treats only range as hard because real operations
  and flashy catchments can resemble statistical errors.

Non-hard flags are confirmed when at least `min.evidence` independent
test families agree. Station recommendations additionally use missing,
review, confirmed-suspicious, record-duration, and optional breakpoint
criteria. The default correction is `"none"`; `"set_na"` removes
confirmed points, and `"spatial"` substitutes an available spatial
estimate and otherwise uses `NA`.

## 4 Precipitation QC

The precipitation design combines gross-error, temporal, climatological,
and neighbour-consistency checks used in operational rain-gauge QC
([Hamada et al. 2011](#ref-hamada2011); [Scherrer et al.
2011](#ref-scherrer2011); [Lewis et al. 2021](#ref-lewis2021); [El
Hachem et al. 2022](#ref-elhachem2022)).

### 4.1 Daily precipitation

| Check key and function | Default behaviour | What is flagged |
|----|----|----|
| `range` — [`precipQC_range()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | `lower=0`, `upper=1825` mm d(^{-1}) | Negative values or values above the configured physical limit. The upper default is a conservative 24-hour record screen, not a regional design threshold. |
| `duplicate` — [`precipQC_duplicate()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | At least 20 paired month positions or 300 paired year positions, with at least three wet values | Entire calendar months or years copied exactly at corresponding positions ([Durre et al. 2010](#ref-durre2010)). |
| `frequency` — [`precipQC_frequency()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | Consecutive wet reports with 9/10, 8/10, 7/10, or 5/10 identical values, screened by 0.3, 0.5, 0.7, or 0.9 climatological probabilities | Suspicious clustering caused by rounding, coding, or repeated manual entries ([Durre et al. 2010](#ref-durre2010)). |
| `gap` — [`precipQC_gap()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | A gap of at least 300 mm in a calendar-month wet-value distribution | Values in the separated upper tail above the first large distribution gap ([Durre et al. 2010](#ref-durre2010)). |
| `climatology` — [`precipQC_climatology()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | 15-day circular day-of-year window; probability 0.999; robust score 8; at least 30 samples | Upper extremes beyond both an empirical quantile and a robust iterative `log1p` median/MAD limit. |
| `persistence` — [`precipQC_persistence()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | Five identical consecutive values of at least 10 mm d(^{-1}) | Repeated high values consistent with stuck, copied, or artificial reports. |
| `accumulation` — [`precipQC_accumulation()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | Candidate exceeds twice the station mean wet-day depth and follows a missing previous day | A multi-day amount plausibly assigned to one daily timestamp. It is a candidate flag, not proof of accumulation. |
| `weekday` — [`precipQC_weekday()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | At least 20 wet days in a station-year; chi-squared `alpha=0.001`; observed/expected ratio at most 0.5 | Dry reports on a strongly under-recorded weekday, targeting systematic false zeros in manually observed networks ([Estevez et al. 2022](#ref-estevez2022)). |
| `breakpoint` — [`precipQC_breakpoint()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md) | Pettitt tests on annual total, wet-day count, maximum, and extreme-day count; Holm adjustment | A station-level homogeneity change. It is diagnostic and does not discard a station unless `discard.breakpoint=TRUE` ([Stepanek et al. 2009](#ref-stepanek2009)). |

The daily spatial test is applied to rolling 1-, 3-, and 7-day totals by
default. One native-scale neighbour graph is selected with Spearman
correspondence on `log1p` precipitation and reused at every scale. For
each target and time,
[`precipQC_spatial()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md)
calculates an ordinary or distance/elevation-weighted neighbour median.
The critical ratio divides the positive transformed residual by the
larger of simultaneous neighbour dispersion, historical residual
dispersion, and a numerical floor. An accumulation endpoint flag is
expanded to all contributing daily values. Multi-scale testing prevents
an error hidden at one accumulation from being missed at another ([El
Hachem et al. 2022](#ref-elhachem2022)).

[`precipQC_dryspell()`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md)
separately examines default 15-day target dry windows. A window is
flagged when the requested neighbour fraction (default 1) records at
least three wet days. This targets false zeros or missing rainfall that
an upper-outlier test cannot detect ([Lewis et al.
2021](#ref-lewis2021); [Golkhatmi and Farzandi
2024](#ref-golkhatmi2024)).

### 4.2 Sub-daily precipitation

| Check key | Default behaviour and difference from daily QC |
|----|----|
| `range` | The upper limit is `max.rate=401` mm h(^{-1}) multiplied by the detected interval duration; negative depths are invalid ([Lewis et al. 2021](#ref-lewis2021)). |
| `climatology` | Month-specific `log1p` limits use probability 0.999, robust score 8, and at least 100 observations. |
| `persistence` | Two identical high values are suspicious. When no high threshold is supplied, it is twice the station mean wet-day depth. Any identical wet value persisting for 24 hours is also flagged. |
| `daily.accumulation` | A large interval is screened against 23 dry hours before and after it. |
| `monthly.accumulation` | A large interval is screened against 28 preceding dry days and 23 following dry hours. |
| `weekday` | Native values are first aggregated to daily totals only when at least 90% of expected intervals are present; daily weekday flags are expanded back to finite dry native intervals. |
| `breakpoint` | Native precipitation is aggregated to daily totals before the annual multi-indicator Pettitt analysis. |

Spatial consistency is evaluated at the native interval and 1, 3, 6, and
24 hours by default. Positive durations must be exact multiples of the
detected interval. Accumulated flags are expanded back to their
contributing native observations. The same 15-day dry-spell logic is
applied after in-memory daily wet/dry aggregation. Raw tipping-bucket
inter-tip checks cannot be reconstructed from already accumulated `zoo`
values; they require the original tip timestamps ([Villalobos-Herrera et
al. 2022](#ref-villalobos2022)).

## 5 Air-temperature QC

Temperature QC combines physical limits, robust seasonal references,
temporal consistency, spatial regression, and homogeneity diagnostics
([Durre et al. 2010](#ref-durre2010); [Feng et al. 2004](#ref-feng2004);
[Hubbard et al. 2007](#ref-hubbard2007); [Brunet et al.
2020](#ref-brunet2020)).

### 5.1 Daily air temperature

| Check key and function | Default behaviour | What is flagged |
|----|----|----|
| `range` — [`tempQC_range()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md) | (-89.4) to (57.7) degrees C | Values outside global physical screening limits. Regional limits should be supplied when authoritative. |
| `duplicate` — [`tempQC_duplicate()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md) | At least 20 paired month or 300 paired year positions | Exactly copied complete months or years ([Durre et al. 2010](#ref-durre2010)). |
| `climatology` — [`tempQC_climatology()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md) | Circular 15-day day-of-year window; robust absolute score 6; at least 100 samples | Values far from the local seasonal median relative to MAD/IQR dispersion ([Feng et al. 2004](#ref-feng2004); [Durre et al. 2010](#ref-durre2010)). |
| `persistence` — [`tempQC_persistence()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md) | Seven observations with range no greater than zero | Exact flat lines consistent with a stuck sensor. `persistence.tolerance` can admit near-flat lines. |
| `step` — [`tempQC_step()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md) | Month-specific first differences; robust score 3; at least 20 differences | Implausible consecutive-day changes; optional absolute rise/fall limits may also be imposed ([Hubbard et al. 2007](#ref-hubbard2007)). |
| `spike` — [`tempQC_spike()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md) | Centre value at least 25 degrees from both neighbours in the same direction | An isolated spike or dip ([Durre et al. 2010](#ref-durre2010)). |
| `breakpoint` — [`tempQC_breakpoint()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md) | Pettitt tests on sufficiently complete annual means and standard deviations; at least 10 years; Holm-adjusted `alpha=0.05`; shift at least 1 degree | A station-level homogeneity change, diagnostic by default ([Boulanger et al. 2010](#ref-boulanger2010); [Brunet et al. 2020](#ref-brunet2020)). |

[`tempQC_spatial()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md)
selects up to five neighbours using overall Pearson correlation and
proximity-adjusted target-on-neighbour regression error. It fits
separate monthly regressions, combines their predictions with
geographic/elevation proximity and inverse-error-variance weights, and
flags a target only when both the absolute residual (default 8 degrees)
and the standardised residual (default 4) are large. Pairwise intercepts
estimate persistent station offsets without imposing a universal lapse
rate. This is a resistant spatial-regression-test implementation for
complex terrain ([Hubbard et al. 2007](#ref-hubbard2007); [Estevez et
al. 2018](#ref-estevez2018); [Xiong et al. 2022](#ref-xiong2022),
[2024](#ref-xiong2024)).

### 5.2 Sub-daily air temperature

| Check key | Default behaviour and difference from daily QC |
|----|----|
| `range` | Uses the same configurable physical limits as daily temperature. |
| `climatology` | References are conditioned jointly on calendar month and hour; the robust score is 6 with at least 30 samples. |
| `persistence` | A range no greater than 0.05 degrees across a default three-hour duration is flagged; the observation count is derived from the detected interval ([Beele et al. 2022](#ref-beele2022)). |
| `step` | Month-hour first differences use robust score 5 and at least 30 samples. |
| `spike` | The isolated spike/dip threshold is 12 degrees. |
| `spatial` | Regressions are fitted separately by month and hour, with minimum absolute residual 5 degrees, standardised residual 4, and preferred correlation at least 0.7 ([Cheng et al. 2016](#ref-cheng2016)). |
| `breakpoint` | Native observations are aggregated to daily means before annual homogeneity analysis. |

When paired Tmin and Tmax series are available, the adjunct
[`tempQC_internal()`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md)
flags Tmin greater than Tmax, possible interchange relative to the
previous/current/following-day window, and excessive lagged separation.
This test is intentionally separate because a single generic temperature
matrix cannot reveal which member of an inconsistent Tmin/Tmax pair is
wrong.

## 6 Streamflow QC

Streamflow QC is cautious about hydrologically real extremes,
intermittent zeros, regulation, tidal behaviour, and rapidly responding
catchments. It therefore keeps physical impossibilities separate from
statistical evidence and combines correlated high-flow diagnostics into
one evidence family ([Gudmundsson et al. 2018](#ref-gudmundsson2018);
[Crochemore et al. 2020](#ref-crochemore2020); [U.S. Integrated Ocean
Observing System 2018](#ref-ioos2018)).

### 6.1 Daily streamflow

| Check key and function | Default behaviour | What is flagged |
|----|----|----|
| `range` — [`streamflowQC_range()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | `lower=0`, `upper=Inf` | Negative/non-finite non-missing discharge and optional station-specific upper-limit violations. |
| `duplicate` — [`streamflowQC_duplicate()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | At least 20 paired month or 300 paired year positions and one positive value | Exactly copied calendar blocks; all-zero intermittent-flow blocks are protected by default. |
| `gap` — [`streamflowQC_gap()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | Expected interval one day | The first finite observation after a missing value or missing calendar interval. |
| `climatology` — [`streamflowQC_climatology()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | `log(Q + 0.01)`; circular five-day window; mean plus/minus six standard deviations; at least 30 samples | Seasonally implausible low or high flow following the GSIM structure ([Gudmundsson et al. 2018](#ref-gudmundsson2018)). |
| `flatline` — [`streamflowQC_flatline()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | More than 10 equal positive daily values (`run=11`); zeros protected | Extended constant or near-constant flow. |
| `spike` — [`streamflowQC_spike()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | Factor-five reversal or change larger than the station 0.99 quantile | An isolated peak or dip. |
| `rate` — [`streamflowQC_rate()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | Monthly robust log-difference score 6, optional absolute limits, and factor-five abrupt fall | Implausible consecutive changes. |
| `highflow` — [`streamflowQC_highflow()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | Global log-flow score 6; largest/second-largest AMAX ratio 2; GEV return period 1000 years after at least 10 annual maxima | Exceptionally high flows. Component flags and return-period diagnostics are retained, but they count as one evidence family. |
| `breakpoint` — [`streamflowQC_breakpoint()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | Pettitt tests on annual mean, standard deviation, minimum, and maximum; at least 10 years; Holm `alpha=0.01`; relative change at least 0.5 | A station-level change that may indicate measurement change, regulation, or catchment change. |

The daily spatial test selects up to five neighbours using Spearman
correlation on log flow plus optional distance/elevation proximity. With
area metadata, target-on-neighbour models operate on specific discharge.
A target event must exceed its 0.99 quantile, have a spatial residual
score of at least 3, and lack the required neighbour support (default
one neighbour above its 0.95 quantile) within three days before or after
the event. Thus a genuine regional flood is corroborated rather than
rejected solely because it is extreme.

### 6.2 Sub-daily streamflow

| Check key | Default behaviour and difference from daily QC |
|----|----|
| `range` | Uses the same non-negative physical rule and optional upper limit. |
| `gap` | The expected interval is the detected modal interval in seconds. |
| `climatology` | Month-specific `log(Q + 0.01)` limits use score 6 and at least 100 observations. |
| `flatline` | Ordinary flat lines use 168 hours; high-flow flat lines above the 0.99 quantile use 24 hours. Durations are converted to observation counts. |
| `spike` | Uses the same factor-five/0.99-quantile isolated-reversal logic. |
| `rate` | Monthly log-difference score 6, optional absolute limits, and factor-five abrupt fall. |
| `fluctuation` — [`streamflowQC_fluctuation()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md) | More than eight derivative sign reversals in 16 observations after suppressing changes below the 0.05 absolute-difference quantile |
| `highflow` | Native values feed the same log-flow, AMAX-ratio, and GEV diagnostics. |
| `breakpoint` | Native values are aggregated to daily means before annual Pettitt tests. |

Sub-daily spatial event support uses a default 72-hour window rather
than a fixed observation count. The target and neighbour quantiles,
regression score, specific-discharge option, and conservative
lack-of-support rule otherwise match the daily formulation. These rules
generalise operational sub-daily flow QC to the actual modal interval
([Fileni et al. 2023](#ref-fileni2023); [Zhao et al.
2018](#ref-zhao2018)).

## 7 Activating and deactivating tests

Names omitted from `checks` retain their workflow defaults. This allows
a small override rather than restating the complete vector:

``` r

# Keep daily precipitation defaults but disable weekday and breakpoint tests.
p.qc <- precipQC_daily(
  p, metadata=stations,
  checks=c(weekday=FALSE, breakpoint=FALSE)
)

# Explicitly enable correlation-only spatial temperature QC without metadata.
t.qc <- tempQC_subdaily(
  temperature,
  checks=c(spatial=TRUE),
  ncores=4, parallel.type="socket"
)

# Retain streamflow temporal tests but disable high-flow and spatial evidence.
q.qc <- streamflowQC(
  discharge,
  checks=c(highflow=FALSE, spatial=FALSE)
)
```

Thresholds should be adapted when authoritative regional limits,
instrument resolution, catchment behaviour, or network operating
practices are known. The defaults are screening values grounded in the
cited literature; they are not universal proof that an observation is
wrong.

## 8 Returned audit trail

The three result classes share a common audit structure:

- `accepted.metadata` and `discarded.metadata` preserve user metadata;
- `accepted.data` contains accepted station series without modification;
- `corrected.data` applies the requested correction only to accepted
  stations;
- `flags` retains every enabled point-level test separately;
- `suspicious` lists station, timestamp, value, contributing tests,
  evidence, and action;
- `station.summary` records completeness, review/rejection fractions,
  record length, breakpoint evidence, and recommendation;
- spatial estimates, scores, neighbours, and streamflow event-support
  counts remain available when spatial QC ran; and
- `settings` records the resolved thresholds, subset bounds, and
  parallel backend for reproducibility.

Plots from
[`plot.precipQC()`](https://hzambran.github.io/hydroTSM/reference/precipQC-class.md),
[`plot.tempQC()`](https://hzambran.github.io/hydroTSM/reference/tempQC-class.md),
and
[`plot.streamflowQC()`](https://hzambran.github.io/hydroTSM/reference/streamflowQC-class.md)
summarise station recommendations, the most affected stations, flags by
test, and the timing of confirmed suspicious observations.

## 9 Limitations

These workflows evaluate the `zoo` observations and metadata supplied to
them. They cannot reconstruct logger diagnostics, raw tipping-bucket tip
times, rating-curve revisions, maintenance logs, observer notes, or
upstream operations that are absent from those inputs. Spatial agreement
is also not independence: common radar adjustments, shared telemetry,
regional storms, or basin-wide regulation can affect several stations
together. Final rejection and correction decisions should therefore
retain the audit trail and, for high-impact records, include expert
review.

Beele, E., M. Reyniers, R. Aerts, and B. Somers. 2022. “Quality Control
and Correction Method for Air Temperature Data from a Citizen Science
Weather Station Network in Leuven, Belgium.” *Earth System Science Data*
14: 4681–717. <https://doi.org/10.5194/essd-14-4681-2022>.

Boulanger, J.-P., J. Aizpuru, L. Leggieri, and M. Marino. 2010. “A
Procedure for Automated Quality Control and Homogenization of Historical
Daily Temperature and Precipitation Data (APACH): Part 1.” *Climatic
Change* 98: 471–91.

Brunet, M. et al. 2020. *Best Practice Guidelines for Climate Data and
Metadata Formatting, Quality Control and Submission*. Copernicus Climate
Change Service.

Cheng, A. R., T. H. Lee, H. I. Ku, and Y. W. Chen. 2016. “Quality
Control Program for Real-Time Hourly Temperature Observation in Taiwan.”
*Journal of Atmospheric and Oceanic Technology* 33: 953–65.

Crochemore, L. et al. 2020. “Lessons Learnt from Checking the Quality of
Openly Accessible River Flow Data Worldwide.” *Hydrological Sciences
Journal* 65: 699–711. <https://doi.org/10.1080/02626667.2019.1659509>.

Durre, I., M. J. Menne, B. E. Gleason, T. G. Houston, and R. S. Vose.
2010. “Comprehensive Automated Quality Assurance of Daily Surface
Observations.” *Journal of Applied Meteorology and Climatology* 49:
1615–33.

El Hachem, A. et al. 2022. “Space-Time Statistical Quality Control of
Extreme Precipitation Observations.” *Hydrology and Earth System
Sciences* 26: 6137–46. <https://doi.org/10.5194/hess-26-6137-2022>.

Estevez, J. et al. 2022. “A Quality Control Procedure for Long-Term
Series of Daily Precipitation Data in a Semiarid Environment.”
*Theoretical and Applied Climatology* 149: 1029–41.

Estevez, J., P. Gavilan, and A. P. Garcia-Marin. 2018. “Spatial
Regression Test for Ensuring Temperature Data Quality in Southern
Spain.” *Theoretical and Applied Climatology* 131: 309–18.

Feng, S., Q. Hu, and W. Qian. 2004. “Quality Control of Daily
Meteorological Data in China, 1951–2000.” *International Journal of
Climatology* 24: 853–70.

Fileni, F., H. J. Fowler, E. Lewis, F. McLay, and L. Yang. 2023. “A
Quality-Control Framework for Sub-Daily Flow and Level Data for
Hydrological Modelling in Great Britain.” *Hydrology Research* 54:
1357–67. <https://doi.org/10.2166/nh.2023.045>.

Golkhatmi, N. S. N., and M. Farzandi. 2024. “Enhancing Rainfall Data
Consistency and Completeness: A Spatiotemporal Quality Control Approach
and Missing Data Reconstruction Using MICE on Large Precipitation
Datasets.” *Water Resources Management* 38: 815–33.

Gudmundsson, L. et al. 2018. “The Global Streamflow Indices and Metadata
Archive (GSIM) – Part 2: Quality Control, Time-Series Indices and
Homogeneity Assessment.” *Earth System Science Data* 10: 787–804.
<https://doi.org/10.5194/essd-10-787-2018>.

Hamada, A., O. Arakawa, and A. Yatagai. 2011. “An Automated Quality
Control Method for Daily Rain-Gauge Data.” *Global Environmental
Research* 15: 183–92.

Hubbard, K. G., N. B. Guttman, J. You, and Z. Chen. 2007. “An Improved
QC Process for Temperature in the Daily Cooperative Weather
Observations.” *Journal of Atmospheric and Oceanic Technology* 24:
206–13.

Lewis, E. et al. 2021. “Quality Control of a Global Hourly Rainfall
Dataset.” *Environmental Modelling and Software* 144: 105169.

Scherrer, S. C. et al. 2011. “Operational Quality Control of Daily
Precipitation Using Spatio-Climatological Plausibility Testing.”
*Meteorologische Zeitschrift* 20: 397–407.

Stepanek, P., P. Zahradnicek, and P. Skalak. 2009. “Data Quality Control
and Homogenization of Air Temperature and Precipitation Series in the
Area of the Czech Republic in the Period 1961–2007.” *Advances in
Science and Research* 3: 23–26.

U.S. Integrated Ocean Observing System. 2018. *Manual for Real-Time
Quality Control of Stream Flow Data, Version 1.0*. U.S. IOOS.
<https://doi.org/10.25923/gszc-ha43>.

Villalobos-Herrera, R. et al. 2022. “Sub-Hourly Resolution Quality
Control of Rain-Gauge Data Significantly Improves Regional Sub-Daily
Return Level Estimates.” *Quarterly Journal of the Royal Meteorological
Society*.

Xiong, X. et al. 2022. “Research on Quality Control Methods for Surface
Temperature Observations via Spatial Correlation Analysis.”
*International Journal of Climatology*, ahead of print.
<https://doi.org/10.1002/joc.7897>.

Xiong, X. et al. 2024. “Research on Quality Control Method of Surface
Temperature Observations for Complex Physical Geography.” *Journal of
Atmospheric and Oceanic Technology* 41: 803–15.

Zhao, Q. et al. 2018. “Research on the Data-Driven Quality Control
Method of Hydrological Time Series Data.” *Water* 10: 1712.
<https://doi.org/10.3390/w10121712>.
