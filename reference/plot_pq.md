# Plot precipitation and streamflow time series in the same figure.

Given a time series of precipitation and streamflow, this function plots
the two time series in the same figure, streamflows as a normal time
series and precipitation as bars coming from the upper part of the
plotting window.

## Usage

``` r
plot_pq(p, ...)

# S3 method for class 'zoo'
plot_pq(p, q, ptype=c("original", "monthly"),
                na.fill=c("remove", "linear", "spline"), 
                from=start(p), to=end(p), date.fmt=NULL, tz=NULL,
                main=ifelse(ptype=="original", "Precipitation and Streamflows", 
                            "Monthly Precipitation and Streamflows"),
                xlab=ifelse(ptype=="original", "Time", "Month"), 
                ylab=c("P, [mm]", "Q, [m3/s]"), 
                p.col=ifelse(ptype=="original", "blue", "lightblue"),
                q.col=ifelse(ptype=="original", "black", "blue"), 
                leg.title="", leg.text=c("P", "Q"),
                q.pch=16, q.cex=0.3,
                            
                start.month=1, 
                plot.p.probs=TRUE, p.probs=c(0.25, 0.75), 
                p.alpha=0.8,
                plot.q.probs=TRUE, q.probs=c(0.25, 0.75), 
                q.probs.col="lightskyblue1", q.probs.alpha=0.8, 
                labels=TRUE, labels.cex=0.8,
                labels.p.dy=NULL,
                labels.q.dx=c(rep(-0.2,6), rep(0.2,6)),
                labels.q.dy=rep(median(q, na.rm=TRUE)*1.3, 12),
                
                ...)
```

## Arguments

- p:

  zoo object with precipitation time series, with any time frequency.

- q:

  zoo object with streamflow time series, with any time frequency.

- ptype:

  Character indicating the type of plot to be produced. Valid values
  are:  
  -) original =\> a time series plot with precipitation in the upper
  panel (as bars from the time axis) and streamflows in the lower panel
  (as dotted lines).  
  -) monthly =\> a plot with mean monthly values of precipitation in the
  upper panel (as bars from top to bottom) and streamflows in the lower
  panel (as bars from bottom to up). Quantiles of precipitation and
  streamflows are also plotted depending on the values defined in
  `p.probs` and `q.probs`, respectively.

- na.fill:

  Character indicating how to fill any NA present in `p` or `q`.  
  -) remove =\> NAs are not plotted -) linear =\> NAs are removed by
  linear interpolation, using
  [`na.approx`](https://rdrr.io/pkg/zoo/man/na.approx.html)  
  -) spline =\> NAs are removed by spline interpolation, using
  [`na.spline`](https://rdrr.io/pkg/zoo/man/na.approx.html)

- from:

  Character indicating the starting date for subsetting `p` and `q`. The
  default value corresponds to the date of the first element of `p`  
  It has to be in the format indicated by `date.fmt`.

- to:

  Character indicating the ending date for subsetting `p` and `q`. The
  default value corresponds to the date of the last element of `p`  
  It has to be in the format indicated by `date.fmt`.

- date.fmt:

  character indicating the format in which the dates are stored in
  `from` and `to`, e.g. %Y-%m-%d. See ‘Details’ section in
  [`strptime`](https://rdrr.io/r/base/strptime.html). By default,
  `date.fmt` is missing, and it is automatically set to %Y-%m-%d when
  `time(p)` is `Date` object, and set to %Y-%m-%d %H:%M:%S when `x` is a
  sub-daily zoo object.

- tz:

  character, with the specification of the time zone used for `from`,
  `to`. System-specific (see time zones), but `""` is the current time
  zone, and `"GMT"` is UTC (Universal Time, Coordinated). See
  [`Sys.timezone`](https://rdrr.io/r/base/timezones.html) and
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html).  

  If `tz` is missing (the default), it is automatically set to the time
  zone used in `time(p)`.  

  This argument can be used when working with sub-daily zoo objects to
  force using time zones other than the local time zone for `from` and
  `to`. It should be used with caution, being well aware of the time
  zone of the data. See examples.

- main:

  The main title (on top) of the figure.

- xlab:

  a title for the `x` axis. See
  [`title`](https://rdrr.io/r/graphics/title.html).

- ylab:

  a two-element title for the `y` axis: see
  [`title`](https://rdrr.io/r/graphics/title.html).  
  The first element is used for the right `y` axis (i.e., for
  precipitation). The second element is used for the left `y` axis
  (i.e., for streamflows).

- p.col:

  character, representing the colors to be used for plotting the
  precipitation time series.

- q.col:

  character, representing the colors to be used for plotting the
  streamflow time series.

- leg.title:

  a character string or length-one expression giving a title to be
  placed at the top of the legend.
  [`legend`](https://rdrr.io/r/graphics/legend.html).

- leg.text:

  a two-element character to appear in the legend placed at the bottom
  of the figure.  
  The first element is used for precipitation and the second element is
  used for streamflows.

- q.pch:

  numeric, representing the symbols used for plotting the streamflow
  time series.

- q.cex:

  a numerical vector giving the amount by which plotting characters and
  symbols should be scaled relative to the default. This works as a
  multiple of `par("cex")`. See
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html)

- start.month:

  numeric in \[1:12\] indicating the starting month for the
  monthlycurve. Numeric values in \[1, 12\] represents months in
  \[January, December\]. By default `start.month=1`.

- plot.p.probs:

  logical used to decide whether to show lower and upper uncertainty
  bounds for each one of the 12 monthly precipitation values. By default
  `plot.p.probs=TRUE`.  
  When `plot.p.probs=TRUE` the `p.probs` argument is used to define the
  values of the lower and upper uncertainty bounds.

- p.probs:

  numeric of length 2. It defines the quantile values used to compute
  the lower and upper uncertainty bounds for each one of the 12 monthly
  precipitation values. This uncertainty bounds are drawn as vertical
  lines over the bars used to plot the 12 monthly precipitation
  values.  
  By default `p.probs=c(0.25, 0.75)`, which indicates that the quantiles
  0.25 and 0.75 are used to compute the lower and upper uncertainty
  bounds for each one of the 12 monthly precipitation values. If `p` is
  a (sub)daily zoo object, it is first aggregated into monthly values
  using `mean`, and then the `p.probs` quantiles are computed over all
  the monthly values belonging to a calendar month.

- p.alpha:

  numeric of length 1, with the factor used to modify the opacity of
  `p.col`. Typically in \[0,1\], with 0 indicating a completely
  transparent colour and 1 indicating no transparency.

- plot.q.probs:

  logical used to decide whether to show uncertainty bands around each
  one of the 12 monthly average or median streamflow values. By default
  `plot.q.probs=TRUE`.  
  When `plot.q.probs=TRUE` the `q.probs` argument is used to define the
  values of the lower and upper uncertainty bands.

- q.probs:

  numeric of length 2. It is used to define quantile values used to
  compute the lower and upper uncertainty bands around each one of the
  12 monthly average or median streamflow.  
  If `q` is a (sub)daily zoo object, it is first aggregated into monthly
  values using `FUN`, and then the `q.probs` quantiles are computed over
  all the monthly values belonging to a calendar month..  
  By default `q.probs=c(0.25, 0.75)`, which indicates that the quantiles
  0.25 and 0.75 are used to compute the lower and upper uncertainty
  bounds for each one of the 12 monthly average or median values. If `q`
  is provided and is a (sub)daily zoo object, it is first aggregated
  into monthly values using `FUN`, and then the `q.probs` quantiles are
  computed over all the monthly values belonging to a calendar month.

- q.probs.col:

  character with the color used to plot the uncertainty bands around the
  average or median streamflow values.  

- q.probs.alpha:

  numeric of length 1, with the factor used to modify the opacity of
  `q.probs.col`. Typically in \[0,1\], with 0 indicating a completely
  transparent colour and 1 indicating no transparency.

- labels:

  logical. Should monthly streamflow values to be shown above the
  lines?. By default `labels=TRUE`.

- labels.cex:

  numeric giving the amount by which plotting characters used to show
  the numeric values of monthly streamflow values are scaled relative to
  the default.

- labels.p.dy:

  numeric of length 12 giving the amount of vertical coordinate
  positions that have to be used to vertically shift the labels of
  monthly precipitation values.  
  It is only used when `labels=TRUE`.  
  Lengths smaller than 12 are recycled and larger lengths are not used.

- labels.q.dx:

  numeric of length 12 giving the amount of horizontal coordinate
  positions that have to be used to horizontally shift the labels of
  monthly streamflow values.  
  It is only used when `labels=TRUE`.  
  Lengths smaller than 12 are recycled and larger lengths are not used.

- labels.q.dy:

  numeric of length 12 giving the amount of vertical coordinate
  positions that have to be used to vertically shift the labels of
  monthly streamflow values.  
  It is only used when `labels=TRUE`.  
  Lengths smaller than 12 are recycled and larger lengths are not used.

- ...:

  further arguments passed to or from other methods. Not used yet.

## Details

Given a time series of precipitation and streamflow, this function plots
the two time series in the same figure, streamflows as a normal time
series and precipitation as bars coming from the upper part of the
plotting window.

## Value

A figure with the two time series in the same graphical area,
streamflows as a normal time series and precipitation as bars coming
from the upper part of the plotting window.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`hydroplot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md),
[`climograph`](https://hzambran.github.io/hydroTSM/reference/climograph.md),
[`fdc`](https://hzambran.github.io/hydroTSM/reference/fdc.md),
[`fdcu`](https://hzambran.github.io/hydroTSM/reference/fdcu.md),
[`monthlyfunction`](https://hzambran.github.io/hydroTSM/reference/monthlyfunction.md)

## Examples

``` r
######################
## Ex1: Plotting precipitation and streamflows for the full time period of both
##      time series.
##      First, we load the daily P and Q time series for the Cauquenes en 
##      El Arrayan catchment. P, [mm] is the first column and Q, [mm] is the 
##      fifth column.

data(Cauquenes7336001)

# Subsetting to only the 1981-1990 temporal period
Cauquenes7336001 <- window(Cauquenes7336001, start="1981-01-01", end="1990-12-31")

p <- Cauquenes7336001[, 1]
q <- Cauquenes7336001[, 5]

## Plotting P and Q for the full time period of both time series
plot_pq(p=p, q=q)


if (FALSE) { # \dontrun{
  ######################
  ## Ex2: Plotting precipitation and streamflows only for a specific time period,
  ##      from April to December 2000.
  plot_pq(p, q, from="1985-04-01", to="1985-12-31")

  ######################
  ## Ex3: Plotting monthly values of precipitation and streamflows for the 
  ##      full time period of both time series.
  plot_pq(p, q, ptype="monthly")

  ######################
  ## Ex4: Plotting monthly values of precipitation and streamflows for the 
  ##      full time period of both time series, but using a hydrologic year
  ##      starting on April
  plot_pq(p, q, ptype="monthly", start.month=4)
} # }
```
