# Zoo -\> RHTest

It creates the input file to the `'RHtest_dlyPrcp.r'` script, used for
testing the homogeneity of climatological time series (available at:
<https://etccdi.pacificclimate.org/software.shtml>)

## Usage

``` r
zoo2RHtest(x, fname="pcp.txt", tstep.out="daily", dec=".", na="-999.0")
```

## Arguments

- x:

  time series that will be written. class(x) must be a zoo object

- fname:

  Character, with the filename of the precipitation time series

- tstep.out:

  Character indicating the time step that have to be used for writing
  `x` into the output file

- dec:

  the string to use for decimal points in numeric or complex columns:
  must be a single character.

- na:

  character to be used for representing the missing values in the data

## References

<https://etccdi.pacificclimate.org/software.shtml>
<https://etccdi.pacificclimate.org/RHtest/RHtestsV4_UserManual_10Dec2014.pdf>

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Examples

``` r
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

#Getting the monthly ts
pcp.m <- daily2monthly(x, FUN=sum, na.rm=FALSE)

if (FALSE) { # \dontrun{
# From zoo to the input format required by 'FindU.dlyPrcp' function
zoo2RHtest(x=pcp.m, fname="pcp-monthly.txt", tstep.out="monthly", na="-999.0")

# Homogeneity analysis
FindU.dlyPrcp(InSeries="pcp-monthly.txt", output="pcp-monthly", MissingValueCode="-999.0", 
GUI=FALSE, pthr=0, Mq=10, p.lev=0.95, Iadj=10000)
} # }
```
