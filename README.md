
<!-- README.md is generated from README.Rmd. Please edit that file -->

# volleyxml

[![Travis-CI Build
Status](https://travis-ci.org/raymondben/volleyxml.svg?branch=master)](https://travis-ci.org/raymondben/volleyxml)

An R package for reading volleyball scouting files in the
[hudl](https://www.hudl.com/sports/volleyball) XML format. Files in this
format can be exported from scouting apps including
[iStatVball 2](http://www.istatvball.com/), [SoloStats
Live](https://www.solostatslive.com/solostats-live.html), and others.
(This R package is not affiliated with or endorsed by hudl or any of
these scouting apps.)

See also the [datavolley R
package](https://github.com/raymondben/datavolley) for handling
DataVolley files. The data structures and conventions used by
`volleyxml` have been chosen to be similar to those of the `datavolley`
package.

## Installation

``` r
library(devtools)
install_github("raymondben/volleyxml")
```

## Example

Read the example data file bundled with the package:

``` r
library(volleyxml)
x <- vx_read(vx_example_file())
summary(x)
#> Match summary:
#> Date: 2018-03-17
#> Teams: Attacking Aardvarks
#>        vs
#>        Blocking Badgers
#> Result: 3-0 (25-16, 25-20, 25-22)
```
