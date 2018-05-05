
<!-- README.md is generated from README.Rmd. Please edit that file -->

# volleyxml

[![Travis-CI Build
Status](https://travis-ci.org/raymondben/volleyxml.svg?branch=master)](https://travis-ci.org/raymondben/volleyxml)

An R package for reading volleyball scouting files in XML format.

See also the [datavolley R
package](https://github.com/raymondben/datavolley) for DataVolley files.
Note that the data structures and conventions returned by `volleyxml`
have been chosen to be similar (partly compatible) with those of the
`datavolley` package.

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
