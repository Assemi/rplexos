# rplexos

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/NREL/rplexos?branch=master&svg=true)](https://ci.appveyor.com/project/NREL/rplexos)
[![Coverage status](https://codecov.io/gh/NREL/rplexos/branch/master/graph/badge.svg)](https://codecov.io/github/NREL/rplexos?branch=master)

`rplexos` is an R package developed to read and analyze PLEXOS solutions. It currently supports the
conversion of PLEXOS solution files into SQLite databases and functions to query those databases.

The code for rplexos has been obtained from https://github.com/NREL/rplexos and updated to fix issues and make it compatible with PLEXOS 9.

It can only be installed from GitHub at the moment as the package has been removed from CRAN:

```
## RTools should be installed first: https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html
## Then install devtools package
# install.packages("devtools")
library(devtools)
install_github("Assemi/rplexos")
```

The "Getting started" vignette presents the preferred workflow to process PLEXOS solutions with this package.

```
library(rplexos)
vignette("rplexos")
```
