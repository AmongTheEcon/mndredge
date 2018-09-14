# mndredge

[![Travis-CI Build Status](https://travis-ci.org/markanewman/mndredge.svg?branch=master)](https://travis-ci.org/markanewman/mndredge)
[![Coverage Status](https://codecov.io/github/markanewman/mndredge/coverage.svg?branch=master)](https://codecov.io/github/markanewman/mndredge?branch=master)

This repo contains data dredging routenes


# Documentation

Documentation can be found [here][docs]

# Installation

## Official, stable release

None yet.

## Latest development build

To install the latest development snapshot, type following commands into the R console

```{r}
if (!require('devtools')) install.packages('devtools', quiet=TRUE)
devtools::install_github("markanewman/mndredge", quiet = T)
library(mndredge)
```

## Rebuild from scratch

1. Clone the [repo][repo] from [GitHub][github]
    * I suggest to C:\repos\mndredge
2. Open up [R Studio][rstudio]
3. Run the below code to recompile everything

```{r}
# load libraries
if (!require('devtools')) install.packages('devtools', quiet=TRUE)
if (!require('roxygen2')) install.packages('roxygen2', quiet=TRUE)
if (!require('pkgdown')) install.packages('pkgdown', quiet=TRUE)

# set working directory and clean files that will be regenerated
setwd("C:/repos/mndredge")
unlink(c("./docs/*", "./man/*", "./NAMESPACE"), recursive = T)

# run the tests
devtools::test()

# add in documentation and github pages
devtools::document()
pkgdown::build_site()

# full check before the package is CRAN ready
devtools::check()
```

[docs]: https://markanewman.github.io/mndredge
[github]:https://github.com
[repo]: https://github.com/markanewman/mndredge
[rstudio]: https://www.rstudio.com/