
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `distribglm`: Distributed Computing Model from a Synced Folder

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/muschellij2/distribglm.svg?branch=master)](https://travis-ci.com/muschellij2/distribglm)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/muschellij2/distribglm?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/distribglm)
[![R-CMD-check](https://github.com/muschellij2/distribglm/workflows/R-CMD-check/badge.svg)](https://github.com/muschellij2/distribglm/actions)
<!-- badges: end -->

The goal of `distribglm` is to provide an example of a Distributed
Computing Model from a Synced Folder. In this case, “synced folder”
usually means Dropbox or Box Sync.

## Installation

You can install the released version of distribglm from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("distribglm")
remotes::install_github("muschellij2/distribglm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(distribglm)
## basic example code
```

To set up a model, run `setup_model.R`. Currently, it’s only been tested
on logistic regression. This will ensure the correct folders are set up
on the synced folder. Once the folders are synced, each hospital/site
should use the `secondary.R` file.

## Hospital/Site Configuration

For each site, a few things need to be specified. First, the `site_name`
needs to be specified in the document. This can be something like
`site1` or something like `my_hospital_name`. Second, the site must
specify the `synced_folder` path. Though this folder is synced across
the sites and the computing site, it may be located on different
sub-folders on each computer. For example
`~/Dropbox/Projects/distributed_model`,
`C:/My Documents/distributed_model`, etc.

Additionally, the site may need to specify `all_site_names`, which is
the names of all the other sites that are computing this model as well.
This can be relaxed, but is currently there so that each site can see
what’s going on, whether they’re waiting for another site to finish
computing or for the computing site to update the beta coefficients.

The third thing is the site needs to specify the `data_source`. In the
cases above, we use a CSV for the data. The important thing is that
**this data can be in a secure place**. The data from this is used for
computation but is **not** in the synced folder. Lastly, the site needs
to specify the `model_name` to indicate which model is being analyzed.
This could be relaxed where once a model is finalized/converged, all
iterations and estimates are zipped up and then the formula is removed.
Thus, if you check that a formula no longer exists, the site code will
exit.

All sites need to have the data structured so the column names match
exactly across sites for the formula specification to work. There could
be an initial data check step too.

## Computing Site Configuration

The computing/master site first needed to setup the model from above.
(Maybe specify all site names here too?). Similar to the sites, the
computing site needs to specify the `model_name` to indicate which model
is being analyzed, the `synced_folder` path, and `all_site_names`. That
should be it to start the process until the model converges.

# What actually happens?

Currently, the process that happens is as follows:

1.  The data is read into `R`. The model design matrix `X` is created
2.  Current β estimates are pulled for the specified model. If no
    estimates exist, β is initialized to a vector of zeros.
3.  `Xβ` is computed, which is *ŷ*, and *ε̂* = *y* − *ŷ*. Then
    *δ̂* = *X**ε̂* is created and *δ̄* is used as the vector of gradients
    to update *β*\`. Each site returns *δ̂* and the sample size at that
    site so that they can be combined to *δ̄*.
4.  The compute site gathers the *δ̂* and the sample sizes, and computes
    *δ̄*, and then writes the updated *β*.  
5.  The compute site checks to see if *δ̄* is below some tolerance level
    to determine convergence.

# Outstanding issues

1.  Standard errors
2.  Better diagnostics
3.  Use Fisher scoring vs. gradient descent for better convergence rates
    (<https://statmath.wu.ac.at/courses/heather_turner/glmCourse_001.pdf>).
    This will solve the standard errors todo as well.
