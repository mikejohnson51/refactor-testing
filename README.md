
<!-- README.md is generated from README.Rmd. Please edit that file -->

# refactor-testing

<!-- badges: start -->
<!-- badges: end -->

The goal of `refactor-testing` is to test the spatial sensitivty to
different refactoring parameters to support routing using t-route. This
is stictly a workflow directory intended to interate processes over
space and parameters sets either locally or via AWS batch.

Package requires access to the USGS reference hydrofabic and POIs
(currently being develped locally but soon to be public) as well as an
archive of the FDR and FDC rasters from the NHDPlusv2.1.

The steps on this workflow are to (1) take the USGS reference fabric and
POIs to create a `ngen_reference_fabric`. The second step is to refactor
this network using a grid of variables controlling the max and minimun
length permited in the output network.

The third step is to apply the t-route routing routines covering these
iterative hydrofabrics to identify the minimum reach length that
produces a minimally acceptable (?) number of Corant number violations.

For those with access to the Lynker AWS account create an inst/aws.R
file with the following:

``` r
library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID"     = "***",
           "AWS_SECRET_ACCESS_KEY" = "***",
           "AWS_DEFAULT_REGION"    = "us-east-2")
```
