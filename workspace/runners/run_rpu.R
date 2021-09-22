## ---------------------------
## Test Refactoring Params
## for assigned RPU
##
## Author: Mike (mikecp11@gmail.com)
##
## ---------------------------
## Notes:
##
## ---------------------------

# Move down a level in project
setwd("workspace")

## ------------------------------------------------------

# Load creds and packages
library(rmarkdown)
source("../inst/aws.R") # Personal file containing AWS access

# -------------------------------------------------------------------------
# User Defined Parameters
base       <- '/Volumes/Transcend/ngen/refactor-tests'

# GO! ---------------------------------------------------------------------
rpu_code = "01a"
## Step 1: ngen_reference
render(
  input  = '01_ngen_reference.Rmd',
  params = list(
    RPU =  rpu_code,
    min_da_km        =  20,
    base_dir         = file.path(base, "base-data"),
    reference_fabric = file.path(base, "usgs-reference"),
    output_dir       = file.path(base,"ngen-reference")),
  envir        = new.env(),
  output_file  = paste0('temp/01_ngen_reference_', rpu_code, '.html')
)

## Step 2: refactor tests
render(
  input  = '02_refactor_grid.Rmd',
  params = list(
    RPU = rpu_code,
    reference_fabric = file.path(base,"ngen-reference"),
    output_dir       = file.path(base, "refactor-tests-rpu"),
    split_flines_meters         =  10000,
    collapse_flines_main_meters = c(500),#, 1000, 1500, 2500, 3500, 4500),
    # Adopts those above if NULL
    collapse_flines_meters      = NULL,
    catchments                  = FALSE,
    fdrfac = file.path(base, 'base-data/fdrfac'),
    cores  =  2),
  envir        = new.env(),
  output_file  = paste0('temp/02_ngen_reference_', rpu_code, '.html')
)



