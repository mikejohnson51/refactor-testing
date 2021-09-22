# Move down a level in project
setwd("workspace")

## ------------------------------------------------------

# Load creds and packages
library(rmarkdown)
library(purrr)
library(dplyr)
source("../inst/aws.R") 

base       <- '/Volumes/Transcend/ngen/refactor-tests'

meta <-  read.csv('../inst/ngen-hydrofabric-mapping.csv') %>% 
  mutate(VPU = substr(rpu_code, 1,2)) %>% 
  filter(as.numeric(VPU) <= 18) %>% 
  filter(VPU != "09") %>% 
  split(.$rpu_code) %>%
  map(~rmarkdown::render(
                  input  = '01_ngen_reference.Rmd',
                  params = list(
                    RPU              =  .$rpu_code,
                    min_da_km        =  .$rf_min_da_km,
                    reference_fabric =  file.path(base, "usgs-reference"),
                    output_dir       =  file.path(base, "ngen-reference"),
                    AWS_bucket = "formulations-dev/refactoring-test/ngen-reference"),
                  envir        = new.env(),
                  output_file  = paste0('temp/01_ngen_reference_', .$rpu_code, '.html')
  ))


system.time({
  read.csv('../inst/ngen-hydrofabric-mapping.csv') %>% 
    mutate(VPU = substr(rpu_code, 1,2)) %>% 
    filter(as.numeric(VPU) <= 18) %>% 
    filter(VPU != "09") %>% 
    slice(8:25) %>% 
    split(.$rpu_code) %>%
    map(~rmarkdown::render(
      input  = '02_refactor_grid.Rmd',
      params = list(
        RPU              =  .$rpu_code,
        routelink        =  file.path(base, 'base-data/RouteLink_CONUS.nc'),
        reference_fabric =  file.path(base, "ngen-reference"),
        output_dir       =  file.path(base, "refactor-tests-rpu"),
        split_flines_meters = c(10000),
        collapse_flines_main_meters = c(500, 1000, 1500, 2500, 3500, 4500),
        collapse_flines_meters = NULL,
        fdrfac =  NULL,
        AWS_bucket = "formulations-dev/refactoring-test/refactor-tests-rpu"),
      envir        = new.env(),
      output_file  = paste0('temp/01_ngen_reference_', .$rpu_code, '.html')
    ))
})




merge_VPU3 = function(dir = file.path(base, "usgs-reference")){
  
  refs = list.files(dir, pattern = '03', full.names = TRUE)

  fps = lapply(1:length(refs), function(x) { read_sf(refs[x], 'nhd_flowline') })
  POIs = lapply(1:length(refs), function(x) { 
    poi_layer = paste0("POIs_", gsub(".gpkg", "", gsub("reference_", "", basename(refs[x]))))
    read_sf(refs[x], poi_layer) })
  
  fps  = bind_rows(fps)
  pois = bind_rows(POIs)
  
  out_file = file.path(dirname(refs[1]), "reference_03.gpkg")
  
  write_sf(fps,  out_file, 'nhd_flowline')
  write_sf(pois, out_file, 'POIs_03')

}

merge_VPU10 = function(dir = file.path(base, "usgs-reference")){
  
  refs = list.files(dir, pattern = '10', full.names = TRUE)
  
  fps = lapply(1:length(refs), function(x) { read_sf(refs[x], 'nhd_flowline') })
  
  POIs = lapply(1:length(refs), function(x) { 
    poi_layer = paste0("POIs_", gsub(".gpkg", "", gsub("reference_", "", basename(refs[x]))))
    read_sf(refs[x], poi_layer) })
  
  fps  = bind_rows(fps)
  pois = bind_rows(POIs)
  
  out_file = file.path(dirname(refs[1]), "reference_10.gpkg")

  unlink(out_file)
    
  write_sf(fps,  out_file, 'nhd_flowline')
  write_sf(pois, out_file, 'POIs_10')
}


params$AWS_bucket = "formulations-dev/refactoring-test/refactor-tests-rpu"
