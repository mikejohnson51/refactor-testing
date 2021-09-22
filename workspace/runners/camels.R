library(nhdplusTools)
library(dplyr)
library(rmapshaper)
library(sf)
library(hyRefactor)

setwd("workspace")
source('R/utils.R')

# url <- "https://ral.ucar.edu/sites/default/files/public/product-tool/camels-catchment-attributes-and-meteorology-for-large-sample-studies-dataset-downloads/basin_set_full_res.zip"
# out <- tempdir(check = TRUE)
# 
# out_f <- file.path(out, basename(url))
# 
# if(!file.exists(file.path(out, basename(url)))) {
#   download.file(url, destfile = out_f)
# }
# 
# outdir <- gsub(".zip", "", out_f)
# try(zip::unzip(out_f, overwrite = FALSE, exdir = outdir))
# 
# basins <- sf::read_sf(file.path(outdir, "HCDN_nhru_final_671.shp"))
# 
# basins$ID <- stringr::str_pad(as.character(basins$hru_id), width = 8, side = "left", pad = "0")
# 
# comids = lapply(1:nrow(basins), function(x){
#   discover_nhdplus_id(nldi_feature = list(featureSource = "nwissite", featureID = paste0("USGS-", basins$ID[x])))
#   })
# 
# basins$comid = unlist(comids)
# 
# sf::write_sf(basins, dsn = "/Users/mjohnson/github/refactor-testing/workspace/runners/data/camels_basins.gpkg")


# cats      = readRDS("/Volumes/Transcend/ngen/refactor-tests/base-data/catchments_all.rds")
# fps       = readRDS('/Users/mjohnson/github/hydroresolve/workspace/NEEDED-DATA/nhdplus_flowline_update_sb.rds')
# basins    = read_sf("/Users/mjohnson/github/refactor-testing/workspace/runners/data/camels_basins.gpkg") %>% 
#   st_drop_geometry() %>% 
#   select(ID, comid)
# 
# 
# for (i in 1:nrow(basins)) {
#   gpkg = file.path('/Volumes/Transcend/ngen/refactor-tests/CAMELS/base-runs/', paste0('gage_', basins$ID[i], ".gpkg"))
#   
#   if (!file.exists(gpkg)) {
#     UT_COMIDs = get_UT(st_drop_geometry(fps), basins$comid[i])
#     
#     flowpaths = make_standalone(filter(fps, COMID %in% UT_COMIDs))
#     
#     catchments = filter(cats, FEATUREID %in% UT_COMIDs) %>% 
#       catchment_geometry_doctor(ID = "FEATUREID", keep = .9)
#     
#     unlink(gpkg)
#     write_sf(flowpaths, gpkg, "flowpaths")
#     write_sf(catchments, gpkg, "catchments")
#   }
#   message(i)
# }

base_files = list.files('/Volumes/Transcend/ngen/refactor-tests/CAMELS/base-runs/', full.names = TRUE)
reference_fabric = list.files('/Volumes/Transcend/ngen/refactor-tests/ngen-reference', full.names = TRUE)

meta = read.csv('https://raw.githubusercontent.com/mikejohnson51/ngen-refactor-params/master/ngen-hydrofabric-mapping.csv')

new_dir= '/Volumes/Transcend/ngen/refactor-tests/CAMELS/temp'

lapply(1:nrow(grid), function(x){
  path = base_files[i]

  RPU    = unique(vapour::vapour_read_fields(path, sql = "SELECT RPUID FROM flowpaths")$RPUID)
  params = filter(meta, rpu_code %in% RPU)
 
  ref_fab = grep(params$rpu_code, reference_fabric, value = TRUE)

  new_file = file.path(new_dir, basename(path))
  fs::file_copy(path, new_file)
  
  refactor_wrapper(in_file                     = new_file, 
                   events = read_sf(ref_fab, "events"),
                   avoid  = read_sf(ref_fab, "avoid")$COMID,
                   split_flines_meters         = params$rf_split_flines_meters, 
                   collapse_flines_meters      = params$rf_collapse_flines_meters,  
                   collapse_flines_main_meters = params$rf_collapse_flines_main_meters,
                   cores                       = 1,  
                   facfdr                      = '/Volumes/Transcend/ngen/refactor-tests/base-data/fdrfac')
  
  aggregate_wrapper(new_file, 
                    ideal_size_sqkm = params$agg_ideal_size_km, 
                    min_area_sqkm   = params$agg_min_size_km, 
                    min_length_km   = params$agg_min_length_km)
})

aggregate_wrapper = function(in_file,
                             ideal_size_sqkm = 10, 
                             min_area_sqkm = 3, 
                             min_length_km = 1){
  
  network_list = list(flowpaths = read_sf(in_file, "refactored_flowpaths") %>% 
    select(ID, 
           toID, 
           lengthkm = LENGTHKM, 
           totdasqkm = TotDASqKM, 
           levelpath = LevelPathID, 
           hydroseq = Hydroseq, 
           member_COMID),
                      catchments = read_sf(in_file, "refactored_catchments")
    ) %>% 
    merge_along_levelpath(ideal_size_sqkm) %>%
    mainstem_reduce(min_area   = min_area_sqkm,  min_length = min_length_km) %>%
    single_levelpaths(min_area = min_area_sqkm,  min_length = min_length_km)
  
  write_sf(network_list$flowpaths,  in_file, "aggregated_flowpaths",  overwrite = TRUE)
  write_sf(network_list$catchments, in_file, "aggregated_catchments", overwrite = TRUE)
  
}

mapview(network_list$flowpaths['levelpath']) + network_list$catchments
