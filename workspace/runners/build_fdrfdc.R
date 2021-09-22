## Build tif files for FDR/FDC 

files = list.dirs('/Users/mjohnson/github/hydroresolve/workspace/NEEDED-DATA/fdrfac')

fac = grep("/fac", files, value = TRUE)


dir = '/Volumes/Transcend/ngen/refactor-tests/fdrfac'
for(i in 1:length(fac)){
  r = terra::rast(fac[i])
  b = basename(gsub("/fac", "", fac[i]))
  file = file.path(dir, paste0(basename(gsub("NHDPlusFdrFac", "", b)), "_fac.tif"))
  terra::writeRaster(r,  file,  overwrite = TRUE, gdal=c("COMPRESS=LZW","of=COG"))
  message(i)
}

fdr = grep("/fdr$", files, value = TRUE)

for(i in 1:length(fdr)){
  b = basename(gsub("/fdr", "", fdr[i]))
  file = file.path(dir, paste0(basename(gsub("NHDPlusFdrFac", "", b)), "_fdr.tif"))
  if(!file.exists(file)){
    r = terra::rast(fdr[i])
    terra::writeRaster(r, file,  overwrite = TRUE,  gdal=c("COMPRESS=LZW","of=COG"))
  }
 
}

