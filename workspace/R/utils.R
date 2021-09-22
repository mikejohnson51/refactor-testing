library(sf)
library(dplyr)
library(raster)
library(hyRefactor)

select <- dplyr::select

#' rgeos LINE Merge
#' @description Wayyyy faster then either data.table, or sf based line merging
#' @param lines lines to merge
#' @param ID ID to merge over
#' @return an sf object
#' @importFrom sf as_Spatial st_geometry_type st_as_sf st_line_merge
#' @importFrom rgeos gLineMerge
#' @importFrom dplyr mutate
#' @importFrom methods slot
#' @export
spLineMerge = function(lines, ID){
  
  SPDF =  as_Spatial(lines)
  
  rownames(SPDF@data) <- sapply(slot(SPDF, "lines"), function(x) slot(x, "ID"))
  
  tmp <- rgeos::gLineMerge(SPDF, byid = TRUE, id = lines[[ID]])
  
  ids <- as.numeric(sapply(slot(tmp, "lines"), function(x) slot(x, "ID")))
  
  st_as_sf(tmp) %>% 
    mutate("{ID}" := ids) %>% 
    flowpaths_to_linestrings() 
}

#' Convert MULITLINESTINGS to LINESTRINGS
#' @param flowpaths a flowpath `sf` object
#' @return a `sf` object
#' @export
#' @importFrom sf st_geometry_type st_geometry st_line_merge
#' @importFrom dplyr bind_rows

flowpaths_to_linestrings = function(flowpaths){
  bool = (st_geometry_type(sf::st_geometry(flowpaths)) == "MULTILINESTRING")
  multis = flowpaths[bool, ]
  if(nrow(multis) > 0){
    sf::st_geometry(multis) = st_line_merge(sf::st_geometry(multis))
  }
  singles = flowpaths[!bool, ]
  
  bind_rows(multis, singles)
}


# path = '/Volumes/Transcend/ngen/refactor-tests/CAMELS/gage_01013500.gpkg'
# flowpaths = read_sf(path, "flowpaths")
# catchments = read_sf(path, "catchments")
# facfdr = '/Volumes/Transcend/ngen/refactor-tests/base-data/fdrfac'
# 
# outfile = '/Volumes/Transcend/ngen/refactor-tests/CAMELS/gage_01013500_refactor.gpkg'

refactor_wrapper = function(in_file, 
                            events = NULL,
                            avoid = NULL,
                            split_flines_meters = 10000, 
                            collapse_flines_meters = 1000,  
                            collapse_flines_main_meters = 1000,
                            cores = 1,  
                            facfdr = NULL,
                            routing = NULL,
                            keep = .9){

  tf <- tempfile(pattern = "refactored", fileext = ".gpkg")
  tr <- tempfile(pattern = "reconciled", fileext = ".gpkg")
  flowpaths = read_sf(in_file, "flowpaths")
  
  if(!is.null(events)){
    events = filter(events, COMID %in% flowpaths$COMID)
  }
  
  if(!is.null(events)){
    avoid = avoid[avoid %in% flowpaths$COMID]
  }
  
  refactor_nhdplus(nhdplus_flines              = flowpaths, 
                   split_flines_meters         = split_flines_meters, 
                   split_flines_cores          = 1, 
                   collapse_flines_meters      = collapse_flines_meters,
                   collapse_flines_main_meters = collapse_flines_main_meters,
                   out_refactored = tf, 
                   out_reconciled = tr, 
                   three_pass          = TRUE, 
                   purge_non_dendritic = FALSE, 
                   events = events,
                   exclude_cats = avoid,
                   warn = FALSE)

   rec = st_transform(read_sf(tr), 5070)
   
  if(!is.null(routing)){
      
      rec$order = nhdplusTools::get_streamorder(st_drop_geometry(select(rec, ID, toID)), status = FALSE)
      
      rec = hyRefactor::add_lengthmap(rec) %>% 
        attributes_for_flowpaths(
          weight_col = "lengthMap",
          length_weight = TRUE,
          rl_vars = c(
            "link", "Qi", "MusK", "MusX", "n",
            "So", "ChSlp", "BtmWdth",
            "time", "Kchan", "nCC",
            "TopWdthCC", "TopWdth", "alt"),
          rl_path  = routing
        )

      
      write_sf(st_transform(rec, 5070), in_file, "refactored_flowpaths", overwrite = TRUE)
    } else {
      write_sf(st_transform(rec, 5070), in_file, "refactored_flowpaths", overwrite = TRUE)
    }

    
    if(!is.null(facfdr)){
      
      rpus = unique(flowpaths$RPUID)
      rpus = rpus[!is.na(rpus)]
      
      fdrfac_files = list.files(facfdr, pattern = rpus, full.names = TRUE)
      fdr = raster::raster(grep("_fdr", fdrfac_files, value = TRUE))
      fac = raster::raster(grep("_fac", fdrfac_files, value = TRUE))
      catchments <-  read_sf(in_file, "catchments")
      catchments <-  st_transform(catchments, st_crs(fdr)) 
      st_precision(catchments) <- raster::res(fdr)[1]
      
      reconciled <- st_transform(read_sf(tr), st_crs(fdr)) 
      refactored <- st_transform(read_sf(tf),  st_crs(fdr)) 
      
      divides    <- reconcile_catchment_divides(catchment = catchments,
                                                fline_ref = refactored,
                                                fline_rec = reconciled,
                                                fdr       = fdr,
                                                fac       = fac,
                                                para      = cores, 
                                                cache     = NULL, 
                                                fix_catchments = TRUE) 

      write_sf(st_transform(divides, 5070), in_file, "refactored_catchments", overwrite = TRUE)
    } 
  
  unlink(list(tr, tf))
  
}

add_length = function(x){ as.numeric(units::set_units(st_length(x), "km")) }  


#' needs_layer
#' @description Checks if layer existing in geopackage
#' @param db character geopackage to check
#' @param layer character layer name
#' @return logical
#' @export
#' @importFrom sf st_layers

needs_layer <- function(db, layer) {
  
  if(file.exists(db)) {
    layers <- st_layers(db)
    if(layer %in% layers$name)
      return(FALSE)
  }
  TRUE
}

# make sf1 compatible with sf2
st_compatibalize <- function(sf1, sf2) {
  sf1 <- st_transform(sf1, st_crs(sf2))
  
  g <- attr(sf1, "sf_column")
  gp <- attr(sf2, "sf_column")
  names(sf1)[names(sf1) == g] <- gp
  attr(sf1, "sf_column") <- gp
  sf1
}


attributes_for_flowpaths = function(flowpaths,
                                    weight_col = "lengthMap",
                                    length_weight = TRUE,
                                    rl_vars,
                                    rl_path){
  
  if(!"Length" %in% rl_vars){ rl_vars = c("Length", rl_vars) }
  
  net_map  <- dplyr::select(st_drop_geometry(flowpaths), ID, !!weight_col) %>%
    mutate(comid = strsplit(get(weight_col), ",")) %>%
    tidyr::unnest(cols = comid) %>%
    mutate(full_comids = floor(as.numeric(comid)),
           w = 10 * (as.numeric(comid) - full_comids), 
           w = ifelse(rep(length_weight, n()), w, 1),
           comid = NULL) 

  nc = RNetCDF::open.nc(rl_path)
  
  ll = lapply(rl_vars, function(x) RNetCDF::var.get.nc(nc, x))

  df = data.frame(do.call(cbind, ll)) %>% 
    setNames(rl_vars) %>% 
    rename(comid = link) %>% 
    right_join(net_map, by = c('comid' = 'full_comids')) %>% 
    select(-!!weight_col) %>% 
    mutate(w = w * Length) %>% 
    group_by(ID) %>% 
    summarise(across(everything(), ~ round(
      weighted.mean(x = ., 
                    w = w, 
                    na.rm = TRUE), 3))) %>% 
    dplyr::select(-comid, -Length, -w) 
  
  df2 = lapply(c("link", "gages", 'NHDWaterbodyComID'), function(x) x = RNetCDF::var.get.nc(nc, x)) %>% 
    bind_cols() %>% 
    setNames(c("link", "gages", 'NHDWaterbodyComID')) %>% 
    rename(comid = link) %>% 
    right_join(net_map, by = c('comid' = 'full_comids')) %>%
    mutate(gages = trimws(gages),
           gages = ifelse(gages == "", NA, gages),
           NHDWaterbodyComID = ifelse(NHDWaterbodyComID == -9999, NA, NHDWaterbodyComID)
           ) %>% 
    group_by(ID) %>% 
    summarise(gages = paste(gages[!is.na(gages)], collapse = ","),
              NHDWaterbodyComID = paste(unique(NHDWaterbodyComID[!is.na(NHDWaterbodyComID)]), collapse = ",")) %>% 
    left_join(df) %>% 
    mutate(gages = ifelse(gages == "", NA, gages),
           NHDWaterbodyComID = ifelse(NHDWaterbodyComID == "", NA, NHDWaterbodyComID))
  
  left_join(flowpaths, df2, by = "ID") %>% 
    mutate(Length_m = st_length(.))
}

mainstem_reduce = function(network_list, min_area = 3, min_length = 1000) {
  
  cat = network_list$catchments %>% 
    mutate(areasqkm = hyRefactor::add_areasqkm(.))
  
  fl  = network_list$flowpaths %>%
    mutate(lengthkm = add_length(.), areasqkm = NULL) %>% 
    left_join(st_drop_geometry(cat), by = "ID")
  
  network  = fl %>% 
    st_drop_geometry() %>% 
    group_by(levelpath) %>% 
    mutate(toID   = lead(ID),
           fromID = lag(ID),
           flag = lengthkm < (min_length /1000) | areasqkm < min_area) %>% 
    ungroup() 
  
  # Those that are flagged and not an outlet
  # These will all be merged down the network
  mid_reach = network %>% 
    filter(flag & !is.na(toID)) %>% 
    select(ID, into = toID) 
  
  # Outlet reaches that are flagged
  # These will all be merged up the network
  outlet = filter(network, is.na(toID) & flag) %>% 
    select(ID, into = fromID) %>% 
    filter(!is.na(into))
  
  # Identities to merge  
  gg = bind_rows(mid_reach, outlet) %>% 
    group_by(into) %>% 
    mutate(g = cur_group_id()) %>% 
    ungroup() %>% 
    tidyr::pivot_longer(-g) %>% 
    select(ID = value, direction = name, g) 
  
  tmp = st_as_sf(left_join(gg, fl, by = 'ID'))
  
  if(nrow(tmp) > 0){
    meta = tmp %>%
      st_drop_geometry() %>% 
      group_by(g) %>%
      arrange(direction) %>% 
      summarize(
        ID = last(ID),
        levelpath = unique(levelpath),
        hydroseq  = last(hydroseq),
        comids    = paste(comids, collapse = ",")
      ) %>% 
      ungroup()
  
    tmp = select(left_join(spLineMerge(tmp, "g"), meta, by = 'g'), -g) 
  }

  new_fl = fl %>% 
    filter(!ID %in% gg$ID) %>% 
    bind_rows(tmp)
  
  gaps = filter(new_fl, st_geometry_type(new_fl) == "MULTILINESTRING")
  
  if(nrow(gaps) > 0){
    
  new_geom = lapply(1:nrow(gaps), function(x) {
    st_cast(st_line_merge(st_union(st_cast(
      st_cast(gaps$geometry[x], "LINESTRING"), "MULTILINESTRING"
    ))), "LINESTRING")
  })
 
  st_geometry(gaps) =  do.call('c', new_geom)
  
  } else {
    gaps = NULL
  }
  
  
  new_fl = filter(new_fl, !ID %in% gaps$ID) %>% 
    bind_rows(gaps)
 
  ####################
  
  
  tmp_cat = st_as_sf(left_join(gg, cat, by = 'ID'))
  
  if(nrow(tmp_cat) > 0){
    tmp_cat = select(left_join(hyRefactor::union_polygons_geos(tmp_cat, "g"), select(meta, g, ID), by = 'g'), -g)  
  }
  
  new_cat = cat %>% 
    filter(!ID %in% gg$ID) %>% 
    bind_rows(tmp_cat)
  
  dups = new_cat$ID[duplicated(new_cat$ID)]
  
  remove_islands(network_list = list(flowpaths  = select(new_fl, flowpath_names), 
       catchments = select(new_cat, catchment_names)))
}


build_int_map = function(fl, min_length, min_area){
  
  candidates  = fl %>% 
    group_by(levelpath) %>% 
    mutate(n = n()) %>% 
    filter(n == 1) %>% 
    filter(lengthkm < (min_length) | areasqkm < min_area) %>% 
    ungroup() %>% 
    mutate(int = lengths(st_intersects(., get_node(fl, "end"))) - 1) %>% 
    filter(int < 2)
  
  
  #mapview(fl) + mapview(candidates, color = "Red") + cat
  # find intersections!
  imap = st_intersects(candidates, fl)
  
  data.frame(
    nodeID = rep(candidates$ID, times = lengths(imap)),
    intID  = fl$ID[unlist(imap)],
    intHS  = fl$hydroseq[unlist(imap)])  %>% 
    filter(nodeID != intID) %>% 
    group_by(nodeID) %>% 
    slice_min(intHS) %>% 
    ungroup() %>% 
    filter(!nodeID %in% intID) %>% 
    select(nodeID, intID) %>% 
    filter(!duplicated(.)) %>% 
    group_by(intID) %>% 
    mutate(g = cur_group_id()) %>% 
    ungroup() %>% 
    tidyr::pivot_longer(-g, values_to = 'ID') %>% 
    filter(!duplicated(.))
}

single_levelpaths = function(network_list,
                             min_area   = 3, 
                             min_length = .6){
  
  
  cat = network_list$catchments %>% 
    mutate(areasqkm = hyRefactor::add_areasqkm(.)) %>% 
    select(ID, areasqkm) %>% 
    nhdplusTools::rename_geometry("geometry")
  
  fl  = network_list$flowpaths %>%
    mutate(lengthkm = add_length(.), areasqkm = NULL) %>% 
    left_join(st_drop_geometry(cat), by = "ID")
  
 int_map_p = build_int_map(fl, min_length, min_area)
 
  if(nrow(int_map_p) > 0) {

    tmp_cat = st_as_sf(left_join(int_map_p, cat, by = 'ID'))
    
    c  = hyRefactor::union_polygons_geos(tmp_cat, "g")
    cc = left_join(c, filter(int_map_p, name == "intID"), by = "g") %>% 
      select(ID, areasqkm) 

    cat2 = cat %>%
      filter(!ID %in% int_map_p$ID) %>%
      bind_rows(cc) %>%
      select(ID, areasqkm)
    
    fl2 = filter(fl, !ID %in% filter(int_map_p, name == "nodeID")$ID)
    
    remove_islands(list(flowpaths = fl2, catchments = cat2))
    } else {
    network_list
  }
}


remove_islands = function(network_list){
  
  cat = network_list$catchments
  fl  = network_list$flowpaths
  
  inters = st_intersects(cat)
  
  interior_rings = data.frame(ID = cat$ID, n = lengths(inters)) %>%
    filter(n == 2)
  
  int_list = inters[which(cat$ID %in% interior_rings$ID)]
  
  all = cat$ID[unlist(int_list)]
  
  exteriors = all[!all %in% interior_rings$ID]
  
  
  if(nrow(interior_rings) > 0){
  message("Dissolving ", nrow(interior_rings), " islands...")
  
  outs = list()
  for(i in 1:nrow(interior_rings)){
    outs[[i]] = cat[int_list[[i]],] %>%
      mutate(area = st_area(.), id = 1) %>%
      arrange(-area) %>%
      group_by(id) %>%
      summarize(ID = ID[1])
  }
  
  new_exteriors = bind_rows(outs) %>%
    select(-id) %>%
    group_by(ID) %>%
    summarise()
  
  new_cat = cat %>%
    filter(!ID %in% all) %>%
    bind_rows(new_exteriors) %>%
    mutate(areasqkm = as.numeric(st_area(.)/1e6))
  
  new_fl = fl %>%
    filter(ID %in% new_cat$ID) %>%
    inner_join(st_drop_geometry(new_cat), by = 'ID')
  
  list(flowpaths = select(new_fl, flowpath_names), 
       catchments = select(new_cat, catchment_names))
  }
  
   network_list
}


merge_levelpath = function(network_list = out, ideal_size = 10){
  
  # Preprocessing .... ------------------------------------------------------
  cat = network_list$catchments %>% 
    mutate(areasqkm = hyRefactor::add_areasqkm(.))
  
  fl  = network_list$flowpaths %>%
    mutate(lengthkm = add_length(.)) %>% 
    left_join(st_drop_geometry(cat), by = "ID")
  
  network_group = fl %>%
    group_by(levelpath) %>% 
    arrange(-hydroseq) %>%
    mutate(ind = cs_group(areasqkm, ideal_size)) %>%
    ungroup()   %>% 
    group_by(levelpath, ind) %>% 
    mutate(n = 1:n())
  
  network_group$g = group_indices(network_group)
  network_group   = ungroup(network_group)
  
  catchment_group = left_join(cat, 
                              select(st_drop_geometry(network_group), 
                                     ID, n, g))
  
  meta = network_group %>%
    st_drop_geometry() %>% 
    group_by(g) %>%
    summarize(
      levelpath = unique(levelpath),
      hydroseq  = min(hydroseq),
      comids    = paste(comids, collapse = ",")
    )
  
  tmp = left_join(spLineMerge(network_group, "g"), meta)
  
  gaps = filter(tmp, st_geometry_type(tmp) == "MULTILINESTRING")
  
  if(length(gaps) > 0){
    new_geom = lapply(1:nrow(gaps), function(x) {
      st_cast(gaps$geometry[x], "MULTIPOINT") %>% 
        st_combine() %>% 
        st_cast("LINESTRING")
    })
    
    st_geometry(gaps)  =  do.call('c', new_geom)
  } else {
    gaps = NULL
  }
  
  tmp = filter(tmp, !g %in% gaps$g) %>% 
    bind_rows(gaps) %>% 
    rename(ID = g)
  
  if(sum(st_geometry_type(tmp) == "MULTILINESTRING") != 0){
    stop("MLs remain in the network")
  }
  
  cat_tmp = suppressWarnings({
    spCatMerge(catchment_group, "g") %>% 
      st_cast("POLYGON")
  })
  
  dups = cat_tmp$g[duplicated(cat_tmp$g)]
  
  if(lengths(dups) > 0){
    
    cat_tmp$tmpID = 1:nrow(cat_tmp)
    message("Cleaning up ", length(dups), " duplicated catchment fragments")
    
    for(i in 1:length(dups)){
      here = filter(cat_tmp, g == dups[i])
      tmap = st_intersects(here, filter(tmp, ID == dups[i]))
      
      dissolve = here[lengths(tmap) == 0, ]
      
      opt = suppressWarnings({
        st_intersection(dissolve, cat_tmp) %>% 
          st_collection_extract("LINESTRING") %>%
          mutate(l = st_length(.)) %>%
          slice_max(l, with_ties = FALSE)
      })
      
      ind = which(cat_tmp$tmpID == opt$tmpID.1)
      
      cat_tmp$geometry[ind] = st_union(dissolve$geometry, cat_tmp$geometry[ind])
      cat_tmp = filter(cat_tmp, tmpID != dissolve$tmpID)
    }
    
    cat_tmp = dplyr::select(cat_tmp, -tmpID)
  }
  
  cat_tmp = rename(cat_tmp, ID = g)
  
  list(flowpaths  = tmp,
       catchments = cat_tmp)
}
