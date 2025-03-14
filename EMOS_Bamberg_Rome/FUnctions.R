#---- Function converts datacubes with one band

#---- and reduced time dimension to datatable

convert_band_to_table <- function(band_cube){
  res <-  p$save_result(band_cube,format = f$output$GTiff)
  band_name_string <- deparse(substitute(band_cube))
  manual_file <- paste0(band_name_string, "_file.gtiff")
  file <- compute_result(res, output_file = manual_file)
  gt <- raster(manual_file)
  #spplot(gt)
  sf_gt <- st_as_sf(rasterToPolygons(gt))
  return(sf_gt)
}

#---- Function gets Sentine data for one band

#---- and reduces time dimension

get_band <- function(TimeFire, Region, Sentinal,  band_name){
  if (!is.character(Sentinal) || Sentinal == "") {
    warning("Sentinal should be a non-empty character.")
    return(NULL)  
  }
  if (!is.character(band_name) || band_name == "") {
    warning("band_name should be a non-empty character.")
    return(NULL)  
  }
  band <- p$load_collection(id = Sentinal, 
                            temporal_extent = TimeFire, 
                            spatial_extent = Region,
                            bands=list(band_name))
  band_red <- p$reduce_dimension(data = band, 
                                 reducer = function(data, context) 
                                 { p$median(data) }, dimension = "t")
  
  return(band_red)
}

#---- Function gets multiple bands, transforms them into

#---- indices, merges them and stores them in one shapefile

get_data_fire <- function(TimeFire, TimeCovariates, Region){
  #################################
  #bands:
  fire_band_red <- get_band(TimeFire, Region, "SENTINEL3_SLSTR", "F1")
  fire1_band_red <- get_band(TimeFire, Region, "SENTINEL3_SLSTR", "S7")
  temp_band_red <- get_band(TimeCovariates, Region, "SENTINEL3_SLSTR_L2_LST", "LST")
  clor_band_red <- get_band(TimeCovariates, Region, "SENTINEL3_OLCI_L1B", "B03" )
  veg_band_red <- get_band(TimeCovariates, Region,"SENTINEL2_L2A","SCL" )
  moist_band_11 <- get_band(TimeCovariates, Region, "SENTINEL2_L2A", "B11" )
  moist_band_8A <- get_band(TimeCovariates, Region, "SENTINEL2_L2A", "B8A" )
  topo_band_4 <- get_band(TimeCovariates, Region,"SENTINEL2_L2A", "B04" )
  
  topo_band_8 <- get_band(TimeCovariates, Region, "SENTINEL2_L2A", "B08")
  #################################
  #create tables for every band:
  
  
  tem_band_table <- convert_band_to_table(band_cube = temp_band_red)
  clor_band_table <- convert_band_to_table(band_cube = clor_band_red)
  veg_band_table <- convert_band_to_table(band_cube = veg_band_red)
  topo_band_table_4 <- convert_band_to_table(topo_band_4)
  topo_band_table_8 <- convert_band_to_table(topo_band_8)
  moi_band_table_8A <- convert_band_to_table(moist_band_8A)
  moi_band_table_11 <- convert_band_to_table(moist_band_11)
  #Tem in celsius
  tem_band_table$LST <- tem_band_table$LST - 273.15
  #########################
  #Extra calculation for fireidx:
  fire1_band_table <- convert_band_to_table(band_cube = fire_band_red)
  fire2_band_table <- convert_band_to_table(band_cube = fire1_band_red)
  merged <- st_join(fire1_band_table,fire2_band_table, left = T)
  merged$S7 <- ifelse(is.na(merged$S7), 325, merged$S7)
  merged$Fireidx <- (merged$S7 + merged$F1)/2
  fire_band_table <- merged[,3:4]
  #Extra calculation for moisture:
  moisture_band_table <- st_join(moi_band_table_8A, moi_band_table_11)
  moisture_band_table$moi <- (moisture_band_table$B8A - moisture_band_table$B11)/
    (moisture_band_table$B8A + moisture_band_table$B11)
  moisture_band_table <- moisture_band_table[, c("geometry", "moi")]
  #Extra calculations for topology 
  topo_band_table <- st_join(topo_band_table_4, topo_band_table_8)
  topo_band_table$topo <- (topo_band_table$B08 - topo_band_table$B04)/
    (topo_band_table$B08 + topo_band_table$B04)
  topo_band_table <- topo_band_table[, c("geometry", "topo")]
  #######################
  #join  data for faster computing time
  fire_band_table_sum <- fire_band_table %>%
    group_by(Fireidx) %>% # Grouping by the 'group_id' column or any other relevant column
    summarise(
      geometry = st_union(geometry)    # Merging polygons into one
    )
  tem_band_table_sum  <- tem_band_table %>%
    group_by(LST) %>% # Grouping by the 'group_id' column or any other relevant column
    summarise(
      geometry = st_union(geometry)    # Merging polygons into one
    )
  clor_band_table_sum <- clor_band_table %>%
    group_by(B03) %>% # Grouping by the 'group_id' column or any other relevant column
    summarise(
      geometry = st_union(geometry)    # Merging polygons into one
    )
  veg_band_table_sum <- veg_band_table %>%
    group_by(SCL) %>% # Grouping by the 'group_id' column or any other relevant column
    summarise(
      geometry = st_union(geometry)    # Merging polygons into one
    )
  moisture_band_table_sum <- moisture_band_table %>%
    group_by(moi) %>% # Grouping by the 'group_id' column or any other relevant column
    summarise(
      geometry = st_union(geometry)    # Merging polygons into one
    )
  topo_band_table_sum <- topo_band_table %>%
    group_by(topo) %>% # Grouping by the 'group_id' column or any other relevant column
    summarise(
      geometry = st_union(geometry)    # Merging polygons into one
    )
  ###############################
  #merge tables: 
  sf_joined1 <- st_join(fire_band_table_sum, tem_band_table_sum)
  sf_joined2 <- st_join(sf_joined1, clor_band_table_sum)
  sf_joined3 <- st_join(topo_band_table_sum, moisture_band_table_sum)
  sf_joined3 <- st_transform(sf_joined3, st_crs(sf_joined2))
  sf_joined_final <- st_join(sf_joined2, sf_joined3)
  sf_joined_final_data <- st_drop_geometry(sf_joined_final)
  return(sf_joined_final_data)
}