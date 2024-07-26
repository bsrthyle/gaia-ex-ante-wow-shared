
# ------------------------------------------------------------------------------

# directories
input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/data-output/')

# load rasters

# number_of_farms instead of crop_area..
# number_of_farms <- rural_populations * 0.7 or ask deo

crop_area <- terra::rast(paste0(input_path, "spam_harv_area_processed.tif"))
q10 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_q10_year1_yield_1_cprice_1_lprice_100_discrate_0.1.tif')))
q50 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_q50_year1_yield_1_cprice_1_lprice_100_discrate_0.1.tif')))
q90 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_q90_year1_yield_1_cprice_1_lprice_100_discrate_0.1.tif')))

# select variables of interest
q10 <- c(crop_area, q10[[grep('_return_usha|_gm_usha|_roi_usha', names(q10))]])
q50 <- c(crop_area, q50[[grep('_return_usha|_gm_usha|_roi_usha', names(q50))]])
q90 <- c(crop_area, q90[[grep('_return_usha|_gm_usha|_roi_usha', names(q90))]])

for(crop in names(crop_area)){
  print(crop)
  # subset per crop and do calculations
  q10_maize <- q10[[grep(crop, names(q10))]]
  q10_maize$profit_class <- terra::ifel(q10_maize[[grep('_gm_usha', names(q10_maize))]] > 0, 1, 0)
  q10_maize$profit_ha <- 0.1 * q10_maize[[crop]] * q10_maize$profit_class
  q10_maize$profit_usdha <- 0.1 * q10_maize[[grep('_gm_usha', names(q10_maize))]] * q10_maize$profit_class
  q50_maize <- q50[[grep(crop, names(q50))]]
  q50_maize$profit_class <- terra::ifel(q50_maize[[grep('_gm_usha', names(q50_maize))]] > 0, 1, 0)
  q50_maize$profit_ha <- 0.8 * q50_maize[[crop]] * q50_maize$profit_class
  q50_maize$profit_usdha <- 0.8 * q50_maize[[grep('_gm_usha', names(q50_maize))]] * q50_maize$profit_class
  q90_maize <- q90[[grep(crop, names(q90))]]
  q90_maize$profit_class <- terra::ifel(q90_maize[[grep('_gm_usha', names(q90_maize))]] > 0, 1, 0)
  q90_maize$profit_ha <- 0.1 * q90_maize[[crop]] * q90_maize$profit_class
  q90_maize$profit_usdha <- 0.1 * q90_maize[[grep('_gm_usha', names(q90_maize))]] * q90_maize$profit_class
  # sum area across quantiles per crop
  maize <- crop_area[[crop]]
  names(maize) <- 'ha_spam'
  maize$area_profitable_ha <- q10_maize$profit_ha + q50_maize$profit_ha + q90_maize$profit_ha
  maize$area_profitable_perc <- 100 * maize$area_profitable_ha / maize$ha_spam
  maize$actual_profit <- q10_maize$profit_usdha + q50_maize$profit_usdha + q90_maize$profit_usdha
  # plot  
  par(mfrow=c(1,2))
  terra::plot(maize$area_profitable_perc, main=crop)
  terra::plot(maize$actual_profit, breaks=c(0,200,400,600, Inf), main=crop)
  # save raster
  names(maize) <- paste0(crop, '_', names(maize))
  terra::writeRaster(maize, paste0(output_path, 'crop-rasters-final/', crop, '_profit_rasters.tif'), overwrite=T)  
  }




# across crops
all_crops <- terra::rast(Sys.glob(paste0(output_path, 'crop-rasters-final/*_profit_rasters.tif')))  

# total profitable area
all_crops_ha <- all_crops[[grep('_ha_spam', names(all_crops))]]
all_crops_ha <- sum(all_crops_ha, na.rm=T)
all_crops_ha_profit <- all_crops[[grep('area_profitable_ha', names(all_crops))]]                      
all_crops_ha_profit <- sum(all_crops_ha_profit, na.rm=T)
a_p <- 100 * all_crops_ha_profit / all_crops_ha

# crop-area weighted profit
all_crops_ha <- all_crops[[grep('_ha_spam', names(all_crops))]]
all_crops_gm <- all_crops[[grep('actual_profit', names(all_crops))]]                      
w_p <- terra::weighted.mean(all_crops_gm, all_crops_ha, na.rm=T)

# plot
par(mfrow=c(1,2))
terra::plot(a_p, breaks=c(0,20,40,60,80,100))
terra::plot(w_p, breaks=c(0,200,600,Inf))




