
# ------------------------------------------------------------------------------

# directories
input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/data-output/')

# load rasters

# number_of_farms instead of crop_area..
# number_of_farms <- rural_populations * 0.7 or ask deo

yield_factor <- 2
lp <- 100

crop_area <- terra::rast(paste0(input_path, "spam_harv_area_processed.tif"))
q10 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_q10_year1_yield_',yield_factor,'_cprice_1_lprice_',lp,'_discrate_0.1.tif')))
q50 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_q50_year1_yield_',yield_factor,'_cprice_1_lprice_',lp,'_discrate_0.1.tif')))
q90 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_q90_year1_yield_',yield_factor,'_cprice_1_lprice_',lp,'_discrate_0.1.tif')))

# select variables of interest
q10 <- c(crop_area, q10[[grep('_return_usha|_gm_usha|_roi_usha', names(q10))]])
q50 <- c(crop_area, q50[[grep('_return_usha|_gm_usha|_roi_usha', names(q50))]])
q90 <- c(crop_area, q90[[grep('_return_usha|_gm_usha|_roi_usha', names(q90))]])

for(crop in names(crop_area)){
  
  print(crop)
  # subset per crop and do calculations
  q10_crop <- q10[[grep(crop, names(q10))]]
  q10_crop$profit_class <- terra::ifel(q10_crop[[grep('_gm_usha', names(q10_crop))]] > 0, 1, 0)
  q10_crop$profit_ha <- 0.1 * q10_crop[[crop]] * q10_crop$profit_class
  q10_crop$profit_usdha <- 0.1 * q10_crop[[grep('_gm_usha', names(q10_crop))]] * q10_crop$profit_class
  q50_crop <- q50[[grep(crop, names(q50))]]
  q50_crop$profit_class <- terra::ifel(q50_crop[[grep('_gm_usha', names(q50_crop))]] > 0, 1, 0)
  q50_crop$profit_ha <- 0.8 * q50_crop[[crop]] * q50_crop$profit_class
  q50_crop$profit_usdha <- 0.8 * q50_crop[[grep('_gm_usha', names(q50_crop))]] * q50_crop$profit_class
  q90_crop <- q90[[grep(crop, names(q90))]]
  q90_crop$profit_class <- terra::ifel(q90_crop[[grep('_gm_usha', names(q90_crop))]] > 0, 1, 0)
  q90_crop$profit_ha <- 0.1 * q90_crop[[crop]] * q90_crop$profit_class
  q90_crop$profit_usdha <- 0.1 * q90_crop[[grep('_gm_usha', names(q90_crop))]] * q90_crop$profit_class
  # sum area across quantiles per crop
  crop_ <- crop_area[[crop]]
  names(crop_) <- 'ha_spam'
  crop_$area_profitable_ha <- q10_crop$profit_ha + q50_crop$profit_ha + q90_crop$profit_ha
  crop_$area_profitable_perc <- (crop_$area_profitable_ha) / (crop_$ha_spam)*100
  crop_$actual_profit <- q10_crop$profit_usdha + q50_crop$profit_usdha + q90_crop$profit_usdha
  # plot  
  par(mfrow=c(1,2))
  terra::plot(crop_$area_profitable_perc, main=crop)
  terra::plot(crop_$ha_spam)
  terra::plot(crop_$actual_profit, breaks=c(0,200,400,600, Inf), main=crop)
  # save raster
  names(crop_) <- paste0(crop, '_', names(crop_))
  terra::writeRaster(crop_, paste0(output_path, 'crop-rasters-final/', crop,'_',lp,'_',yield_factor, '_profit_rasters.tif'), overwrite=T)  
  }




# across crops
all_crops <- terra::rast(Sys.glob(paste0(output_path, 'crop-rasters-final/','*_',lp,'_',yield_factor,'_profit_rasters.tif')))

par(mfrow=c(1,2))
terra::plot(all_crops$MAIZ_actual_profit, breaks=c(0,200,600,Inf))
terra::plot(all_crops$MAIZ_area_profitable_perc, breaks=c(0,20,40,60,80,100))

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
terra::plot(w_p, breaks=c(0,200,400,600,Inf))




