
# ------------------------------------------------------------------------------

# directories
input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/data-output/')

# ------------------------------------------------------------------------------

# keys 
fao <- data.frame(spam = c("MAIZ", "SORG", "BEAN", "CHIC", 'LENT', "WHEA", "BARL", "ACOF", "RCOF", 'PMIL', 'SMIL', 'POTA', 'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA'), 
                  Item = c('Maize (corn)', "Sorghum", "Beans, dry", "Chick peas, dry", 'Lentils, dry', "Wheat", "Barley", "Coffee, green", "Coffee, green", 'Millet', 'Millet', 'Potatoes', 'Sweet potatoes', 'Cassava, fresh', 'Cow peas, dry', 'Pigeon peas, dry', 'Soya beans', 'Groundnuts, excluding shelled', 'Sugar cane', 'Cotton seed', 'Cocoa beans', 'Tea leaves', 'Unmanufactured tobacco'))
crops_df <- read.csv(paste0(input_path, 'ecocrop_parameters_hp.csv'))[-c(1)]
ctype_df <- read.csv(paste0(input_path, 'crop_types.csv'))[-c(1)]
crops_df <- merge(crops_df, ctype_df, by.x='spam', by.y='crop')
crops_df <- merge(crops_df, fao, by='spam')
country <- data.frame(iso3 = c('AGO', 'BEN', 'BWA', 'BFA', 'BDI', 'CMR', 'CAF', 'TCD', 'COG', 'COD', 'GNQ', 'ERI', 'SWZ', 'ETH', 'GAB', 'GMB', 'GHA', 'GIN', 'GNB', 'CIV', 'KEN', 'LSO', 'LBR', 'MDG', 'MWI', 'MLI', 'MRT', 'MOZ', 'NAM', 'NER', 'NGA', 'RWA', 'SEN', 'SLE', 'SOM', 'SSD', 'SDN', 'SWZ', 'TZA', 'TGO', 'UGA', 'ZAF', 'ZMB', 'ZWE'), 
                      Area =  c('Angola', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 'Cameroon', 'Central African Republic', 'Chad', 'Congo', 'DRC', 'Equatorial Guinea', 'Eritrea', 'Swaziland', 'Ethiopia', 'Gabon', 'Gambia', 'Ghana', 'Guinea', 'Guinea-Bissau', 'Côte d\'Ivoire', 'Kenya', 'Lesotho', 'Liberia', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique', 'Namibia', 'Niger', 'Nigeria', 'Rwanda', 'Senegal', 'Sierra Leone', 'Somalia', 'Sudan (former)', 'Sudan (former)', 'Swaziland', 'United Republic of Tanzania', 'Togo', 'Uganda', 'South Africa', 'Zambia', 'Zimbabwe'))

# ------------------------------------------------------------------------------

# loss
resp_hp <- terra::rast(Sys.glob(paste0(input_path, 'ecocrop_yieldloss/hp_crop_suitability_*_0.tif'))) 
names(resp_hp) <- gsub("_hp", "", names(resp_hp))
resp_hp <- terra::aggregate(resp_hp, 10, fun='mean', na.rm=T)
resp_ph <- terra::rast(Sys.glob(paste0(input_path, 'ecocrop_yieldloss/ph_crop_suitability_*_0.tif')))
names(resp_ph) <- gsub("_ph", "", names(resp_ph))
resp_ph <- terra::aggregate(resp_ph, 10, fun='mean', na.rm=T)

# spam area (yield replaced with quantile raster)
crop_area <- terra::rast(paste0(input_path, "spam_harv_area_processed.tif"))

# price
fao_price <- read.csv(paste0(input_path, 'FAOSTAT_data_en_4-24-2023.csv'))
fao_price <- subset(fao_price, Year > 2015 & Year <= 2020)
fao_price <- subset(fao_price, Item %in% crops_df$Item & Area %in% country$Area)
fao_price <- merge(fao_price, crops_df, by='Item', all.y=T)
fao_price <- merge(fao_price, country, by='Area', all.x=T)
fao_price <- fao_price[c(1, 2, 10, 12, 13, 14, 17, 18, 19)]
write.csv(fao_price, paste0(input_path, 'crop-prices.csv'))
crop_price <- aggregate(fao_price$Value, by=list('crop'=fao_price$spam), FUN=median, na.rm=T) # median price

# lime per tas
merlos <- terra::aggregate(terra::rast(paste0(input_path, 'caco3_merlos.tif')), 10, mean, na.rm=T)
merlos_m <- terra::aggregate(terra::rast(paste0(input_path, 'caco3_merlos_maintenance.tif')), 10, mean, na.rm=T)

# lime per crop
soil <- terra::aggregate(terra::rast(paste0(input_path, 'soilgrids_properties_all.tif')), 10, 'mean', na.rm=T)
lr <- function(crop, lime_method) {
  c_subset <- crops_df[crops_df$spam == crop,]
  lime_tha <- lime_method[[grep(paste0("_", c_subset$ac_sat), names(lime_method))]]
  area <- crop_area[[crop]]
  area <- terra::ifel(area > 0, 1, NA)
  soil_hp <- terra::ifel(soil$hp_sat > c_subset$ac_sat, 1, NA)
  lime_tha <- lime_tha * soil_hp * area
  names(lime_tha) <- crop
  return(lime_tha)}
lr_crops <- terra::rast(lapply(crops_df$spam, function(crp){lr(crop=crp, lime_method=merlos)}))
lr_m_crops <- terra::rast(lapply(crops_df$spam, function(crp){lr(crop=crp, lime_method=merlos_m)}))

# ------------------------------------------------------------------------------

# returns
returns <- function(crop, crop_yield, yield_resp, crop_price, yield_f){
  c_subset <- crops_df[crops_df$spam == crop,]
  actual_yield <- crop_yield
  names(actual_yield) <- paste0(crop, '_', names(actual_yield), '_ya')
  yield_loss <- yield_resp[[crop]]
  names(yield_loss) <- paste0(crop, '_loss')
  yield_level <- actual_yield * yield_f
  yield_resp_tha <- (yield_level / yield_loss) - yield_level 
  yield_resp_tha <- terra::subst(yield_resp_tha, 0, NA)
  names(yield_resp_tha) <- paste0(crop, '_yresp_tha')
  return_usha <- yield_resp_tha * crop_price
  names(return_usha) <- paste0(crop, '_return_usha')
  return(c(actual_yield, yield_loss, yield_resp_tha, return_usha))
}

# costs (year 1)
costs <- function(crop, lime_method, lime_price) {
  # c_subset <- subset(crops_df, spam == crop) 
  # lime_tha <- lime_method[[grep(paste0("_", c_subset$ac_sat), names(lime_method))]]
  lime_tha <- lime_method[[crop]]
  names(lime_tha) <- paste0(crop, '_lr_tha')
  lime_usha <- lime_tha * lime_price
  names(lime_usha) <- paste0(crop, '_cost_usha')
  return(c(lime_tha, lime_usha))
}

# number of years
nyears_f <- function(crop, lime_year1, lime_maint){
  c_subset <- subset(crops_df, spam == crop) 
  # lime_1 <- lime_year1[[grep(paste0("_", c_subset$ac_sat), names(lime_year1))]]
  # lime_m <- lime_maint[[grep(paste0("_", c_subset$ac_sat), names(lime_maint))]]
  lime_1 <- lime_year1[[crop]]
  lime_m <- lime_maint[[crop]]
  nyears <- round(lime_1/lime_m, 0); names(nyears) <- 'nyears'
  return(nyears)
}

# profitability
profit <- function(crop, crop_yield, yield_resp, yf, crop_price, returns_f=c('year1', 'npv', 'equilibrium'), discount_rate, lime_method, lime_m_method, lime_price, nyrs){
  if(returns_f == 'year1'){                                         
    # year 1 return & year 1 cost
    return <- returns(crop, crop_yield, yield_resp, yield_f=yf, crop_price)  
    cost <- costs(crop, lime_method, lime_price)           
  } else if(returns_f == 'npv'){                                    
    # npv return & year 1 cost
    return_1 <- returns(crop, crop_yield, yield_resp, yield_f=yf, crop_price) # output not as per returns() function
    return <- limer::NPV_lime(return_1[[paste0(crop, "_return_usha")]], nyears=nyrs, discount_rate=discount_rate)   
    names(return) <- paste0(crop, "_return_usha")
    cost <- costs(crop, lime_method, lime_price)           
  } else if(returns_f == 'equilibrium'){                             
    # year 1 return & maintenance cost
    return <- returns(crop, crop_yield, yield_resp, yield_f=yf, crop_price)
    cost <- costs(crop, lime_m_method, lime_price)           
  }
  gm <- return[[paste0(crop, "_return_usha")]] - cost[[paste0(crop, "_cost_usha")]]; names(gm) <- paste0(crop, '_gm_usha')
  roi <- return[[paste0(crop, "_return_usha")]] / cost[[paste0(crop, "_cost_usha")]]; names(roi) <- paste0(crop, '_roi_usha')
  return(c(return, cost, gm, roi))
}

# ------------------------------------------------------------------------------

# run profitability 
profit_calc <- function(profit_type='year1', cprice_factor=1, yield_factor=1, lime_price=100, discount_rate=0.1){
  # profit_type = c('year1', npv', 'equilibrium)
  for(r_f in profit_type){
    print(r_f)
    for(crop in unique(crops_df$spam)){
      print(crop)
      area_ha <- crop_area[[crop]]
      c_price <- crop_price[crop_price$crop==crop,]$x * cprice_factor
      c_nyrs <- nyears_f(crop, lr_crops, lr_m_crops)
      crop_yield_q <- terra::rast(Sys.glob(paste0(input_path, "yield_quantiles/", crop, "_spam_yield_quantiles.tif"))) 
      profit_quantile <- lapply(names(crop_yield_q), function(q){
        print(q)
        yield_q <- crop_yield_q[[q]]
        filename <- paste0(input_path, 'profit_sensitivity/', crop, '_', q, '_', r_f, '_yield_', yield_factor, '_cprice_', cprice_factor, '_lprice_', lime_price, '_discrate_', discount_rate, '.tif')
        if(r_f == 'npv'){                                         
          crop1 <- profit(returns_f=r_f, crop, crop_yield=yield_q, yield_resp=resp_hp, yf=yield_factor, crop_price=c_price, discount_rate=discount_rate, lime_method=lr_crops, nyrs=c_nyrs, lime_price=lime_price) 
        } else{
          crop1 <- profit(returns_f=r_f, crop, crop_yield=yield_q, yield_resp=resp_hp, yf=yield_factor, crop_price=c_price, discount_rate=discount_rate, lime_method=lr_crops, lime_m_method=lr_m_crops, lime_price=lime_price) 
        }
        crop2 <- c(area_ha, crop1)
        terra::writeRaster(crop2, filename, overwrite=T) })
    }
  }  
}

# ------------------------------------------------------------------------------

# user defined parameters
ya <- 1
cp <- 1
lp <- seq(0, 100, 50)
dr <- 0.1

# run sensitivity analysis
params <- expand.grid(ya=ya, cp=cp, lp=lp, dr=dr)
for(i in 1:nrow(params)){
  p <- params[i,]    
  profit_calc(profit_type='year1', yield_factor=p$ya, cprice_factor=p$cp, lime_price=p$lp, discount_rate=p$dr)
  }


# ... post-processing ...

q10 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_q10_year1_yield_1_cprice_1_lprice_100_discrate_0.1.tif')))
q90 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_q90_year1_yield_1_cprice_1_lprice_100_discrate_0.1.tif')))

q10 <- q10[[c('MAIZ', 'MAIZ_gm_usha')]]
q10$profit_class <- terra::ifel(q10$MAIZ_gm_usha <= 0, 0, 1)
q10$area_prof <- q10$MAIZ * 0.1 * q10$profit_class

q90 <- q90[[c('MAIZ', 'MAIZ_gm_usha')]]
q90$profit_class <- terra::ifel(q90$MAIZ_gm_usha <= 0, 0, 1)
q90$area_prof <- q90$MAIZ * 0.1 * q90$profit_class


names(q10)
par(mfrow=c(1,2))
terra::plot(q10$area_prof)
terra::plot(q90$area_prof)


# example to load respective rasters
# yf1 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_year1_yield_1_cprice_1_lprice_100_discrate_0.1.tif')))
# yf1 <- yf1[[grep('_gm_usha', names(yf1))]]
# yf1 <- terra::mean(yf1, na.rm=T); names(yf1) <- 'yf1'
# yf2 <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_year1_yield_2_cprice_1_lprice_100_discrate_0.1.tif')))
# yf2 <- yf2[[grep('_gm_usha', names(yf2))]]
# yf2 <- terra::mean(yf2, na.rm=T); names(yf2) <- 'yf2'
# yf <- c(yf1, yf2)
# terra::plot(yf)

# ------------------------------------------------------------------------------
