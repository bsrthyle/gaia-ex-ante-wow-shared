
# ------------------------------------------------------------------------------
# function to assemble raster layers per crop
# ------------------------------------------------------------------------------

# function to assemble layers
assemble_lyrs <- function(var, is_usd=F, is_profitable=F){  
  # var = c('ya', 'yresp_tha')
  # is_usd = F returns production
  # is_usd = T returns value of production
  # crop prices
  prices <- read.csv(paste0(input_path, 'crop-prices.csv'))
  prices <- aggregate(prices$Value, by=list('crop'=prices$spam), FUN=median, na.rm=T) # median price
  # calculations
  crop_data <- lapply(crop_names, function(crop){
    c_price <- prices[prices$crop == crop,]$x
    area_c <- exante_output[[crop]]
    var_c <- exante_output[[paste0(crop, '_', var)]]
    gm_c <- exante_output[[paste0(crop, '_gm_usha')]]
    gm_c <- terra::ifel(gm_c > 0, 1, NA)
    if(is_profitable==F){
      if(is_usd==F){
        label <- ifelse(var=='ya', '_curr_t', '_add_t')
        var_c <- area_c * var_c; names(var_c) <- paste0(crop, label)
      } else{
        label <- ifelse(var=='ya', '_curr_usd', '_add_usd')
        var_c <- area_c * var_c * c_price; names(var_c) <- paste0(crop, label) }
      var_c
    } else{
      if(is_usd==F){
        label <- ifelse(var=='ya', '_curr_t', '_add_t'); 
        var_c <- area_c * var_c * gm_c; names(var_c) <- paste0(crop, label)
      } else{
        label <- ifelse(var=='ya', '_curr_usd', '_add_usd')
        var_c <- area_c * var_c * c_price * gm_c; names(var_c) <- paste0(crop, label) }
      var_c }
  })
  return(crop_data) }

# ------------------------------------------------------------------------------
# function to summarize rasters as table
# ------------------------------------------------------------------------------

# function to calculate acidic cropland per admin unit
summary_f <- function(country_iso3, var_rast, unit, select_admin){
  folder_name <- ifelse(unit=='_ha', 'acidic-cropland', 
                        ifelse(unit=='_add_t', 'additional-production', 
                          ifelse(unit=='_curr_t', 'current-production', 
                            ifelse(unit=='_add_usd', 'additional-value',
                              ifelse(unit=='_curr_usd', 'current-value', 
                                ifelse(unit=='_add_t_profit', 'additional-production-profitable',
                                  ifelse(unit=='_add_usd_profit', 'additional-value-profitable', 'lime-requirements')))))))
  # select variable
  l_ph_h_hp_crop <- var_rast * l_ph_h_hp; names(l_ph_h_hp_crop) <- paste0(names(l_ph_h_hp_crop), '_l_ph_h_hp')
  l_ph_l_hp_crop <- var_rast * l_ph_l_hp; names(l_ph_l_hp_crop) <- paste0(names(l_ph_l_hp_crop), '_l_ph_l_hp')
  h_ph_h_hp_crop <- var_rast * h_ph_h_hp; names(h_ph_h_hp_crop) <- paste0(names(h_ph_h_hp_crop), '_h_ph_h_hp')
  h_ph_l_hp_crop <- var_rast * h_ph_l_hp; names(h_ph_l_hp_crop) <- paste0(names(h_ph_l_hp_crop), '_h_ph_l_hp')
  acid_soil1 <- c(l_ph_h_hp_crop, l_ph_l_hp_crop, h_ph_h_hp_crop, h_ph_l_hp_crop)
  # select country
  cty <- geodata::gadm(country_iso3, level=2, path=input_path)
  # calculate total cropland per admin unit
  interest_var <- cbind(data.frame(cty[c('COUNTRY', 'NAME_1', 'NAME_2')]), terra::extract(acid_soil1, cty, fun=sum, na.rm=T, ID=F))
  interest_var[is.na(interest_var)] <- 0
  interest_var$total <- round(rowSums(interest_var[names(interest_var) %in% names(acid_soil1)], na.rm=T), 1)
  # calculate crop area per admin unit
  crop_data <- lapply(unique(substr(names(acid_soil1), 1, 4)), function(crop){
    interest_var$crop <- rowSums(interest_var[grep(paste0(crop, '_'), names(interest_var))], na.rm=T)
    names(interest_var)[names(interest_var) == 'crop'] <- paste0(crop, unit)
    interest_var[c('COUNTRY', 'NAME_1', 'NAME_2', paste0(crop, unit))] })
  crop_data <- Reduce(function(x, y) merge(x, y, by=c('COUNTRY', 'NAME_1', 'NAME_2')), crop_data)
  interest_var <- merge(interest_var, crop_data, by=c('COUNTRY', 'NAME_1', 'NAME_2'))
  # calculate area per quadrant
  interest_var$l_ph_h_hp <- rowSums(interest_var[grep('_l_ph_h_hp', names(interest_var))], na.rm=T)
  interest_var$l_ph_l_hp <- rowSums(interest_var[grep('_l_ph_l_hp', names(interest_var))], na.rm=T)
  interest_var$h_ph_h_hp <- rowSums(interest_var[grep('_h_ph_h_hp', names(interest_var))], na.rm=T)
  interest_var$h_ph_l_hp <- rowSums(interest_var[grep('_h_ph_l_hp', names(interest_var))], na.rm=T)
  # screen most acidic admins
  interest_var$high_hp <- round(interest_var$l_ph_h_hp + interest_var$h_ph_h_hp, 1)
  acidic <- interest_var
  if(select_admin=='admin2'){
    acidic$high_hp_perc <- round(100 * acidic$high_hp / acidic$total, 1)
    acidic <- acidic[order(acidic$high_hp_perc, decreasing=T),]
    head(acidic[c('NAME_2', 'total', 'high_hp', 'high_hp_perc')], 10) # top 10 most acidic counties
  } else{
    acidic <- aggregate(acidic[c(4:ncol(acidic))], by=list('COUNTRY'=acidic$COUNTRY, 'NAME_1'=acidic$NAME_1), FUN=sum)
    acidic$high_hp_perc <- round(100 * acidic$high_hp / acidic$total, 1)
    acidic[order(acidic$high_hp_perc, decreasing=T),]
    head(acidic[c('NAME_1', 'total', 'high_hp', 'high_hp_perc')], 10) # top 10 most acidic counties
  }
  write.csv(acidic, paste0(output_path, folder_name, "/", country_iso3, '-', select_admin, ".csv"), row.names=T)
  print(paste0("finished for ", folder_name, ' ... ', select_admin))
  return(acidic) }

# ------------------------------------------------------------------------------
# master function to extract values in one go
# ------------------------------------------------------------------------------

master_f <- function(country_iso3, select_admin, var, is_usd, is_profitable){
  # ----------------------------------------------------
  # user-defined
  # country_iso3 = country code in ISO3 format
  # select_admin = gadm admin for which tables are generated
  # var = variable in the ex-ante output
  # is_usd = T estimate value of production, else estimate production
  # is_profitable = T estimate profitable value of production, else estimate profitable production
  # ----------------------------------------------------
  # calculated
  # unit = unit of the variable of interest -- determines folder path
  # var_rast = raster layers per crop for the variable of interest
  # ----------------------------------------------------  
  if(var=='ya' && is_usd==F && is_profitable==F){
    # total current production
    unit <- '_curr_t' 
  } else if(var=='yresp_tha' && is_usd==F && is_profitable==F){
    # total additional production
    unit <- '_add_t' 
  } else if(var=='ya' && is_usd==T && is_profitable==F){
    # total current value of production
    unit <- '_curr_usd' 
  } else if(var=='yresp_tha' && is_usd==T && is_profitable==F){
    # total additional value of production
    unit <- '_add_usd'
  } else if(var=='yresp_tha' && is_usd==F && is_profitable==T){
    # profitable additional production
    unit <- '_add_t_profit' 
  } else if(var=='yresp_tha' && is_usd==T && is_profitable==T){
    # profitable additional value of production
    unit <- '_add_usd_profit' 
  } else{
    unit <- 'no_unit'
    print("no combination possible")

  }
  
  if(unit != 'no_unit'){
    raster_layers <- terra::rast(assemble_lyrs(var=var, is_usd=is_usd, is_profitable=is_profitable))
    final_table <- summary_f(var_rast=raster_layers, country_iso3=country_iso3, select_admin=select_admin, unit=unit)
    return(final_table) }
  }

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
