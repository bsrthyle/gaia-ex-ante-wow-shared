
# ------------------------------------------------------------------------------

# directories
input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/data-output/')

# ------------------------------------------------------------------------------

# soil-grids
sprops_cropland <- terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif'))
sprops_cropland <- sprops_cropland[[c(2, 10)]]
names(sprops_cropland) <- c("ph", "hp")

# crops
crops_df <- read.csv(paste0(input_path, 'ecocrop_parameters_ph.csv')) # load ph because there is no rice pars

# ------------------------------------------------------------------------------

# yield-loss-function
ecocrop_f <- function(crop, response, properties, parameter=0) {
  crops_df <- read.csv(paste0(input_path, 'ecocrop_parameters_', response, '.csv'))
  subset_crops <- subset(crops_df, spam == crop)
  ecocrop_crop <- Recocrop::ecocropPars(subset_crops$ecocrop)
  sprop <- properties[[response]]
  if(response == 'hp'){
    svar <- "max_ac_sat"
    p <- subset_crops[[svar]]
    pars1 <- c(-Inf, -Inf, subset_crops$ac_sat, max_ac_sat=p)
  } else { # ph
    svar <- "min_ph"
    p <- subset_crops[[svar]]
    pars1 <- c(min_ph=p, subset_crops$max_ph, Inf, Inf)
  }
  pars <- pars1
  pars[[svar]] <- pars1[[svar]] - parameter
  ecocrop_crop$parameters <- cbind(ecocrop_crop$parameters, pars)
  colnames(ecocrop_crop$parameters)[6] <- "response"
  model <- Recocrop::ecocrop(ecocrop_crop)
  # Recocrop::plot(model)
  Recocrop::control(model, get_max=T)
  yield_loss <- Recocrop::predict(model, response=sprop)
  yield_loss <- terra::clamp(yield_loss, 0.2, 1)
  names(yield_loss) <- paste0(crop, '_', response)
  return(yield_loss)
}

# ------------------------------------------------------------------------------

# run
param <- 0
for(crop in unique(crops_df$spam)){
  for(resp in c('hp', 'ph')){
    print(paste0(crop, '_', resp))
    yield_loss <- ecocrop_f(crop=crop, response=resp, properties=sprops_cropland, parameter=param)
    terra::writeRaster(yield_loss, paste0(input_path, 'ecocrop_yieldloss/', resp, '_crop_suitability_', crop, '_', param, '.tif'), overwrite=T)
  }
}

# ------------------------------------------------------------------------------
