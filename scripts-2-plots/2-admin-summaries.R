
# ------------------------------------------------------------------------------
# directories
# ------------------------------------------------------------------------------

input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/data-output/')
source('# table-functions.R')

# ------------------------------------------------------------------------------
# user selected parameters
# ------------------------------------------------------------------------------

# set country etc.
country_iso3 <- 'KEN'
select_admin <- c('admin1', 'admin2')
crop_names <- c("MAIZ", "SORG", "BEAN", "CHIC", "LENT", "WHEA", "BARL", "ACOF", "RCOF", "PMIL", "SMIL", "POTA", "SWPO", "CASS", "COWP", "PIGE", "SOYB", "GROU", "SUGC", "COTT", "COCO", "TEAS", "TOBA")

# load raster data
profit <- 'year1'
ya <- 1
cp <- 1
lp <- 100
dr <- 0.1
exante_output <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_', profit, '_yield_', ya, '_cprice_', cp, '_lprice_', lp, '_discrate_', dr,'.tif')))

# ------------------------------------------------------------------------------
# soil acidity quadrants
# ------------------------------------------------------------------------------

soils <- terra::aggregate(terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif')), 10, mean, na.rm=T)
l_ph_h_hp <- terra::ifel(soils$ph < 5.5 & soils$hp_sat >= 10, 1, NA); names(l_ph_h_hp) <- 'l_ph_h_hp'
l_ph_l_hp <- terra::ifel(soils$ph < 5.5 & soils$hp_sat < 10, 1, NA); names(l_ph_l_hp) <- 'l_ph_l_hp'
h_ph_h_hp <- terra::ifel(soils$ph >= 5.5 & soils$hp_sat >= 10, 1, NA); names(h_ph_h_hp) <- 'h_ph_h_hp'
h_ph_l_hp <- terra::ifel(soils$ph >= 5.5 & soils$hp_sat < 10, 1, NA); names(h_ph_l_hp) <- 'h_ph_l_hp' 

# ------------------------------------------------------------------------------
# acidic cropland
# ------------------------------------------------------------------------------

# crop area
area_crop <- terra::rast(paste0(input_path, "spam_harv_area_processed.tif"))
for(sa in select_admin){ crop_area <- summary_f(country_iso3=country_iso3, var_rast=area_crop, unit='_ha', select_admin=sa) }

# ------------------------------------------------------------------------------
# production & value of production
# ------------------------------------------------------------------------------

# combinations
var <- c('ya', 'yresp_tha')   # current vs additional
is_usd <- c(T, F)             # t vs usd
is_profitable <- c(T, F)      # all vs profitable
all_combs <- expand.grid('country_iso3'=country_iso3, 'select_admin'=select_admin, 'var'=var, 'is_usd'=is_usd, 'is_profitable'=is_profitable)

# run function
for(i in 1:nrow(all_combs)){
  params <- all_combs[i,]
  master_f(params$country_iso3, params$select_admin, params$var, params$is_usd, params$is_profitable)
  }

# ------------------------------------------------------------------------------
# lime requirements
# ------------------------------------------------------------------------------

lime_data <- lapply(crop_names, function(crop){
  area_c <- exante_output[[crop]]
  lime_c <- exante_output[[paste0(crop, '_lr_tha')]]
  lime_c <- area_c * lime_c; names(lime_c) <- paste0(crop, '_lr_tha')
  lime_c })
for(sa in select_admin){ lime_rate <- summary_f(country_iso3=country_iso3, var_rast=terra::rast(lime_data), unit='_lr_tha', select_admin=sa) }
  
# correct unrealistic high lime rates? -- ask joao
# problem with the modelling of LR?
# lr_crops[lr_crops > 4] <- 2.5
# lr_crops[lr_crops > 2.5 & lr_crops <= 4] <- 1.5

# ------------------------------------------------------------------------------
# the end
# ------------------------------------------------------------------------------
