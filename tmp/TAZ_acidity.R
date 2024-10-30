
library(sf)


input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/data-output/')
country_iso3 <- "TZA"

cty_1 <- geodata::gadm(country_iso3, level = 1, path = input_path)

admin_3_TZ <- sf::st_read("tmp/tza_admbnda_adm3_20181019.shp")

soils <- terra::aggregate(terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif')), 10, mean, na.rm=T)
l_ph_h_hp <- terra::ifel(soils$ph < 5.5 & soils$hp_sat >= 10, 1, NA); names(l_ph_h_hp) <- 'l_ph_h_hp'
l_ph_l_hp <- terra::ifel(soils$ph < 5.5 & soils$hp_sat < 10, 1, NA); names(l_ph_l_hp) <- 'l_ph_l_hp'
h_ph_h_hp <- terra::ifel(soils$ph >= 5.5 & soils$hp_sat >= 10, 1, NA); names(h_ph_h_hp) <- 'h_ph_h_hp'
h_ph_l_hp <- terra::ifel(soils$ph >= 5.5 & soils$hp_sat < 10, 1, NA); names(h_ph_l_hp) <- 'h_ph_l_hp' 

high_acidity <- terra::ifel((soils$ph < 5.5 & soils$hp_sat >= 10)|(soils$ph >= 5.5 & soils$hp_sat >= 10), 1, NA)
# clip to Tanzania
l_ph_h_hp <- terra::crop(l_ph_h_hp, cty_1, mask = TRUE)
l_ph_l_hp <- terra::crop(l_ph_l_hp, cty_1, mask=TRUE)
h_ph_h_hp <- terra::crop(h_ph_h_hp, cty_1, mask = TRUE)
h_ph_l_hp <- terra::crop(h_ph_l_hp, cty_1, mask=TRUE)


terra::plot(l_ph_h_hp)
terra::plot(h_ph_h_hp)
terra::plot(total_crop_land)


profit <- 'year1'
ya <- 1
cp <- 1
lp <- 100
dr <- 0.1
q <- "spam_"
exante_output <- terra::rast(Sys.glob(paste0(input_path, 'profit_sensitivity/*_',q, profit, '_yield_', ya, '_cprice_', cp, '_lprice_', lp, '_discrate_', dr,'.tif')))


# filter all rasters that doesn't contain _ in the name
crop_area <- exante_output[[grep("_", names(exante_output), invert = TRUE)]]

crop_area_tz <- terra::crop(crop_area, cty_1, mask = TRUE)
terra::plot(crop_area_tz)
# sum all the crop areas
total_crop_land <- sum(crop_area_tz, na.rm = TRUE)
terra::plot(total_crop_land)

crop_land_l_ph_h_hp <- (l_ph_h_hp*total_crop_land)
crop_land_h_ph_h_hp <- (h_ph_h_hp*total_crop_land)
acidic_crop_land <- crop_land_l_ph_h_hp + crop_land_h_ph_h_hp
terra::plot(crop_land_h_ph_h_hp)

# iringa and kilolo 
iringa_kilolo <- admin_3_TZ[admin_3_TZ$ADM2_EN == "Iringa" | admin_3_TZ$ADM3_EN == "Kilolo",]

terra::plot(iringa_kilolo)

iringa_kilolo_crop_land <- terra::crop(total_crop_land, iringa_kilolo, mask = TRUE)
irringa_kilolo_acidic_crop_land <- terra::crop(high_acidity, iringa_kilolo, mask = TRUE)
terra::plot(iringa_kilolo_crop_land)
terra::plot(irringa_kilolo_acidic_crop_land)


soil_ph <- terra::crop(soils$ph, cty_1, mask=T)
terra::plot(soil_ph, main = "Soil pH", col = wesanderson::wes_palette("Zissou1Continuous", 10))
terra::plot(iringa_kilolo)


terr::