input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/tmp/Kenya-maps/')
output_path2 <- paste0(here::here(), '/data-output/')
library(tidyverse)

ken<- geodata::gadm("TZA", level = 1, path = input_path)
png(paste0(output_path, "/figure-1-targetting-variables-KEN.png"), units="in", width=9, height=4.75*2, res=1000)
par(mfrow=c(2,2), mar=c(4,4,2,1), xaxs='i', yaxs='i', mgp=c(2.5,1,0))
# plot 1
soil_data <- terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif'))
ph <- soil_data[['ph']]
ph <- terra::crop(ph, ken, mask=T)
pal <- colorRampPalette(c("darkred", "firebrick", "gold", "yellowgreen", "#C6DCC9"))
terra::plot(ken, mar=c(4,3,2,1), clip=F, col='white', main='Soil pH', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(ph, breaks=c(0, 4.5, 5.0, 5.5, 6.0, 6.5, Inf), legend=F, col=pal(6), main='', add=T)
legend(33.9,-1.5, bty='n', cex=1, ncol=1, box.col="white", title="", legend=c('< 4.5', '4.5 - 5.0', '5.0 - 5.5', '5.5 - 6.0', '6.0 - 6.5', '> 6.5'), fill=pal(6), horiz=F)
terra::plot(ken, axes=F, add=T)

# plot 2
# hp <- terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif'))
hp <- soil_data[['hp_sat']]
hp <- terra::crop(hp, ken, mask=T)
pal <- colorRampPalette(c('#FBF5E7', 'yellow', "gold", "firebrick", "darkred"))
terra::plot(ken, mar=c(4,2,2,1), clip=F, col='white', main='Acidity saturation (% of ECEC)', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(hp, breaks=c(0, 10, 20, Inf), col=pal(3), legend=F, main='', add=T)
legend(34,-2.1, bty='n', cex=1.2, ncol=1, box.col="white", title="", legend=c('< 10', '10 - 20', '> 20'), fill=pal(3), horiz=F)
terra::plot(ken, axes=F, add=T)
# plot 3
both <- c(ph, hp)
both_df <- terra::spatSample(both, 200000, 'regular')
#hp_val <- mean(both_df$hp_sat, na.rm=T)
#ph_val <- mean(both_df$ph, na.rm=T)
hp_val <- 10
ph_val <- 5.5
plot(both_df$ph, both_df$hp_sat, ylim=c(0,100), cex.lab=1.5, cex.axis=1.4, xlim=c(4.5, 8.5), xlab='pH in water', ylab='Acidity saturation (% of ECEC)')
grid(nx=NULL, ny=NULL)
abline(h=hp_val, col=2, lty=2)
abline(v=ph_val, col=2, lty=2)
points(both_df$ph[both_df$ph < ph_val & both_df$hp_sat > hp_val], both_df$hp_sat[both_df$ph < ph_val & both_df$hp_sat > hp_val], col="gold", cex=0.8)
points(both_df$ph[both_df$ph < ph_val & both_df$hp_sat <= hp_val], both_df$hp_sat[both_df$ph < ph_val & both_df$hp_sat <= hp_val], col="#22A884FF", cex=0.8)
points(both_df$ph[both_df$ph >= ph_val & both_df$hp_sat > hp_val], both_df$hp_sat[both_df$ph >= ph_val & both_df$hp_sat > hp_val], col="#2A788EFF", cex=0.8)
points(both_df$ph[both_df$ph >= ph_val & both_df$hp_sat <= hp_val], both_df$hp_sat[both_df$ph >= ph_val & both_df$hp_sat <= hp_val], col="#414487FF", cex=0.8)
abline(h=hp_val, col=2, lty=1, lwd=2)
abline(v=ph_val, col=2, lty=1, lwd=2)
legend('topright', bty='n', cex=1.2, ncol=1, box.col="white", title="", legend=c('< pH > Hp', '< pH < Hp', '> pH > Hp', '> pH < Hp'), fill=c("gold", "#22A884FF", "#2A788EFF", "#D5D5E3"), horiz=F)
box()
# plot 4
both$cluster <- terra::ifel(both$ph < ph_val & both$hp_sat > hp_val, 1, NA)
both$cluster <- terra::ifel(both$ph < ph_val & both$hp_sat <= hp_val, 2, both$cluster)
both$cluster <- terra::ifel(both$ph >= ph_val & both$hp_sat > hp_val, 3, both$cluster)
both$cluster <- terra::ifel(both$ph >= ph_val & both$hp_sat <= hp_val, 4, both$cluster)
terra::plot(ken, mar=c(4,2,2,1), clip=F, col='white', main='Targetting clusters', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(both$cluster, col=c("gold", "#22A884FF", "#2A788EFF", "#D5D5E3"), legend=F, main='', add=T)
legend(33.7,-1.8, bty='n', cex=1.2, ncol=1, box.col="white", title="", legend=c('< pH > Hp', '< pH < Hp', '> pH > Hp', '> pH < Hp'), fill=c("gold", "#22A884FF", "#2A788EFF", "#D5D5E3"), horiz=F)
terra::plot(ken, axes=F, add=T)

dev.off()

# ------------------------------------------------------------------------------
# cropland and population
# ------------------------------------------------------------------------------

both <- terra::aggregate(both, 10, mean)
both$cluster <- terra::ifel(both$ph < ph_val & both$hp_sat > hp_val, 1, NA)
both$cluster <- terra::ifel(both$ph < ph_val & both$hp_sat <= hp_val, 2, both$cluster)
both$cluster <- terra::ifel(both$ph >= ph_val & both$hp_sat > hp_val, 3, both$cluster)
both$cluster <- terra::ifel(both$ph >= ph_val & both$hp_sat <= hp_val, 4, both$cluster)

ha <- terra::rast(paste0(input_path, 'spam_harv_area_processed.tif'))
ha <- terra::crop(ha, ken, mask=T)
ha <- terra::resample(ha, hp)

low_ph <- terra::ifel(both$cluster == 1 | both$cluster == 2, 1, NA)
low_ph <- ha * low_ph
low_ph <- terra::global(low_ph, sum, na.rm=T)
low_ph$crop <- row.names(low_ph); row.names(low_ph) <- NULL
colnames(low_ph)[1] <- 'pH'

high_hp <- terra::ifel(both$cluster == 1 | both$cluster == 3, 1, NA)
high_hp <- ha * high_hp
high_hp <- terra::global(high_hp, sum, na.rm=T)
high_hp$crop <- row.names(high_hp); row.names(high_hp) <- NULL
colnames(high_hp)[1] <- 'Hp'

ctype_df <- read.csv(paste0(input_path, 'crop_types.csv'))[-c(1)]
final <- merge(low_ph, high_hp, by='crop')
final <- merge(final, ctype_df, by='crop')
final <- aggregate(final[c(2,3)], by=list('type'=final$type), FUN=sum)
aa <- reshape2::melt(final, id.var='type')
aa$type <- factor(aa$type, levels=c('Cereal', 'Legume', 'RTBs', 'Commodity'))
aa <- aa[order(aa$type),]

png(paste0(output_path, "/figure-1-targetting-variables-2.png"), units="in", width=5.25, height=5, res=1000)
par(mfrow=c(1,1), mar=c(5,5,1,1), cex.lab=1.4, cex.axis=1.3, las=1)
col <- c("#440154FF", "#440154FF", "#31688EFF", "#31688EFF", "#35B779FF", "#35B779FF", "#FDE725FF", "#FDE725FF")
b <- barplot(aa$value/1000000, beside=T, col=col, ylim=c(0,1.8), xlab = "Targetting property", ylab = "Crop type area (M ha)")
grid(nx=NULL, ny=NULL)
b <- barplot(aa$value/1000000, names=aa$variable, beside=T, col=col, ylim=c(0,1.8), xlab = "Targetting property", ylab = "Crop type area (M ha)", add=T)
text(x=b, y=aa$value/1000000 + 0.05, labels=round(aa$value/1000000, 2))
legend('topright', cex=1.1, legend=unique(aa$type), pch=22, pt.bg=c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"))
box()        
dev.off()



# additional value of production 
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

exante_output <- terra::rast(Sys.glob(
  paste0(
    input_path,
    'profit_sensitivity/*_',
    "spam_",
    'year1',
    '_yield_',
    1,
    '_cprice_',
    1,
    '_lprice_',
    100,
    '_discrate_',
    0.1,
    '.tif'
  )
))

crop_yield_loss<-exante_output[[grep("_loss",names(exante_output))]]
crop_yield_loss<-terra::crop(crop_yield_loss,ken,mask=TRUE)
crop_yield_loss <- (1-crop_yield_loss)*100
crop_yield_loss <- terra::ifel(crop_yield_loss !=0,crop_yield_loss, NA)

# mean crop yield loss
mean_crop_yield_loss<-terra::app(crop_yield_loss,mean,na.rm=TRUE)


png(paste0(output_path, "/figure-1-Crop-loss.png"), units="in", width=8, height=8, res=1000)

pal <- colorRampPalette(c('#FBF5E7', 'yellow', "gold", "firebrick", "darkred"))
terra::plot(ken, mar=c(4,2,2,1), clip=F, col='white', main='', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(mean_crop_yield_loss, breaks=c(0,0.0001, 10, 20,40,60, Inf), col=pal(3), legend=F, main='', add=T)
legend(33.6,-2.1, bty='n', cex=1.2, ncol=1, box.col="white", title="Crop Yield\nloss (%)", legend=c('No loss', '0 - 20', '20 - 40', '40 - 60', '> 60'), fill=pal(5), horiz=F)
terra::plot(ken, axes=F, add=T)

dev.off()


# ------------------------------------------------------------------------------
# Crop area weighted gross margin
# ------------------------------------------------------------------------------

exante_output_kenya <- terra::crop(exante_output, ken, mask = TRUE)
crop_area_k <- exante_output_kenya[[grep("_", names(exante_output_kenya), invert = TRUE)]]
all_crop_gm <- exante_output_kenya[[grep('_gm_', names(exante_output_kenya))]]

area_weighted_gm <- terra::weighted.mean(all_crop_gm, crop_area_k, na.rm=T)

area_weighted_gm <- terra::ifel(area_weighted_gm >= 0, area_weighted_gm, NA)

png(paste0(output_path, "/figure-1-area-weighted_gm.png"), units="in", width=8, height=8, res=1000)

pal <- colorRampPalette(c('#FBF5E7', 'yellow', "gold", "firebrick", "darkred"))
terra::plot(ken, mar=c(4,2,2,1), clip=F, col='white', main='', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(mean_crop_yield_loss, breaks=c(0,0.0001, 10, 20,40,60, Inf), col=pal(3), legend=F, main='', add=T)
legend(33.6,-2.1, bty='n', cex=1.2, ncol=1, box.col="white", title="Crop Yield\nloss (%)", legend=c('No loss', '0 - 20', '20 - 40', '40 - 60', '> 60'), fill=pal(5), horiz=F)
terra::plot(ken, axes=F, add=T)

dev.off()

# ------------------------------------------------------------------------------
# Table for Harvested area, curentproduction, current production value, additional production, additional value of production

# ------------------------------------------------------------------------------

crop_area_ha <- high_hp
crop_area_ha$Hp <- round(crop_area_ha$Hp/1000, 2)
colnames(crop_area_ha)[1] <- 'Harvested Area (1000xha)'

current_production <- readr::read_csv(paste0(output_path2, 'current-production/KEN-admin1.csv'))
# current production select NAME_1 and ends with _h_hp
current_production <- current_production[, grepl('NAME_1$|_h_hp$', names(current_production))]
# remove the last two columns
current_production <- current_production[, -c(ncol(current_production)-1, ncol(current_production))]

# sum the columns
sum_columns <- colSums(current_production[, grepl('_h_hp$', names(current_production))], na.rm = TRUE)
modified_names <- sub("^[^_]+_", "", names(sum_columns))

# Create a data frame with modified column names and sums
sum_columns_df <- data.frame(Column = modified_names, Sum = sum_columns)
sum_columns_df <- data.frame(Column = names(sum_columns), Sum = sum_columns)
row.names(sum_columns_df) <- NULL
sum_columns_df$crop <- sum_columns_df$Column
sum_columns_df$crop <- gsub('_curr_t_l_ph_h_hp|_curr_t_h_ph_h_hp', '', sum_columns_df$crop)

# long to wide
current_production_mton <- sum_columns_df %>% 
  # remove the string before the first underscore
  mutate(Column = sub("^[^_]+_", "", Column)) %>%
  pivot_wider(names_from = Column, values_from = Sum)%>%
  mutate(cp_mton = round((curr_t_l_ph_h_hp + curr_t_h_ph_h_hp)/1000,2))%>%
  select(crop, cp_mton)

# additional production
additional_production0 <- readr::read_csv(paste0(output_path2, 'additional-production/KEN-admin1.csv'))



# long to wide
additional_production_mton <- additional_production0 %>% 
  select(NAME_1, ends_with('_h_hp')) %>%
  # remove the last two columns
  select(-c(ncol(.)-1, ncol(.))) %>%
  # sum the columns excluding the first column
  summarise(across(-1, sum, na.rm = TRUE)) %>%
  mutate(sum = "sum") %>%
  select(sum, everything()) %>%
  # pivot from long to wide
  pivot_longer(-sum, names_to = "Column", values_to = "Sum") %>%
  mutate(crop = gsub('_add_t_l_ph_h_hp|_add_t_h_ph_h_hp', '', Column)) %>%
  # remove the string before the first underscore
  mutate(Column = sub("^[^_]+_", "", Column)) %>%
  pivot_wider(names_from = Column, values_from = Sum)%>%
  mutate(ap_mton = round((add_t_l_ph_h_hp + add_t_h_ph_h_hp)/1000,2))%>%
  select(crop, ap_mton)


# current production value
current_production_value0 <- readr::read_csv(paste0(output_path2, 'current-value/KEN-admin1.csv'))

current_production_value <- current_production_value0 %>% 
  select(NAME_1, ends_with('_h_hp')) %>%
  # remove the last two columns
  select(-c(ncol(.)-1, ncol(.))) %>%
  # sum the columns excluding the first column
  summarise(across(-1, sum, na.rm = TRUE)) %>%
  mutate(sum = "sum") %>%
  select(sum, everything()) %>%
  # pivot from long to wide
  pivot_longer(-sum, names_to = "Column", values_to = "Sum") %>%
  mutate(crop = gsub('_curr_usd_l_ph_h_hp|_curr_usd_h_ph_h_hp', '', Column)) %>%
  # remove the string before the first underscore
  mutate(Column = sub("^[^_]+_", "", Column)) %>%
  pivot_wider(names_from = Column, values_from = Sum)%>%
  mutate(cp_value_musd = round((curr_usd_l_ph_h_hp + curr_usd_h_ph_h_hp)/1000000,2))%>%
  select(crop, cp_value_musd)

# additional value of production
additional_value0 <- readr::read_csv(paste0(output_path2, 'additional-value/KEN-admin1.csv'))

additional_value <- additional_value0 %>% 
  select(NAME_1, ends_with('_h_hp')) %>%
  # remove the last two columns
  select(-c(ncol(.)-1, ncol(.))) %>%
  # sum the columns excluding the first column
  summarise(across(-1, sum, na.rm = TRUE)) %>%
  mutate(sum = "sum") %>%
  select(sum, everything()) %>%
  # pivot from long to wide
  pivot_longer(-sum, names_to = "Column", values_to = "Sum") %>%
  mutate(crop = gsub('_add_usd_l_ph_h_hp|_add_usd_h_ph_h_hp', '', Column)) %>%
  # remove the string before the first underscore
  mutate(Column = sub("^[^_]+_", "", Column)) %>%
  pivot_wider(names_from = Column, values_from = Sum)%>%
  mutate(ap_value_musd = round((add_usd_l_ph_h_hp + add_usd_h_ph_h_hp)/1000000,2))%>%
  select(crop, ap_value_musd)


crops_df <- data.frame(crop=c("MAIZ", "SORG", "BEAN", "CHIC", 'LENT', "WHEA", "BARL", "ACOF", "RCOF", 'PMIL', 'SMIL', 'POTA', 'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA'), 
                       crop_name=c('Maize', "Sorghum", "Bean", "Chick pea", 'Lentil', "Wheat", "Barley", "Coffee arabica", "Coffee robusta", 'Pearl millet', 'Finger millet', 'Potato', 'Sweet potato', 'Cassava', 'Cowpea', 'Pigeon Pea', 'Soyabean', 'Groundnut', 'Sugarcane', 'Cotton', 'Cacao', 'Tea', 'Tobacco')
)

all_data <- crop_area_ha %>%
  left_join(current_production_mton, by = "crop") %>%
  left_join(current_production_value, by = "crop") %>%
  left_join(additional_production_mton, by = "crop") %>%
  left_join(additional_value, by = "crop")%>%
  left_join(ctype_df, by = "crop")%>%
  left_join(crops_df, by = "crop")%>%
  select(crop,type,crop_name, `Harvested Area (1000xha)`, cp_mton, cp_value_musd, ap_mton, ap_value_musd)%>%
  rename(`Current Production (M tons)` = cp_mton, `Current Value (M USD)` = cp_value_musd, `Additional Production (M tons)` = ap_mton, `Additional Value (M USD)` = ap_value_musd)

# split the dataframe by type
all_data_split <- split(all_data, all_data$type)

# for each dataframe, mutate a new row with the sum of the columns and called it "Total"
# Assuming your list of data frames is named 'df_list'
all_data_split_2 <- lapply(names(all_data_split), function(df_name) {
  # Get the data frame
  df <- all_data_split[[df_name]]
  
  # Calculate the column sums for numeric columns
  total_row <- colSums(df[, sapply(df, is.numeric)], na.rm = TRUE)
  
  # Add placeholders for non-numeric columns, using the name of the data frame
  total_row <- c(df_name, NA, NA, total_row)
  
  # Bind the total row to the data frame
  df <- rbind(df, total_row)
  
  return(df)
})

# Set the names of the list elements back to the original names
names(all_data_split_2) <- names(all_data_split)

all_data_split_2 <- lapply(names(all_data_split_2), function(df_name) {
  # Get the data frame
  #df_name <- "Cereal"
  df <- all_data_split_2[[df_name]]
  df <- df[order(df[["Additional Value (M USD)"]], decreasing = TRUE), ]
  
  # except 1:3 columns cast to numeric
  df[, 4:ncol(df)] <- lapply(df[, 4:ncol(df)], as.numeric)
  
  # Select the top 4 rows
  top_4 <- head(df, 4)
  
  
  # Identify numeric columns
  numeric_columns <- sapply(df, is.numeric)
  
  # Calculate the sum for the remaining rows (Others) only for numeric columns
  others_row <- colSums(df[-(1:4), numeric_columns, drop = FALSE], na.rm = TRUE)
  
  # Add the label "Others" for the first column and NA for non-numeric columns
  others_row <- c(paste0("Others","-", df_name), rep(NA, sum(!numeric_columns)-1), others_row)
  
  # Bind the top 4 rows and the "Others" row together
  df <- rbind(top_4, others_row)%>%
    mutate(crop_name = ifelse(is.na(crop_name), crop, crop_name))%>%
    select(-c(crop, type))
  
})

# bind the dataframes
all_data_split_3 <- do.call(rbind, all_data_split_2)

write.csv(all_data_split_3, paste0(output_path, 'table-1.csv'), row.names = FALSE)  

# export xlsx
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", all_data_split_3)
saveWorkbook(wb, paste0(output_path, 'table-1.xlsx'), overwrite = TRUE)


# profitable area 

yield_factor <- 2
lp <- 50

profitable_raster_ori <- terra::rast(Sys.glob(
  paste0(output_path2, 'crop-rasters-final/','*_',lp,'_',yield_factor,'_profit_rasters.tif')
))
all_crops_ha <- profitable_raster_ori[[grep('_ha_spam', names(profitable_raster_ori))]]
all_crops_ha <- sum(all_crops_ha, na.rm = T)
all_crops_ha_profit <- profitable_raster_ori[[grep('area_profitable_ha', names(profitable_raster_ori))]]
all_crops_ha_profit <- sum(all_crops_ha_profit, na.rm = T)
a_p <- 100 * all_crops_ha_profit / all_crops_ha
# crop by cty
a_p <- terra::crop(a_p, ken, mask = TRUE)
all_crops_ha <- profitable_raster_ori[[grep('_ha_spam', names(profitable_raster_ori))]]
all_crops_gm <- profitable_raster_ori[[grep('actual_profit', names(profitable_raster_ori))]]
w_p <- terra::weighted.mean(all_crops_gm, all_crops_ha, na.rm = T)


w_p <- terra::crop(w_p,ken, mask = TRUE)
# Define raster pairs and parameters for plotting
pal_2<- colorRampPalette(c('#D5D5E3', '#E27B00', "#1D84FF", "#ADD8E6", "#00640D"))

png(paste0(output_path, "figure-1-profitable-area_",lp,"_",yield_factor,".png"), units="in", width=10, height=5, res=1000,pointsize = 7 )
par(mfrow=c(1,2))
terra::plot(ken, mar=c(2,2,2,1), clip=F, col='#F7F9F7', main='', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(a_p, breaks=c(0, 20, 40, 60, 80, 100), legend=F, main='', add=T, col=pal_2(6)) 
legend(33.9,-2.3, bty='n', cex=1.2, ncol=1, box.col="white", title="Area profitable (%)", legend=c('0 - 20', '20 - 40', '40 - 60', '60 - 80', '80 - 100'), fill = pal_2(6), horiz=F)
terra::plot(ken, axes=F, add=T)

terra::plot(ken, mar=c(2,2,2,1), clip=F, col='#F7F9F7', main='', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(w_p, legend=F, main='',add=T, breaks=c(0, 100, 200, 300), col=pal_2(3))
legend(33.9,-2.3, bty='n', cex=1.2, ncol=1, box.col="white", title="Area Weighted\nProfit (USD/ha)", legend=c('0 - 100', '100 - 200', '200 - 300'), fill = pal_2(3), horiz=F)
terra::plot(ken, axes=F, add=T)

dev.off()


# ------------------------------------------------------------------------------
# Acidic cropland
# ------------------------------------------------------------------------------

df_acidic <- readr::read_csv(paste0(output_path2, 'acidic-cropland/KEN-admin1.csv'))%>%
  dplyr::select(COUNTRY,
                NAME_1,
                high_hp,
                high_hp_perc,
                ends_with("_ha"),
                total)%>%
  mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10))

df_acidic_1 <- df_acidic %>%
  select(NAME_1, high_hp, high_hp_perc, total)%>%
  dplyr::arrange(desc(high_hp_perc))%>%
  dplyr::slice_head(n=10)%>%
  mutate(remaining = total - high_hp)%>%
  mutate(high_hp = high_hp / 1000,
         remaining = remaining / 1000,
         total = total / 1000)%>%
  arrange(desc(total))


extrafont::loadfonts(quiet = T)
my_font_2 <- "Frutiger"
# Create the plot
p_acidic_cropland <- ggplot(df_acidic_1, aes(x = reorder(NAME_1, -total), y = total, fill = "High HP")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = remaining, fill = "Low HP"), stat = "identity") +
  geom_text(
    aes(label = round(total , 2)),
    hjust = 0.5,
    vjust = -0.1,
    color = "black",
    size = 5,
    family = my_font_2
  ) +
  scale_fill_manual(values = alpha(c("#FF8355", "#EBEBF1"),0.9), name = "") +
  geom_text(aes(y = 52, label = paste0(high_hp_perc, "%", "\n","[", round(high_hp,1), "]")), color = "white", family=my_font_2) +
  labs(x = "County", y = "Crop Area (1000 ha)", 
       title = "Top 10 Counties in Kenya with acidic cropland (exchangeable acidity >10% ECEC) ") +
  
  theme_minimal() +
  theme(
    text = element_text(family = my_font_2, size = 14, color = "#445463"),
    #title = element_blank(),
    axis.text = element_text(
      size = 14,
      family = my_font_2,
      color = "#445463"
    ),
    axis.title = element_text(size = 14, family = my_font_2),
    legend.position = "bottom",
    panel.grid.major = element_line(
      colour = "#DBDBDB",
      linewidth = 0.5,
      linetype = "dotted"
    ),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(
      colour = "#DBDBDB",
      fill = NA,
      linewidth = 0.5
    )
  ) +

  guides(fill = guide_legend(reverse = TRUE))

ggsave(paste0(output_path, "figure-1-acidic-cropland.png"), plot = p_acidic_cropland, width = 14, height = 8, dpi = 400)


library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Load Africa map data
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# Define countries to highlight
highlight_countries <- c("Ethiopia", "Kenya", "Rwanda", "Tanzania", "Zambia")

# Plot
gaia_map <- ggplot(data = africa) +
  geom_sf(fill = "#E7EFE7", color = "#445463") +
  geom_sf(data = subset(africa, name %in% highlight_countries), fill = "#B7D0B7", color = "#7BC142") +
  # annotate with arroun the 4 country names

  labs(title = "") +
 ggthemes::theme_map()


ggsave(paste0(output_path, "figure-1-map.png"), plot = gaia_map, width = 8, height = 8, dpi = 400)


# ------------------------------------------------------------------------------
# Lime requirements
# ------------------------------------------------------------------------------

df_lime_requirements <- readr::read_csv(paste0(output_path2, 'lime-requirements/KEN-admin1.csv'))%>%
  dplyr::arrange(desc(high_hp)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::filter(high_hp > 0)%>%
  mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10)) %>%
  dplyr::select(NAME_1, ends_with("lr_tha")) %>%
  tidyr::pivot_longer(cols = -NAME_1,
                      names_to = "crop",
                      values_to = "lime_requirements_t") %>%
  dplyr::mutate(crop = gsub("_lr_tha", "", crop)) %>%
  dplyr::left_join(crops_df, by = c("crop" = "crop")) %>%
  left_join(ctype_df, by = "crop") %>%
  dplyr::filter(lime_requirements_t > 0)%>%
  dplyr::mutate(lime_requirements_t = lime_requirements_t / 1000)

df_lime_requirements_region <- df_lime_requirements %>%
  dplyr::group_by(NAME_1) %>%
  dplyr::summarise(lime_requirements_t = sum(lime_requirements_t))

df_lime_requirement_2 <- readr::read_csv(paste0(output_path2, 'lime-requirements/KEN-admin1.csv'))%>%
  dplyr::arrange(desc(high_hp)) %>%
  dplyr::filter(high_hp > 0)%>%
  mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10)) %>%
  dplyr::select(NAME_1, ends_with("lr_tha")) %>%
  tidyr::pivot_longer(cols = -NAME_1,
                      names_to = "crop",
                      values_to = "lime_requirements_t") %>%
  dplyr::mutate(crop = gsub("_lr_tha", "", crop)) %>%
  dplyr::left_join(crops_df, by = c("crop" = "crop")) %>%
  left_join(ctype_df, by = "crop") %>%
  dplyr::filter(lime_requirements_t > 0)%>%
  dplyr::mutate(lime_requirements_t = lime_requirements_t / 1000)

Chevalier1 <- wesanderson::wes_palettes$Chevalier1
Darjeeling1 <- wesanderson::wes_palettes$Darjeeling1

total_lime_requirements <- sum(df_lime_requirement_2$lime_requirements_t)
plt_lime_requirements <- ggplot(df_lime_requirements,
                                aes(
                                  x = reorder(NAME_1, -lime_requirements_t),
                                  y = lime_requirements_t,
                                  fill = crop_types
                                )) +
  geom_bar(stat = "identity",
           width = 0.8,
           position = "stack") +
  # Add geom_text for labels from df_lime_requirements_region
  geom_text(data = df_lime_requirements_region,  # Ensure this data frame has the same or compatible x-axis labels
            aes(x = reorder(NAME_1, -lime_requirements_t),
                y = lime_requirements_t,
                label = round(lime_requirements_t, 2)),
            hjust = 0.5,
            vjust = -0.1,
            color = "#165667",
            size = 5,
            family = my_font_2,
            inherit.aes = FALSE) +  # Use inherit.aes = FALSE to avoid inheriting the `fill` aesthetic
  
  scale_fill_manual(values = alpha(c(Chevalier1, Darjeeling1), 0.8)) +
  labs(
    title = "Lime requirements (Mt) in the top-10 most acidic counties in Kenya",
    x = "County",
    y = "Lime Requirements (Mt)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = my_font_2, size = 16,color = "#165667"),
    plot.title = element_text(size = 18, family = my_font_2, face = "bold"),
    axis.text.y = element_text(size = 12, family = my_font_2),
    axis.title = element_text(size = 14, family = my_font_2),
    legend.position = "right",
    panel.grid.major = element_line(
      colour = "#DBDBDB",
      linewidth = 0.5,
      linetype = "dotted"
    ),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(
      colour = "#DBDBDB",
      fill = NA,
      linewidth = 0.5
    )
  )


#nits="in", width=10, height=5, res=1000
plt_lime_requirements
ggsave(paste0(output_path, "figure-1-lime-requirements.png"), plot = plt_lime_requirements,units = "in", width = 12, height = 5, dpi = 1000)
