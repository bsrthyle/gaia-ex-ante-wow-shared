library(terra)
library(dplyr)
input_path <- paste0(here::here(), '/data-input/')
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles')

isocodes_ssa_2 <- isocodes_ssa[["ISO3"]]

gadm_0 <- terra::vect("D:/Dropbox/CIMMYT-WORK/Analysis/test-workspace/gadm_410-levels.gpkg", layer="ADM_0")
gadm_1 <- terra::vect("D:/Dropbox/CIMMYT-WORK/Analysis/test-workspace/gadm_410-levels.gpkg", layer="ADM_1")
gadm_2 <- terra::vect("D:/Dropbox/CIMMYT-WORK/Analysis/test-workspace/gadm_410-levels.gpkg", layer="ADM_2")
gadm_3 <- terra::vect("D:/Dropbox/CIMMYT-WORK/Analysis/test-workspace/gadm_410-levels.gpkg", layer="ADM_3")
gadm_4 <- terra::vect("D:/Dropbox/CIMMYT-WORK/Analysis/test-workspace/gadm_410-levels.gpkg", layer="ADM_4")


gadm_0_df <- as.data.frame(gadm_0)%>%dplyr::select(c("GID_0","COUNTRY"))
gadm_0_df <- gadm_0_df%>%dplyr::filter(GID_0 %in% isocodes_ssa_2)%>%
  distinct(GID_0, .keep_all = TRUE)

gadm_1_df <- as.data.frame(gadm_1)%>%dplyr::select(c("GID_0","ENGTYPE_1"))
gadm_1_df <- gadm_1_df%>%dplyr::filter(GID_0 %in% isocodes_ssa_2)%>%
  distinct(GID_0, .keep_all = TRUE)

gadm_2_df <- as.data.frame(gadm_2)%>%dplyr::select(c("GID_0","ENGTYPE_2"))
gadm_2_df <- gadm_2_df%>%dplyr::filter(GID_0 %in% isocodes_ssa_2)%>%
  distinct(GID_0, .keep_all = TRUE)

gadm_3_df <- as.data.frame(gadm_3)%>%dplyr::select(c("GID_0","ENGTYPE_3"))
gadm_3_df <- gadm_3_df%>%dplyr::filter(GID_0 %in% isocodes_ssa_2)%>%
  distinct(GID_0, .keep_all = TRUE)

gadm_4_df <- as.data.frame(gadm_4)%>%dplyr::select(c("GID_0","ENGTYPE_4"))
gadm_4_df <- gadm_4_df%>%dplyr::filter(GID_0 %in% isocodes_ssa_2)%>%
  distinct(GID_0, .keep_all = TRUE)

# merge
gadm_df <- gadm_0_df%>%dplyr::left_join(gadm_1_df, by="GID_0")%>%
  dplyr::left_join(gadm_2_df, by="GID_0")%>%
  dplyr::left_join(gadm_3_df, by="GID_0")%>%
  dplyr::left_join(gadm_4_df, by="GID_0")

write.csv(gadm_df, paste0(input_path, 'gadm_410-levels.csv'))
