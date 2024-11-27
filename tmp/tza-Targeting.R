
input_path1 <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/tmp/Kenya-maps/')

eth <- geodata::gadm('TZA', level=1, path=input_path)
png(paste0(output_path, "/figure-1-targetting-variables-TZA.png"), units="in", width=9, height=4.75*2, res=1000)
par(mfrow=c(2,2), mar=c(4,4,2,1), xaxs='i', yaxs='i', mgp=c(2.5,1,0))
# plot 1
ph <- terra::rast(paste0(input_path1, 'soilgrids_properties_cropland.tif'))
ph <- ph[['ph']]
ph <- terra::crop(ph, eth, mask=T)
pal <- colorRampPalette(c("darkred", "firebrick", "gold", "yellowgreen", "darkgreen"))
terra::plot(eth, mar=c(4,2,2,1), clip=F, col='white', main='Soil pH', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(ph, breaks=c(0, 4.5, 5.0, 5.5, 6.0, 6.5, Inf), legend=F, col=pal(6), main='', add=T)
legend(29.1,-9.2, bty='n', cex=1, ncol=2, box.col="white", title="", legend=c('< 4.5', '4.5 - 5.0', '5.0 - 5.5', '5.5 - 6.0', '6.0 - 6.5', '> 6.5'), fill=pal(6), horiz=F)
terra::plot(eth, axes=F, add=T)
# plot 2
hp <- terra::rast(paste0(input_path1, 'soilgrids_properties_cropland.tif'))
hp <- hp[['hp_sat']]
hp <- terra::crop(hp, eth, mask=T)
pal <- colorRampPalette(c('wheat', 'yellow', "gold", "firebrick", "darkred"))
terra::plot(eth, mar=c(4,2,2,1), clip=F, col='white', main='Acidity saturation (% of ECEC)', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(hp, breaks=c(0, 10, 20, Inf), col=pal(3), legend=F, main='', add=T)
legend(29,-9, bty='n', cex=1.2, ncol=1, box.col="white", title="", legend=c('< 10', '10 - 20', '> 20'), fill=pal(3), horiz=F)
terra::plot(eth, axes=F, add=T)
# plot 3
both <- c(ph, hp)
both_df <- terra::spatSample(both, 200000, 'regular')
hp_val <- mean(both_df$hp_sat, na.rm=T)
ph_val <- mean(both_df$ph, na.rm=T)
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
legend('topright', bty='n', cex=1.2, ncol=1, box.col="white", title="", legend=c('< pH > Hp', '< pH < Hp', '> pH > Hp', '> pH < Hp'), fill=c("gold", "#22A884FF", "#2A788EFF", "#414487FF"), horiz=F)
box()
# plot 4
both$cluster <- terra::ifel(both$ph < ph_val & both$hp_sat > hp_val, 1, NA)
both$cluster <- terra::ifel(both$ph < ph_val & both$hp_sat <= hp_val, 2, both$cluster)
both$cluster <- terra::ifel(both$ph >= ph_val & both$hp_sat > hp_val, 3, both$cluster)
both$cluster <- terra::ifel(both$ph >= ph_val & both$hp_sat <= hp_val, 4, both$cluster)
terra::plot(eth, mar=c(4,2,2,1), clip=F, col='white', main='Targetting clusters', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
terra::plot(both$cluster, col=c("gold", "#22A884FF", "#2A788EFF", "#414487FF"), legend=F, main='', add=T)
legend(29.2,-8.5, bty='n', cex=1.2, ncol=1, box.col="white", title="", legend=c('< pH > Hp', '< pH < Hp', '> pH > Hp', '> pH < Hp'), fill=c("gold", "#22A884FF", "#2A788EFF", "#414487FF"), horiz=F)
terra::plot(eth, axes=F, add=T)
dev.off()

# ------------------------------------------------------------------------------
# cropland and population
# ------------------------------------------------------------------------------

both <- terra::aggregate(both, 10, mean)
both$cluster <- terra::ifel(both$ph < ph_val & both$hp_sat > hp_val, 1, NA)
both$cluster <- terra::ifel(both$ph < ph_val & both$hp_sat <= hp_val, 2, both$cluster)
both$cluster <- terra::ifel(both$ph >= ph_val & both$hp_sat > hp_val, 3, both$cluster)
both$cluster <- terra::ifel(both$ph >= ph_val & both$hp_sat <= hp_val, 4, both$cluster)

ha <- terra::rast(paste0(input_path1, 'spam_harv_area_processed.tif'))
ha <- terra::crop(ha, eth, mask=T)
ha <- terra::resample(ha, both)

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

ctype_df <- read.csv(paste0(input_path1, 'crop_types.csv'))[-c(1)]
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
