
# directories
input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/data-output/')

# function
yield_quantiles <- function(crop, sd_f, quantiles, n_samples){
  # crop yield
  crop_yield <- spam_yield[[crop]] 
  names(crop_yield) <- 'spam'
  crop_yield$sd <- terra::ifel(crop_yield$spam > 0, sd_f * terra::global(crop_yield, fun='sd', na.rm=T)$sd, NA)
  max_crop_yield <- terra::global(crop_yield$spam, max, na.rm=T)$max
  # fit and truncate distribution
  gauss_dist <- terra::as.data.frame(crop_yield, xy=T)
  gauss_dist$fitted <- lapply(seq_len(nrow(gauss_dist)), function(i) { truncnorm::rtruncnorm(n=n_samples, a=0, b=max_crop_yield, mean=gauss_dist$spam[i], sd=gauss_dist$sd[i]) })
  gauss_dist$q <- lapply(gauss_dist$fitted, function(x) quantile(x, quantiles))
  # final data frame
  gauss_dist <- tidyr::unnest_wider(gauss_dist, col=q, names_sep='')
  names(gauss_dist) <- gsub('%', '', names(gauss_dist)) 
  # data frame with quantile to raster
  gauss_dist <- gauss_dist[c('x', 'y', names(gauss_dist[grep('q', names(gauss_dist))]))]
  crop_yield_q <- terra::rast(gauss_dist, extent=terra::ext(crop_yield), crs=terra::crs(crop_yield))
  crop_yield_f <- c(crop_yield$spam, crop_yield_q)
  terra::writeRaster(crop_yield_f, paste0(input_path, 'yield_quantiles/', crop, '_spam_yield_quantiles.tif'), overwrite=T)
  return(crop_yield_f)}

# set parameters
sd_f <- 1
quantiles <- seq(0.1, 1, 0.2)
n_samples <- 100

# load data
spam_yield <- terra::rast(paste0(input_path, "spam_yield_processed.tif")) / 1000

# run function
yield_quant <- lapply(names(spam_yield), function(crop){ print(crop); yield_quantiles(crop=crop, sd_f=sd_f, quantiles=quantiles, n_samples=n_samples) })
