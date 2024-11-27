


library(dplyr)
extrafont::font_import()
input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/reports/eia_technical_notes/data-output/')
output_path1 <- paste0(here::here(), '/data-output/')
#countries_iso <- c("ZMB" , "TZA","UGA","KEN","SLE","RWA",'MWI','GHA','MDG','BDI')
#country_iso3 <- "NGA"
countries_iso <- c("NGA", "ETH")
source(
  "../gaia-ex-ante-wow-shared/reports/eia_technical_notes/1-result-visualization_functions.R",
  
)
source(
  "../gaia-ex-ante-wow-shared/reports/eia_technical_notes/2-beamer-slide-generator.R"
)

soil_data <- terra::rast(paste0(input_path, 'soilgrids_properties_cropland.tif'))
# create the output directory if it does not exist
for (country_iso3 in countries_iso) {
  figures_and_tables_path <- paste0(output_path, '/figures_and_tables/', country_iso3, '/')
  # create the figures and tables directory if it does not exist
  if (!dir.exists(figures_and_tables_path))
    dir.create(figures_and_tables_path, recursive = TRUE)
  report_path <- paste0(output_path, 'Slidedecks')
  # create the reports directory if it does not exist
  if (!dir.exists(report_path))
    dir.create(report_path, recursive = TRUE)
  # Function to get all CSV files starting with a specific country code recursively
  
  country_name <- countrycode::countrycode(country_iso3, origin = "iso3c", destination = "country.name")
  crop_types <- read.csv(paste0(input_path, 'crop_types.csv'))[, -1]
  # read in the json file
  level_2_names <- read.csv(paste0(input_path, 'gadm_410-levels.csv')) %>%
    mutate(
      ENGTYPE_1 = case_when(
        GID_0 == "ETH" ~ "Region",
        GID_0 == "ZWE" ~ "Province",
        GID_0 == "MDG" ~ "Province",
        TRUE ~ ENGTYPE_1
      )
    )
  
  
  # source the functions
  
  
  level_name <- get_name_from_iso3(country_iso3, level_2_names, admin = 1)
  result_tables <- get_result_tables(output_path1, country_iso3)
  
  crops_df <- get_main_crops_and_type(country_iso3, csv_list = result_tables)$crop_types_new
  main_crop <- get_main_crops_and_type(country_iso3, csv_list = result_tables)$main_crop
  # Generate the summary visuals for the country
  
  
  generate_summary_visuals(
    country_iso3,
    country_name,
    result_tables,
    input_path,
    figures_and_tables_path,
    output_path,
    crop_types,
    soil_data
  )

  # Generate the slidedeck for the country
  
  write_latex_slidedeck(
    country_iso3,
    report_path,
    level_name = level_name,
    figures_and_tables_path = figures_and_tables_path
  )
  
  #write_rmd_file(
  #  country_iso3,
  #  report_path,
  #  level_name = level_name,
  #  figures_and_tables_path = figures_and_tables_path
  #)
  
  # compile the slidedeck
  
 # compile_latex_slidedeck(country_name, report_path)
  
  
}

# compile the slidedeck for all countries using parallel processing

compile_latex_slidedeck_parallel <- function(countries_iso3, report_path){
  figures_and_tables_path <- paste0(output_path, '/figures_and_tables/', country_iso3, '/')
  # create the figures and tables directory if it does not exist
  if (!dir.exists(figures_and_tables_path))
    dir.create(figures_and_tables_path, recursive = TRUE)
  report_path <- paste0(output_path, 'Slidedecks')
  # create the reports directory if it does not exist
  if (!dir.exists(report_path))
    dir.create(report_path, recursive = TRUE)
  # Function to get all CSV files starting with a specific country code recursively
  
  country_name <- countrycode::countrycode(country_iso3, origin = "iso3c", destination = "country.name")
  crop_types <- read.csv(paste0(input_path, 'crop_types.csv'))[, -1]
  # read in the json file
  level_2_names <- read.csv(paste0(input_path, 'gadm_410-levels.csv')) %>%
    mutate(
      ENGTYPE_1 = case_when(
        GID_0 == "ETH" ~ "Region",
        GID_0 == "ZWE" ~ "Province",
        GID_0 == "MDG" ~ "Province",
        TRUE ~ ENGTYPE_1
      )
    )
  
  
  # source the functions
  
  
  level_name <- get_name_from_iso3(country_iso3, level_2_names, admin = 1)
  result_tables <- get_result_tables(output_path1, country_iso3)
  
  crops_df <- get_main_crops_and_type(country_iso3, csv_list = result_tables)$crop_types_new
  main_crop <- get_main_crops_and_type(country_iso3, csv_list = result_tables)$main_crop
  
  
  # Generate the slidedeck for the country
  compile_latex_slidedeck(country_name, report_path)
}

library(foreach)
library(doParallel)

# Register parallel backend
num_cores <- detectCores() - 1  # Leave one core free
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Run the function in parallel
foreach(country_iso3 = countries_iso, .packages = c("tidyverse")) %dopar% {
  compile_latex_slidedeck_parallel(countries_iso3, report_path)
}

# Stop the cluster
stopCluster(cl)
