

library(dplyr)

input_path <- paste0(here::here(), '/data-input/')
output_path <- paste0(here::here(), '/data-output/')
#countries_iso <- c("ZMB", "TZA", "NGA", "UGA", "ETH", "KEN", "SLE", "RWA", 'MWI', 'GHA')
country_iso3 <- "KEN"
source(
  "../gaia-ex-ante-wow-shared/scripts-3-summaries/# result-visualization_functions.R"
)


# create the output directory if it does not exist
for (country_iso3 in countries_iso) {
  figures_and_tables_path <- paste0(output_path, '/figures_and_tables/', country_iso3, '/')
  # create the figures and tables directory if it does not exist
  if (!dir.exists(figures_and_tables_path))
    dir.create(figures_and_tables_path, recursive = TRUE)
  report_path <- paste0(here::here(), '/reports/Slidedecks/', country_iso3, '/')
  # create the reports directory if it does not exist
  if (!dir.exists(report_path))
    dir.create(report_path, recursive = TRUE)
  # Function to get all CSV files starting with a specific country code recursively
  
  country_name <- countrycode::countrycode(country_iso3, origin = "iso3c", destination = "country.name")
  crop_types <- read.csv(paste0(input_path, 'crop_types.csv'))[, -1]
  # read in the json file
  level_2_names <- read.csv(paste0(input_path, 'gadm_410-levels.csv')) %>%
    mutate(ENGTYPE_1 = case_when(
      GID_0 == "ETH" ~ "Region",
      GID_0 == "ZWE" ~ "Province",
      TRUE ~ ENGTYPE_1
    ))
  
  
  # source the functions
  
  
  level_name <- get_name_from_iso3(country_iso3, level_2_names, admin = 1)
  result_tables <- get_result_tables(output_path, country_iso3)
  
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
    crop_types
  )
  
  # Generate the slidedeck for the country
  write_latex_slidedeck(
    country_iso3,
    report_path,
    level_name = level_name,
    figures_and_tables_path = figures_and_tables_path
  )
  
 
  # compile the slidedeck
  compile_latex_slidedeck(
    country_iso3,
    report_path
  )
  
}
