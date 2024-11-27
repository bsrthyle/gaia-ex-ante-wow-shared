pacman::p_load(
  here,
  ggplot2,
  ggthemes,
  ggtext,
  extrafont,
  wesanderson,
  ggthemr,
  countrycode,
  kableExtra,
  tidyverse
)
extrafont::loadfonts(quiet = T)
my_font <- "Segoe UI Black"
my_font_2 <- "Frutiger"
# Set my_font_3 depending on the OS
if (Sys.info()[["sysname"]] == "Darwin") {
  my_font_3 <- "Frutiger"
} else if (Sys.info()[["sysname"]] == "Windows") {
  my_font_3 <- "Poppins"
} else {
  my_font_3 <- "Frutiger"
}
my_size <- 12
my_colors <- list(
  "#F09C26",
  "#50AE5E",
  "#BFCCB5",
  "#7C96AB",
  "#B7B7B7",
  "#EDC6B1",
  "#F5F5DC",
  "#A9A9A9",
  "#FFC0CB",
  "#8B0000",
  "#8A2BE2",
  "#8F9779"
)
colors <- c(
  "#8F9779",
  "#FFD700",
  "#8B4513",
  "#87CEEB",
  "#FFA500",
  "#F5F5DC",
  "#A9A9A9",
  "#FFC0CB",
  "#8B0000",
  "#596baf",
  "#8A2BE2"
)
my_colors_1 <- c(
  "#ffae49",
  "#44b7c2",
  "#024b7a",
  "#ee4144",
  "#1f5e77",
  "#c792e9",
  "#5eeda0",
  "#019d9c",
  "#83329b"
)
likert_colors <- c("#a3336d", "#b55b8a", "#7a88bf", "#596baf", "#e7e7e7")
my_colors <-
  c(
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
    "#e377c2",
    "#7f7f7f",
    "#bcbd22",
    "#17becf"
  )
color_new <- c(
  "#F2DFEB",
  "#AEDFF2",
  "#72DBF2",
  "#F2C879",
  "#F2D8A7",
  "#8990B3",
  "#FFD3C4",
  "#DEE3FF",
  "#DEFFC4",
  "#A0B392"
)

eia_colors <- c("#80a172", "#B74F20","#02401B", "#FDD262", "#d62728", "#D3DDDC","#9467bd","#C7B19C","#FFBB70",
                "#bcbd22" )
Darjeeling1 <- wesanderson::wes_palettes$Darjeeling1
Darjeeling2 <- wesanderson::wes_palettes$Darjeeling2
Royal2 <- wesanderson::wes_palettes$Royal2
Moonrise1 <- wesanderson::wes_palettes$Moonrise1
Zissou1 <- wesanderson::wes_palettes$Zissou1Continuous
Chevalier1 <- wesanderson::wes_palettes$Chevalier1
Cavalcanti1 <- wesanderson::wes_palettes$Cavalcanti1
GrandBudapest1 <- wesanderson::wes_palettes$GrandBudapest1
bar_colors <- c("#1B81BB",
                "#288D8D",
                "#5F4C60",
                "#7570B3",
                "#9F0000",
                "#FFBB70",
                "#bcbd22")
#devtools::install_github('cttobin/ggthemr')
ggthemr("pale")

#' Save a Plot to a PNG File
#'
#' This function saves a given ggplot object to a PNG file with a specified width, height, and resolution (DPI).
#' The file name is automatically generated based on the name of the plot variable and includes the country ISO3 code.
#'
#' @param plot A ggplot object to be saved.
#' @param width A numeric value specifying the width of the plot in inches. Default is 10 inches.
#' @param country_iso3 A character string representing the ISO3 code of the country, which will be included in the file name.
#' @param height A numeric value specifying the height of the plot in inches. Default is 6 inches.
#' @param dpi A numeric value specifying the resolution of the plot in dots per inch. Default is 1000 DPI.
#'
#' @return None. The function saves the plot to a file and does not return a value.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a ggplot object named 'my_plot'
#' # Example usage:
#' save_plot(my_plot, country_iso3 = "KEN")
#' }
#'
#' @export
save_plot <- function(plot,
                      width = 10,
                      country_iso3,
                      height = 6 ,
                      dpi = 1000) {
  # Capture the name of the plot variable
  plot_name <- deparse(substitute(plot))
  # Construct the file name with .png extension
  file_name <- paste0(plot_name, ".eps")
  # Construct the full path for the plot
  file_path <- paste0(figures_and_tables_path, country_iso3, "_", file_name)
  # Save the plot
  ggsave(
    file_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi
  )
}
#' Get Result Tables from CSV Files
#'
#' This function retrieves all CSV files in a specified directory that start with a given country code.
#' It then reads each CSV file into a data frame and stores these data frames in a named list.
#' The names of the list elements are created by combining the folder name and the file name.
#'
#' @param dir A character string specifying the directory where the CSV files are located.
#' @param country_code A character string representing the ISO code of the country used to filter files that start with this code.
#' @return A named list of data frames. Each element of the list contains the data from a corresponding CSV file, and the names of the list elements are constructed using the folder name and file name.
#' @export
get_result_tables <- function(dir, country_code) {
  message("Fetching CSV files from directory: ", dir)
  
  csv_files <- list.files(
    path = dir,
    pattern = paste0("^", country_code, ".*\\.csv$"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(csv_files) == 0) {
    stop("No CSV files found for the specified country code.")
  }
  
  csv_list <- lapply(csv_files, function(file) {
    message("Reading file: ", file)
    folder_name <- basename(dirname(file))
    file_name <- basename(file)
    combined_name <- paste0(folder_name, "_", file_name)
    read.csv(file)
  })
  
  names(csv_list) <- sapply(csv_files, function(file) {
    folder_name <- basename(dirname(file))
    file_name <- basename(file)
    paste0(folder_name, "_", file_name)
  })
  
  message("Successfully retrieved all CSV files.")
  return(csv_list)
}

#' Get Administrative Type Name from ISO3 Code
#'
#' This function retrieves the administrative type name (e.g., Province, District) 
#' based on the provided ISO3 code and administrative level. If the name ends with "y",
#' it will be pluralized by replacing "y" with "ies". Otherwise, an "s" will be added to pluralize the name.
#'
#' @param isocode A character string representing the ISO3 code of the country.
#' @param df A data frame containing the administrative levels and their corresponding names.
#' @param admin An integer representing the administrative level (e.g., 1, 2, 3, or 4).
#' 
#' @return A character string representing the pluralized administrative type name for the specified level.
#' If no match is found, the function returns `NA`.
#' 
#' @examples
#' # Example usage:
#' # Assuming `df` is your data frame and contains the required columns
#' result <- get_name_from_iso3("ETH", df, 1)
#' print(result)  # Might return "Citys" based on the dataset provided
#'
get_name_from_iso3 <- function(isocode, df, admin) {
  # Construct the column name based on the admin level
  column_name <- paste0("ENGTYPE_", admin)
  
  # Filter the DataFrame for the matching ISO3 code
  result <- df[df$GID_0 == isocode, column_name]
  
  # Check if any match is found
  if (length(result) > 0 && !is.na(result)) {
    # Check if the result ends with "y"
    if (grepl("y$", result)) {
      # Replace "y" with "ies"
      result <- sub("y$", "ies", result)
    } else {
      # Otherwise, add an "s"
      result <- paste0(result, "s")
    }
    return(result)
  } else {
    return(NA)  # Return NA if no match is found or if result is NA
  }
}







#' Get main crops and crop type
#'
#' Retrieves the main crops and their types for a given country using crop area data.
#'
#' @param country_iso3 ISO3 code of the country.
#' @param csv_list A named list of data frames containing crop area data.
#' 
#' @return A list containing:
#' \describe{
#'   \item{`crop_types_new`}{Data frame with crops, area, and types.}
#'   \item{`main_crop`}{Main crop of the country.}
#' }
#'
#' @importFrom dplyr select summarise across mutate left_join filter pull
#' @importFrom tidyr pivot_longer
#' @export
get_main_crops_and_type <- function(country_iso3,
                                    csv_list) {
  
  crops_df <- data.frame(crop=c("MAIZ", "SORG", "BEAN", "CHIC", 'LENT', "WHEA", "BARL", "ACOF", "RCOF", 'PMIL', 'SMIL', 'POTA', 'SWPO', 'CASS', 'COWP', 'PIGE', 'SOYB', 'GROU', 'SUGC', 'COTT', 'COCO', 'TEAS', 'TOBA'), 
                         crop_name=c('Maize', "Sorghum", "Bean", "Chick pea", 'Lentil', "Wheat", "Barley", "Coffee arabica", "Coffee robusta", 'Pearl millet', 'Finger millet', 'Potato', 'Sweet potato', 'Cassava', 'Cowpea', 'Pigeon Pea', 'Soyabean', 'Groundnut', 'Sugarcane', 'Cotton', 'Cacao', 'Tea', 'Tobacco')
                         )
  df_crops <- csv_list[[paste0("acidic-cropland_", country_iso3, "-admin1.csv")]] %>%
    dplyr::select(ends_with("_ha")) %>%
    dplyr::summarise(across(everything(), sum)) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "crop",
                        values_to = "area_ha") %>%
    dplyr::mutate(crop = gsub("_ha", "", crop)) %>%
    dplyr::left_join(crop_types, by = c("crop" = "crop")) %>%
    mutate(rank = rank(-area_ha)) %>%
    # mutate crop_type if MAIZ its MAIZ if rank is less than 3 its crop else its type
    mutate(crop_type = ifelse(crop == "MAIZ", "MAIZ",
                              ifelse(rank < 3, crop, type)))%>%
    left_join(crops_df, by = c("crop" = "crop"))%>%
    dplyr::mutate(
      crop_types = dplyr::case_when(
        crop == "MAIZ" ~ "Maize",
        rank < 5 ~ crop_name,
        (rank > 5 & type == "Commodity") ~ "Other-Non-food",
        TRUE ~ paste0("Other-", type)
      )
    )%>%
    select(-area_ha)
  
  main_crop <- df_crops %>%
    filter(rank == 1) %>%
    select(crop, crop_types)
  
  # If the first-ranked crop is "MAIZ", retrieve the second-ranked crop and crop_type
  if (main_crop$crop == "MAIZ") {
    main_crop <- df_crops %>%
      filter(rank == 2) %>%
      select(crop, crop_types)
  }
  
  main_crop_list <- list(
    crop = main_crop %>% pull(crop),
    crop_type = main_crop %>% pull(crop_types)
  )
  
  # return df_crops and main_crop
  return(list(crop_types_new = df_crops, main_crop = main_crop_list))
  
  
}

#' Generate Plot of Acidic Crop Area
#'
#' This function generates a bar plot visualizing the top 10 administrative regions
#' within a specified country that have the highest acidic crop area 
#' (defined as exchangeable acidity >10% ECEC). The plot is saved to a specified path.
#'
#' @param country_iso3 A character string representing the ISO3 code of the country.
#' @param csv_list A named list of data frames, where each element corresponds to
#'        a CSV file for a specific country. The key is the file name.
#' @param figures_and_tables_path A character string specifying the directory path
#'        where the generated plot will be saved.
#'
#' @return This function does not return a value. It creates and saves a bar plot 
#'         as a PNG file in the specified directory.
#'
#' @details
#' The function reads crop area data for the specified country from the provided
#' `csv_list`, processes it to identify the top 10 administrative regions based
#' on acidic crop area percentage, and generates a bar plot. The plot visualizes
#' both the high acidic crop area and remaining crop area in thousands of hectares (1000 ha).
#'
#' The plot includes additional annotations to show the percentage of acidic crop area 
#' and absolute values for each region.
#' gen_acidic_crop_area("KEN", csv_list, "/path/to/save/plots/")
#'
#' @import ggplot2
#' @importFrom dplyr select arrange slice_head mutate
#' @importFrom stringr str_wrap
#' @importFrom scales alpha
#' @export
#' 
gen_acidic_crop_area <- function(country_iso3,
                                 csv_list,
                                 figures_and_tables_path) {
  df_acidic <- csv_list[[paste0("acidic-cropland_", country_iso3, "-admin1.csv")]] %>%
    dplyr::select(COUNTRY,
                  NAME_1,
                  high_hp,
                  high_hp_perc,
                  ends_with("_ha"),
                  total) %>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10))
  
  df_acidic_1 <- df_acidic %>%
    select(NAME_1, high_hp, high_hp_perc, total) %>%
    dplyr::arrange(desc(high_hp_perc)) %>%
    filter(high_hp_perc > 0) %>%
    dplyr::slice_head(n = 10) %>%
    mutate(remaining = total - high_hp) %>%
    mutate(
      high_hp = high_hp / 1000,
      remaining = remaining / 1000,
      total = total / 1000
    ) %>%
    arrange(desc(total))
  
  # Create the plot
  plt_acidic_cropland_both <- ggplot(df_acidic_1, aes(
    x = reorder(NAME_1, -total),
    y = total,
    fill = "High HP"
  )) +
    geom_bar(stat = "identity") +
    geom_bar(aes(y = remaining, fill = "Low HP"), stat = "identity") +
    geom_text(
      aes(label = paste0(round(total , 2), "\n","[", round(high_hp_perc, 2), " %]")),
      hjust = 0.5,
      vjust = -0.2,
      color = "black",
      size = 5,
      family = my_font_2
    ) +
    ylim(0, max(df_acidic_1$total) * 1.2) +
    scale_fill_manual(values = alpha(c("#c4521e", "#78a779"), 0.9), name = "") +
    
    labs(
      x = level_name ,
      y = "Crop Area (1000 ha)",
      title = "",
      caption = "Note: High HP=High acidity saturation, (>10% ECEC)\nLow HP=Low acidity saturation(<=10% ECEC)"
    ) +
    
    theme_minimal() +
    theme(
      text = element_text(
        family = my_font_2,
        size = 14,
        color = "#445463"
      ),
      #title = element_blank(),
      axis.text = element_text(
        size = 14,
        family = my_font_2,
        color = "#445463"
      ),
      axis.title = element_text(size = 14, family = my_font_2),
      plot.title = element_text(hjust = 0.5, size = 16, color = "#c4521e") ,
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
  
  # Save the plot
  save_plot(
    plt_acidic_cropland_both,
    width = 12,
    height = 6,
    country_iso3 = country_iso3
  )
}

#' Create and Save a Plot of Acidic Cropland for a Given Country
#'
#' This function processes data related to acidic cropland for a specified country,
#' generates a bar plot showing the top 10 regions with the highest amount of acidic cropland,
#' and saves the plot as a PNG file.
#'
#' @param country_iso3 A character string representing the ISO3 code of the country.
#' @param csv_list A list of data frames, typically generated from the get_result_tables function, containing the relevant CSV data.
#' @param figures_and_tables_path A character string specifying the directory path where the plot should be saved.
#'
#' @return None. The function generates and saves the plot but does not return a value.
#'
#' @export
create_and_save_acidic_cropland_plot <- function(country_iso3,
                                                 csv_list,
                                                 figures_and_tables_path) {
  # Step 1: Filter the relevant CSV file based on the country code
  df_acidic_cropland <- csv_list[[paste0("acidic-cropland_", country_iso3, "-admin1.csv")]] %>%
    dplyr::select(COUNTRY,
                  NAME_1,
                  high_hp,
                  high_hp_perc,
                  ends_with("_ha"),
                  total)%>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10))
  
  # Step 2: Generate the top 10 regions by high_hp
  df_acidic_crop_land <- df_acidic_cropland %>%
    dplyr::select(NAME_1, high_hp) %>%
    dplyr::arrange(desc(high_hp)) %>%
    dplyr::slice_head(n = 10)%>%
    dplyr::filter(high_hp > 0)
    
  
  # Step 3: Generate the top 10 regions by high_hp_perc
  df_acidic_cropland_per <- df_acidic_cropland %>%
    dplyr::select(NAME_1, high_hp_perc) %>%
    dplyr::arrange(desc(high_hp_perc)) %>%
    dplyr::slice_head(n = 10)%>%
    dplyr::filter(high_hp_perc>0)
  
  # Step 4: Create the plot
  plt_acidic_cropland <- ggplot(df_acidic_crop_land,
                                aes(
                                  x = reorder(NAME_1, -high_hp),
                                  y = high_hp,
                                  fill = NAME_1
                                )) +
    geom_bar(stat = "identity",
             width = 0.8,
             fill = alpha("#446455", 0.8)) +
    geom_text(
      aes(label = round(high_hp / 1000, 2)),
      hjust = 0.5,
      vjust = -0.1,
      color = "black",
      size = 5,
      family = my_font_2
    ) +
    scale_fill_manual(values = eia_colors) +
    scale_y_continuous(labels = scales::label_number(scale = 1 / 1000)) +
    # wrap x-axis labels
    scale_x_discrete(labels = stringr::str_wrap(df_acidic_crop_land$NAME_1, width = 10)) +
    labs(
      title = paste0(
        "Top-10 " ,
        level_name,
        " in ",
        country_name,
        " with acidic cropland (exchangeable acidity >10% ECEC)"
      ),
      x = level_name,
      y = " Cropland with High Exchangeable Acidity (x1000 ha)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      title = element_blank(),
      axis.text = element_text(
        size = 14,
        family = my_font_2,
        color = "black"
      ),
      axis.title = element_text(size = 14, family = my_font_2),
      legend.position = "none",
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
  
  plt_acidic_cropland_perc <- ggplot(df_acidic_cropland_per,
                                     aes(
                                       x = reorder(NAME_1, -high_hp_perc),
                                       y = high_hp_perc,
                                       fill = NAME_1
                                     )) +
    geom_bar(stat = "identity",
             width = 0.8,
             fill = alpha("#446455", 0.8)) +
    geom_text(
      aes(label = round(high_hp_perc, 2)),
      hjust = 0.5,
      vjust = -0.1,
      color = "black",
      size = 5,
      family = my_font_2
    ) +
    scale_fill_manual(values = eia_colors) +
    scale_y_continuous(labels = scales::label_number(scale = 1)) +
    scale_x_discrete(labels = stringr::str_wrap(df_acidic_cropland_per$NAME_1, width = 10)) +
    labs(
      title = paste0(
        "Top-10 ",
        level_name,
        " in ",
        country_name,
        " with acidic cropland (exchangeable acidity >10% ECEC) (Percentage)"
      ),
      x = level_name,
      y = " Cropland with High Exchangeable Acidity (%)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      title = element_blank(),
      axis.text = element_text(
        size = 14,
        family = my_font_2,
        color = "black"
      ),
      axis.title = element_text(size = 14, family = my_font_2),
      legend.position = "none",
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
  
  # Step 5: Save the plot
  save_plot(
    plt_acidic_cropland,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  
  save_plot(
    plt_acidic_cropland_perc,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  
  message("Successfully created and saved the plot for acidic cropland.")
}

#' Create and Save a Plot and Table for Crop Area Distribution in Acidic Cropland
#'
#' This function processes data related to the crop area distribution in acidic cropland for a specified country,
#' generates a stacked bar plot showing the crop area in the top 10 most acidic regions, and saves the plot as a PNG file.
#' It also creates a LaTeX table summarizing the crop area distribution and saves it as a `.tex` file.
#'
#' @param country_iso3 A character string representing the ISO3 code of the country.
#' @param country_name A character string representing the name of the country, used in the plot title.
#' @param csv_list A list of data frames, typically generated from the get_result_tables function, containing the relevant CSV data.
#' @param figures_and_tables_path A character string specifying the directory path where the plot and table should be saved.
#' @param crop_types A data frame containing information about crop types used to map crop codes to crop names.
#'
#' @return None. The function generates and saves both the plot and the table but does not return a value.
#'
#' @export
create_and_save_crop_area_acidic <- function(country_iso3,
                                             country_name,
                                             csv_list,
                                             figures_and_tables_path,
                                             crop_types) {
  # Step 1: Filter and process the relevant CSV file based on the country code
  
  
  df_acidic_cropland <- csv_list[[paste0("acidic-cropland_", country_iso3, "-admin1.csv")]] %>%
    dplyr::select(COUNTRY,
                  NAME_1,
                  high_hp,
                  high_hp_perc,
                  ends_with("_ha"),
                  total)%>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10))
  
  
  
  df_crop_area_acidic <- df_acidic_cropland %>%
    dplyr::arrange(desc(high_hp)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::filter(high_hp > 0) %>%
    dplyr::select(NAME_1, ends_with("_ha")) %>%
    tidyr::pivot_longer(cols = -NAME_1,
                        names_to = "crop",
                        values_to = "area_ha") %>%
    dplyr::mutate(crop = gsub("_ha", "", crop)) %>%
    dplyr::mutate(area_ha = area_ha / 1000) %>%
    dplyr::left_join(crop_types, by = c("crop" = "crop")) %>%
    
    left_join(crops_df, by = c("crop" = "crop"))
  # Step 2: Create the stacked bar plot
  plt_crop_area_acidic <- ggplot(df_crop_area_acidic,
                                 aes(
                                   x = reorder(NAME_1, -area_ha),
                                   y = area_ha,
                                   fill = crop_types
                                 )) +
    geom_bar(stat = "identity",
             width = 0.7,
             position = "stack") +
    scale_fill_manual(values = alpha(eia_colors, 0.95)) +
    #scale_x_discrete(labels = stringr::str_wrap(df_crop_area_acidic$NAME_1, width = 10)) +
    labs(
      #title = paste0("Total crop area in the top-10 most acidic  ", level_name),
      #subtitle = "Not segregated by acidic cropland",
      x = level_name,
      y = " Crop Area (x1000 ha)",
      fill = "",
      caption = "Source: IFPRI, 2019. Global Spatially-Disaggregated Crop Production \nStatistics Data for 2010 Version 2.0. https://doi.org/10.7910/DVN/PRFF8V, Harvard Dataverse, V4."
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      #title = element_blank(),
      axis.text.y = element_text(size = 12, family = my_font_2),
      axis.title = element_text(size = 14, family = my_font_2),
      legend.position = "bottom",
      panel.grid.major = element_line(
        colour = "#DBDBDB",
        linewidth = 0.5,
        linetype = "dotted"
      ),
      plot.caption = element_text(
        size = 8,
        family = my_font_2
      ),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.border = element_rect(
        colour = "#DBDBDB",
        fill = NA,
        linewidth = 0.5
      )
    )
  
  # Step 3: Save the plot
  save_plot(
    plt_crop_area_acidic,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  message(
    "Successfully created and saved the plot and table for crop area distribution in acidic cropland."
  )
  
  # Step 4: Create the summary table
  df_crop_area_acidic_tbl <- df_crop_area_acidic %>%
    dplyr::group_by(NAME_1, crop_types) %>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10)) %>%
    dplyr::summarise(area_ha = sum(area_ha)) %>%
    tidyr::pivot_wider(names_from = crop_types, values_from = area_ha) %>%
    dplyr::mutate(across(everything(), ~ round(., 2))) %>%
    mutate(mutate(across(everything(), ~ ifelse(.==0, NA, .)))) %>%
    rename(!!level_name := NAME_1)%>%
    knitr::kable("latex", escape = TRUE, booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), font_size =7) %>%
    kableExtra::column_spec(1, width = "1.8cm")%>%
    as.character() %>%
    gsub("NA", "\\-", .) %>%
    gsub("(?<!\\d)0\\.00(?!\\d)", "-", ., perl = TRUE)%>%
    gsub("Coffee robusta", "\\\\makecell{Coffee \\\\\\\\robusta}", .) %>%
    gsub("Sweet potato", "\\\\makecell{Sweet \\\\\\\\potato}", .) %>%
    gsub("Other-Commodity", "\\\\makecell{Other-\\\\\\\\Commodity}", .) %>%
    gsub("Other-Cereal", "\\\\makecell{Other-\\\\\\\\Cereal}", .) %>%
    gsub("Other-Legume", "\\\\makecell{Other-\\\\\\\\Legume}", .) %>%
    gsub("Other-RTBs", "\\\\makecell{Other-\\\\\\\\RTBs}", .) %>%
    gsub("Other-Non-food",
         "\\\\makecell{Other-\\\\\\\\Non-food}",
         .)
  
  # Step 5: Save the table as a .tex file
  writeLines(df_crop_area_acidic_tbl,
             file.path(
               figures_and_tables_path,
               paste0(country_iso3, "_table_crop_area_acidic.tex")
             ))
  message("Successfully saved the latex table for crop area distribution in acidic cropland.")
}

# Example usage:
# create_and_save_crop_area_acidic("KEN", "Kenya", csv_list, "path/to/save/plots_and_tables/", crop_types)
#' Create and Save Plots for Additional Production and Value in Acidic Cropland
#'
#' This function processes data related to additional production and value in acidic cropland for a specified country,
#' generates stacked bar plots for both the additional production (in tonnes) and the additional production value (in USD),
#' and saves these plots as PNG files.
#'
#' @param country_iso3 A character string representing the ISO3 code of the country.
#' @param country_name A character string representing the name of the country, used in the plot title.
#' @param csv_list A list of data frames, typically generated from the get_result_tables function, containing the relevant CSV data.
#' @param figures_and_tables_path A character string specifying the directory path where the plots should be saved.
#' @param crop_types A data frame containing information about crop types used to map crop codes to crop names.
#'
#' @return None. The function generates and saves the plots but does not return a value.
#'
#' @export
create_and_save_additional_production_plots <- function(country_iso3,
                                                        country_name,
                                                        csv_list,
                                                        figures_and_tables_path,
                                                        crop_types) {
  # Step 1: Process additional production data
  df_additional_production <- csv_list[[paste0("additional-production_", country_iso3, "-admin1.csv")]] %>%
    dplyr::arrange(desc(high_hp)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::filter(high_hp > 0) %>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10))%>%
    dplyr::select(NAME_1, ends_with("_add_t")) %>%
    tidyr::pivot_longer(cols = -NAME_1,
                        names_to = "crop",
                        values_to = "additional_production_t") %>%
    dplyr::mutate(crop = gsub("_add_t", "", crop)) %>%
    dplyr::left_join(crops_df, by = c("crop" = "crop")) %>%
    dplyr::mutate(additional_production_t = additional_production_t / 1000) %>%
    dplyr::filter(additional_production_t > 0) 
  
  # Step 2: Create the stacked bar plot for additional production
  plt_additional_production <- ggplot(
    df_additional_production,
    aes(
      x = reorder(NAME_1, -additional_production_t),
      y = additional_production_t,
      fill = crop_types
    )
  ) +
    geom_bar(stat = "identity",
             width = 0.7,
             position = "stack") +
    scale_fill_manual(values = alpha(eia_colors, 0.95)) +
    labs(
      title = paste0("Additional production in the top-10 most acidic  ", level_name),
      subtitle = "Not segregated by acidic cropland",
      x = level_name,
      y = " Additional Production (x1000 t)",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      title = element_blank(),
      axis.text.y = element_text(size = 12, family = my_font_2),
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
    )
  
  # Step 3: Save the plot for additional production
  save_plot(
    plt_additional_production,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  
  # additional production table
  
  df_additional_production_tbl <- df_additional_production %>%
    dplyr::group_by(NAME_1, crop_types) %>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10)) %>%
    dplyr::summarise(additional_production_t = sum(additional_production_t)) %>%
    tidyr::pivot_wider(names_from = crop_types, values_from = additional_production_t) %>%
    dplyr::mutate(across(everything(), ~ round(., 2))) %>%
    mutate(mutate(across(everything(), ~ ifelse(.==0, NA, .)))) %>%
    rename(!!level_name := NAME_1)%>%
    knitr::kable("latex", escape = TRUE, booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), font_size =7) %>%
    kableExtra::column_spec(1, width = "1.8cm")%>%
    as.character() %>%
    gsub("NA", "\\-", .) %>%
    gsub("(?<!\\d)0\\.00(?!\\d)", "-", ., perl = TRUE)%>%
    gsub("Coffee robusta", "\\\\makecell{Coffee \\\\\\\\robusta}", .) %>%
    gsub("Sweet potato", "\\\\makecell{Sweet \\\\\\\\potato}", .) %>%
    gsub("Other-Commodity", "\\\\makecell{Other-\\\\\\\\Commodity}", .) %>%
    gsub("Other-Cereal", "\\\\makecell{Other-\\\\\\\\Cereal}", .) %>%
    gsub("Other-Legume", "\\\\makecell{Other-\\\\\\\\Legume}", .) %>%
    gsub("Other-RTBs", "\\\\makecell{Other-\\\\\\\\RTBs}", .) %>%
    gsub("Other-Non-food",
         "\\\\makecell{Other-\\\\\\\\Non-food}",
         .)
  
  # Step 5: Save the table as a .tex file
  writeLines(df_additional_production_tbl,
             file.path(
               figures_and_tables_path,
               paste0(country_iso3, "_table_additional_production.tex")
             ))
  
  message("Successfully saved the latex table for additional production in acidic cropland.")
  
  
  
  # Step 4: Process additional production value data
  df_additional_production_value <- csv_list[[paste0("additional-value_", country_iso3, "-admin1.csv")]] %>%
    dplyr::arrange(desc(high_hp)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::filter(high_hp > 0)%>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10)) %>%
    dplyr::select(NAME_1, ends_with("_add_usd")) %>%
    tidyr::pivot_longer(cols = -NAME_1,
                        names_to = "crop",
                        values_to = "additional_production_usd") %>%
    dplyr::mutate(crop = gsub("_add_usd", "", crop)) %>%
    dplyr::left_join(crops_df, by = c("crop" = "crop")) %>%
    dplyr::mutate(additional_production_usd = additional_production_usd / 1000) %>%
    dplyr::filter(additional_production_usd > 0) 
  
  # Step 5: Create the stacked bar plot for additional production value
  plt_additional_production_value <- ggplot(
    df_additional_production_value,
    aes(
      x = reorder(NAME_1, -additional_production_usd),
      y = additional_production_usd,
      fill = crop_types
    )
  ) +
    geom_bar(stat = "identity",
             width = 0.7,
             position = "stack") +
    scale_fill_manual(values = alpha(eia_colors, 0.95)) +
    labs(
      title = paste0(
        "Additional production value in the top-10 most acidic  ",
        level_name,
        ""
      ),
      subtitle = "Not segregated by acidic cropland",
      x = level_name,
      y = " Additional Production Value (x1000 USD)",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      title = element_blank(),
      axis.text.y = element_text(size = 12, family = my_font_2),
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
    )
  
  # Step 6: Save the plot for additional production value
  save_plot(
    plt_additional_production_value,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  
  # additional production value table
  
  df_additional_production_value_tbl <- df_additional_production_value %>%
    dplyr::group_by(NAME_1, crop_types) %>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10)) %>%
    dplyr::summarise(additional_production_usd = sum(additional_production_usd)) %>%
    tidyr::pivot_wider(names_from = crop_types, values_from = additional_production_usd) %>%
    dplyr::mutate(across(everything(), ~ round(., 1))) %>%
    mutate(mutate(across(everything(), ~ ifelse(.==0, NA, .)))) %>%
    rename(!!level_name := NAME_1)%>%
    knitr::kable("latex", escape = TRUE, booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), font_size =7) %>%
    kableExtra::column_spec(1, width = "1.8cm")%>%
    as.character() %>%
    gsub("NA", "\\-", .) %>%
    gsub("(?<!\\d)0\\.00(?!\\d)", "-", ., perl = TRUE)%>%
    gsub("Coffee robusta", "\\\\makecell{Coffee \\\\\\\\robusta}", .) %>%
    gsub("Sweet potato", "\\\\makecell{Sweet \\\\\\\\potato}", .) %>%
    gsub("Other-Commodity", "\\\\makecell{Other-\\\\\\\\Commodity}", .) %>%
    gsub("Other-Cereal", "\\\\makecell{Other-\\\\\\\\Cereal}", .) %>%
    gsub("Other-Legume", "\\\\makecell{Other-\\\\\\\\Legume}", .) %>%
    gsub("Other-RTBs", "\\\\makecell{Other-\\\\\\\\RTBs}", .) %>%
    gsub("Other-Non-food",
         "\\\\makecell{Other-\\\\\\\\Non-food}",
         .)
  
  # Step 7: Save the table as a .tex file
  writeLines(df_additional_production_value_tbl,
             file.path(
               figures_and_tables_path,
               paste0(country_iso3, "_table_additional_production_value.tex")
             ))
  
  
  message(
    "Successfully created and saved the plots for additional production and value in acidic cropland."
  )
}

# Example usage:
# create_and_save_additional_production_plots("KEN", "Kenya", csv_list, "path/to/save/plots/", crop_types)
#' Create and Save Plot and Table for Lime Requirements in Acidic Cropland
#'
#' This function processes data related to lime requirements for acid soil remediation in a specified country,
#' generates a stacked bar plot showing the lime requirements in the top 10 most acidic regions, and saves the plot as a PNG file.
#' It also creates a LaTeX table summarizing the lime requirements and saves it as a `.tex` file.
#'
#' @param country_iso3 A character string representing the ISO3 code of the country.
#' @param country_name A character string representing the name of the country, used in the plot title.
#' @param csv_list A list of data frames, typically generated from the get_result_tables function, containing the relevant CSV data.
#' @param figures_and_tables_path A character string specifying the directory path where the plot and table should be saved.
#' @param crop_types A data frame containing information about crop types used to map crop codes to crop names.
#'
#' @return None. The function generates and saves both the plot and the table but does not return a value.
#'
#' @export
create_and_save_lime_requirements <- function(country_iso3,
                                              country_name,
                                              csv_list,
                                              figures_and_tables_path,
                                              crop_types) {
  # Step 1: Process lime requirements data
  #csv_list <- result_tables
  df_lime_requirements <- csv_list[[paste0("lime-requirements_", country_iso3, "-admin1.csv")]] %>%
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
    dplyr::filter(lime_requirements_t > 0)%>%
    dplyr::mutate(lime_requirements_t = lime_requirements_t / 1000)
  
  
  df_lime_requirements_region <- df_lime_requirements %>%
    dplyr::group_by(NAME_1) %>%
    dplyr::summarise(lime_requirements_t = sum(lime_requirements_t))
  
  # Step 2: Create the stacked bar plot for lime requirements
  plt_lime_requirements <- ggplot(df_lime_requirements,
                                  aes(
                                    x = reorder(NAME_1, -lime_requirements_t),
                                    y = lime_requirements_t,
                                    fill = crop_types
                                  )) +
    geom_bar(stat = "identity",
             width = 0.7,
             position = "stack") +
    scale_fill_manual(values = alpha(eia_colors, 0.95)) +
    labs(
      title = paste0("Lime requirements in the top-10 most acidic  ", level_name, ""),
      subtitle = "Not segregated by acidic cropland",
      x = level_name,
      y = " Lime Requirements (x1000 t)",
      fill = ""
    )+
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
              inherit.aes = FALSE)  +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      title = element_blank(),
      axis.text.y = element_text(size = 12, family = my_font_2),
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
    )
  
  # Step 3: Save the plot for lime requirements
  save_plot(
    plt_lime_requirements,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  
  
  # Step 4: Create the summary table for lime requirements
  df_lime_requirements_tbl <- df_lime_requirements %>%
    dplyr::group_by(NAME_1, crop_types) %>%
    mutate(NAME_1 = stringr::str_wrap(NAME_1, width = 10)) %>%
    dplyr::summarise(lime_requirements_t = sum(lime_requirements_t)) %>%
    #dplyr::mutate(lime_requirements_t = lime_requirements_t / 1000) %>%
    dplyr::mutate(lime_requirements_t = ifelse(lime_requirements_t < 0.00001, NA, lime_requirements_t)) %>%
    tidyr::pivot_wider(names_from = crop_types, values_from = lime_requirements_t) %>%
    dplyr::mutate(across(everything(), ~ round(., 2))) %>%
    rename(!!level_name := NAME_1)%>%
    knitr::kable("latex", escape = TRUE, booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), font_size =7) %>%
    kableExtra::column_spec(1, width = "1.8cm")%>%
    as.character() %>%
    gsub("NA", "\\-", .) %>%
    gsub("(?<!\\d)0\\.00(?!\\d)", "-", ., perl = TRUE)%>%
    gsub("Other-Commodity", "\\\\makecell{Other-\\\\\\\\Commodity}", .) %>%
    gsub("Coffee robusta", "\\\\makecell{Coffee \\\\\\\\robusta}", .) %>%
    gsub("Sweet potato", "\\\\makecell{Sweet \\\\\\\\potato}", .) %>%
    gsub("Other-Cereal", "\\\\makecell{Other-\\\\\\\\Cereal}", .) %>%
    gsub("Other-Legume", "\\\\makecell{Other-\\\\\\\\Legume}", .) %>%
    gsub("Other-RTBs", "\\\\makecell{Other-\\\\\\\\RTBs}", .) %>%
    gsub("Other-Non-food",
         "\\\\makecell{Other-\\\\\\\\Non-food}",
         .)
  
  # Step 5: Save the table as a .tex file
  writeLines(df_lime_requirements_tbl,
             file.path(
               figures_and_tables_path,
               paste0(country_iso3, "_table_lime_requirements.tex")
             ))
  
  message(
    "Successfully created and saved the plot and table for lime requirements in acidic cropland."
  )
  
}

# Example usage:
# create_and_save_lime_requirements("KEN", "Kenya", csv_list, "path/to/save/plots_and_tables/", crop_types)
#' Create and Save Plots for Profitable Production in Acidic Cropland
#'
#' This function processes data related to profitable production in the top 10 most acidic counties of a specified country,
#' generates three plots: one showing the profitable production in USD, another showing the percentage of profitable production,
#' and a third showing the total profitable production in hectares. All plots are saved as PNG files.
#'
#' @param country_iso3 A character string representing the ISO3 code of the country.
#' @param country_name A character string representing the name of the country, used in the plot title.
#' @param csv_list A list of data frames, typically generated from the get_result_tables function, containing the relevant CSV data.
#' @param figures_and_tables_path A character string specifying the directory path where the plots should be saved.
#' @param crop_types A data frame containing information about crop types used to map crop codes to crop names.
#'
#' @return None. The function generates and saves the plots but does not return a value.
#'
#' @export
create_and_save_profitable_production_plots <- function(country_iso3,
                                                        country_name,
                                                        csv_list,
                                                        figures_and_tables_path,
                                                        crop_types) {
  # Step 1: Identify the top 10 most acidic counties
  high_hp <- csv_list[[paste0("lime-requirements_", country_iso3, "-admin1.csv")]] %>%
    dplyr::arrange(desc(high_hp)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::pull(NAME_1)
  
  # Step 2: Process profitable production data
  df_profitable_production <- csv_list[[paste0("new-profitable-production_", country_iso3, "-admin1.csv")]] %>%
    dplyr::filter(NAME_1 %in% high_hp) %>%
    dplyr::select(NAME_1, ends_with("actual_profit")) %>%
    tidyr::pivot_longer(cols = -NAME_1,
                        names_to = "crop",
                        values_to = "actual_profit") %>%
    dplyr::mutate(crop = gsub("_actual_profit", "", crop)) %>%
    dplyr::left_join(crops_df, by = c("crop" = "crop")) %>%
    dplyr::filter(actual_profit > 0) 
  
  my_comma <- scales::label_comma(accuracy = .2,
                                  big.mark = ",",
                                  decimal.mark = ".")
  
  total_profit <- df_profitable_production %>%
    dplyr::group_by(NAME_1) %>%
    dplyr::summarise(total_profit = sum(actual_profit)) %>%
    dplyr::mutate(total_profit_2 = my_comma(total_profit))
  
  df_profitable_production <- df_profitable_production %>%
    dplyr::left_join(total_profit, by = "NAME_1")
  
  # Step 3: Create the stacked bar plot for profitable production in USD
  plt_profitable_production <- ggplot(df_profitable_production,
                                      aes(
                                        x = reorder(NAME_1, -actual_profit),
                                        y = actual_profit,
                                        fill = crop_types
                                      )) +
    geom_bar(stat = "identity",
             width = 0.7,
             position = "stack") +
    scale_fill_manual(values = alpha(eia_colors, 0.95)) +
    geom_text(
      aes(y = total_profit, label = total_profit_2),
      vjust = -0.5,
      size = 3,
      family = my_font_2
    ) +
    labs(
      title = paste0(
        "Profitable production in the top-10 most acidic  ",
        level_name,
        ""
      ),
      subtitle = "Not segregated by acidic cropland",
      x = level_name,
      y = " Profitable Production (USD)",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      title = element_blank(),
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
  
  # Step 4: Save the plot for profitable production in USD
  save_plot(
    plt_profitable_production,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  
  # Step 5: Process data for percentage of profitable production
  df_area_profitable_per <- csv_list[[paste0("new-profitable-production_", country_iso3, "-admin1.csv")]] %>%
    dplyr::filter(NAME_1 %in% high_hp) %>%
    dplyr::select(NAME_1, ends_with("_ha"), ends_with("_spam")) %>%
    tidyr::pivot_longer(cols = -NAME_1,
                        names_to = "crop",
                        values_to = "area_ha") %>%
    tidyr::separate(crop, c("crop", "type"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(type = dplyr::case_when(type == "area" ~ "Profitable_ha", TRUE ~ "Total_ha")) %>%
    tidyr::pivot_wider(names_from = type, values_from = area_ha) %>%
    dplyr::mutate(profitable_perc = Profitable_ha / Total_ha * 100) %>%
    dplyr::filter(profitable_perc > 0) %>%
    dplyr::left_join(crops_df, by = c("crop" = "crop")) 
  
  # Step 6: Create the stacked bar plot for percentage of profitable production
  plt_profitable_perc <- ggplot(df_area_profitable_per,
                                aes(
                                  x = reorder(NAME_1, -profitable_perc),
                                  y = profitable_perc,
                                  fill = crop_types
                                )) +
    geom_bar(stat = "identity",
             width = 0.7,
             position = "stack") +
    scale_fill_manual(values = alpha(eia_colors, 0.95)) +
    labs(
      title = paste0(
        "Percentage of profitable production in the top-10 most acidic  ",
        level_name,
        ""
      ),
      subtitle = "Not segregated by acidic cropland",
      x = level_name,
      y = " Profitable Production (%)",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      title = element_blank(),
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
  
  # Step 7: Save the plot for percentage of profitable production
  save_plot(
    plt_profitable_perc,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  
  message(
    "Successfully created and saved the plots for profitable production in acidic cropland."
  )
  # Step 8: Process data for total profitable production
  df_total_profitable <- df_area_profitable_per %>%
    dplyr::group_by(NAME_1) %>%
    dplyr::summarise(Profitable_ha = sum(Profitable_ha),
                     total_ha = sum(Total_ha)) %>%
    dplyr::mutate(profitable_perc = Profitable_ha / total_ha * 100)
  
  # Step 9: Create the plot for total profitable production
  plt_total_profitable <- ggplot(df_total_profitable,
                                 aes(
                                   x = reorder(NAME_1, -profitable_perc),
                                   y = profitable_perc,
                                   fill = NAME_1
                                 )) +
    geom_bar(stat = "identity",
             width = 0.7,
             position = "stack") +
    scale_fill_manual(values = alpha(c(eia_colors, Darjeeling1), 0.8)) +
    geom_text(
      aes(label = round(profitable_perc, 2)),
      position = position_stack(vjust = 1.05),
      size = 3,
      family = my_font_2
    ) +
    labs(
      title = paste0(
        "Total profitable production in the top-10 most acidic  ",
        level_name,
        ""
      ),
      subtitle = "Not segregated by acidic cropland",
      x = level_name,
      y = " Profitable Production (ha)",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = my_font_2, size = 16),
      title = element_blank(),
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
  
  # Step 10: Save the plot for total profitable production
  save_plot(
    plt_total_profitable,
    width = 14,
    height = 7,
    country_iso3 = country_iso3
  )
  message(
    "Successfully created and saved the plots for total profitable production in acidic cropland."
  )
}

# Example usage:
# create_and_save_profitable_production_plots("KEN", "Kenya", csv_list, "path/to/save/plots/", crop_types)
#' Generic Raster Plotting Function
#'
#' This function plots raster data with an optional overlay of administrative boundaries.
#' It supports various customization options like title, color scheme, and file naming.
#'
#' @param raster_data A terra SpatRaster object representing the raster data to be plotted.
#' @param cty A terra SpatVector object representing the administrative boundaries to overlay.
#' @param plot_title A character string specifying the title of the plot.
#' @param file_name A character string specifying the name of the file to save the plot.
#' @param breaks A numeric vector of break points for the color scale.
#' @param color_scheme A character vector specifying the colors for the plot.
#' @param width Plot width in pixels. Default is 3200.
#' @param height Plot height in pixels. Default is 1400.
#' @param res Plot resolution in dots per inch (DPI). Default is 500.
#' @param units Plot dimensions in units. Default is "px".
#' @param pointsize Base font size. Default is 12.
#'
#' @return None. The function generates and saves the plot but does not return a value.
#'
#' @export
generic_raster_plot <- function(raster_data,
                                cty,
                                plot_title,
                                file_name,
                                breaks = NULL,
                                color_scheme,
                                width = 3400,
                                height = 1400,
                                res = 500,
                                units = "px",
                                pointsize = 11) {
  png(
    filename = file_name,
    width = width,
    height = height,
    res = res,
    units = units,
    pointsize = pointsize
  )
  par(family = my_font_2)
  terra::plot(raster_data,
              main = plot_title,
              breaks = breaks,
              col = color_scheme)
  terra::plot(
    cty,
    add = TRUE,
    border = 'black',
    alpha = 0.3,
    lwd = 0.3
  )
  dev.off()
}

#' Create and Save Raster-Based Plots for Various Indicators
#'
#' This function processes raster data to create and save various plots using a generic plotting function.
#'
#' @param country_iso3 A character string representing the ISO3 code of the country.
#' @param input_path A character string specifying the directory path where the input raster files are located.
#' @param figures_and_tables_path A character string specifying the directory path where the plots should be saved.
#' @param output_path A character string specifying the directory path where the crop raster files are located.
#'
#' @return None. The function generates and saves the plots but does not return a value.
#'
#' @export
create_and_save_raster_plots <- function(country_iso3,
                                         input_path,
                                         figures_and_tables_path,
                                         output_path) {
  
  #country_iso3 <- "TZA"
  # Load administrative boundaries
  cty_1 <- geodata::gadm(country_iso3, level = 1, path = input_path)
  
  
  raster_pairs <- list(
    
  )
  
  # Load and process additional raster data for lime rate, yield loss, yield response, and ROI
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
  
  
  second_crop <- main_crop$crop
  second_crop_name <- main_crop$crop_type

  
  lime_rate_maize <- terra::crop(exante_output[[grep('MAIZ_lr_tha', names(exante_output))]], cty_1, mask = TRUE)
  
  lime_rate_bean <- terra::crop(exante_output[[grep(paste0(second_crop,'_lr_tha'), names(exante_output))]], cty_1, mask = TRUE)
  
  # all crops mean yield loss
  crop_yield_loss<-exante_output[[grep("_loss",names(exante_output))]]
  crop_yield_loss<-terra::crop(crop_yield_loss,cty_1,mask=TRUE)
  crop_yield_loss <- (1-crop_yield_loss)*100
  crop_yield_loss <- terra::ifel(crop_yield_loss !=0,crop_yield_loss, NA)
  
  # mean crop yield loss
  mean_crop_yield_loss<-terra::app(crop_yield_loss,mean,na.rm=TRUE)
  
  
  
  yield_loss_maize <- terra::crop(exante_output[[grep('MAIZ_loss', names(exante_output))]], cty_1, mask = TRUE)
  yield_loss_maize <- (1-yield_loss_maize)*100
  yield_loss_maize <- terra::ifel(yield_loss_maize !=0, yield_loss_maize, NA)
  yield_loss_bean <- terra::crop(exante_output[[grep(paste0(second_crop,'_loss'), names(exante_output))]], cty_1, mask = TRUE)
  #yield_loss_bean <- (1-yield_loss_bean)*100
  yield_loss_bean <- (1-yield_loss_bean)*100
  yield_loss_bean <- terra::ifel(yield_loss_bean !=0, yield_loss_bean, NA)
  
  yield_response_maize <- terra::crop(exante_output[[grep('MAIZ_yresp_tha', names(exante_output))]], cty_1, mask = TRUE)
  yield_response_bean <- terra::crop(exante_output[[grep(paste0(second_crop,'_yresp_tha'), names(exante_output))]], cty_1, mask = TRUE)
  
  roi_maize <- terra::crop(exante_output[[grep('MAIZ_roi_usha', names(exante_output))]], cty_1, mask = TRUE)
  roi_bean <- terra::crop(exante_output[[grep(paste0(second_crop,'_roi_usha'), names(exante_output))]], cty_1, mask = TRUE)
  
  # Add additional raster pairs for plotting
  raster_pairs <- append(raster_pairs, list(
    list(
      raster1 = lime_rate_maize,
      title1 = 'Maize Lime Rate (t/ha)',
      raster2 = lime_rate_bean,
      title2 = paste0(second_crop_name, ' Lime Rate (t/ha)'),
      file_suffix = "lime_rate"
    ),
    list(
      raster1 = mean_crop_yield_loss,
      title1 = 'Average Crop Yield Loss (%)',
      raster2 = yield_loss_maize,
      title2 = 'Maize Yield Loss (%)',
      file_suffix = "yield_loss"
    ),
    list(
      raster1 = yield_response_maize,
      title1 = 'Maize Yield Response (t/ha)',
      raster2 = yield_response_bean,
      title2 = paste0(second_crop_name,' Yield Response (t/ha)'),
      file_suffix = "yield_response"
    ),
    list(
      raster1 = roi_maize,
      title1 = 'Maize ROI (USD/USD)',
      raster2 = roi_bean,
      title2 = paste0(second_crop_name,' ROI (USD/USD)'),
      file_suffix = "roi"
    )
  ))
  
  # Loop through the raster pairs and generate plots
  for (raster_pair in raster_pairs) {
    max_value_1 <- terra::global(raster_pair$raster1, fun = "max", na.rm = TRUE)$max
    max_value_1 <- max_value_1 - (max_value_1 * 0.1)
    max_value_2 <- terra::global(raster_pair$raster2, fun = "max", na.rm = TRUE)$max
    max_value_2 <- max_value_2 - (max_value_2 * 0.1)
    png(
      filename = paste0(
        figures_and_tables_path,
        country_iso3,
        "_",
        raster_pair$file_suffix,
        ".png"
      ),
      width = 3200,
      height = 1400,
      res = 500,
      units = "px",
      pointsize = 12
    )
    
    par(mfrow = c(1, 2), mar=c(2,2,2,1), xaxs='i', yaxs='i', oma = c(0, 0, 0, 0), family = my_font_2,col.main = "#7BC142"  )
    
    # Plot the first raster
    terra::plot(raster_pair$raster1,
                main = raster_pair$title1,
                breaks = raster_pair$breaks1,
                range = c(0,max_value_1),
                fill_range = TRUE,
                col = Zissou1)
    terra::plot(
      cty_1,
      add = TRUE,
      #border = 'bl',
      alpha = 0.3,
      lwd = 0.3
    )
    
    # Plot the second raster
    terra::plot(raster_pair$raster2,
                main = raster_pair$title2,
                range = c(0,max_value_2),
                fill_range = TRUE,
                col = Zissou1)
    terra::plot(
      cty_1,
      add = TRUE,
      #border = '#024b7a',
      alpha = 0.3,
      lwd = 0.3
    )
    
    dev.off()
  }
}

generate_acidity_distribution <- function(country_iso3,
                                          country_name,
                                          soil_data,
                                          figures_and_tables_path) {
  
  ctys <- geodata::gadm(country_iso3, level = 1, path = input_path)
  file_name <- paste0(figures_and_tables_path, country_iso3, "_acidic_cropland_distribution.jpg")
  
  if(country_iso3 %in% c("ETH", "NGA")){
    height <- 5
    width <- 11
  } else {
    height <- 6
    width <- 12
  }
  
  hp <- soil_data[['hp_sat']]
  hp <- terra::crop(hp, ctys, mask = TRUE)
  
  ph <- soil_data[['ph']]
  ph <- terra::crop(ph, ctys, mask = TRUE)
  
  png(file_name, units="in", width=width, height=height, res=1000)
  par(mfrow=c(1,2), family=my_font_2, col.main = "#7BC142")
  pal <- colorRampPalette(c(alpha('#ebebeb',0.9),"gold",  "firebrick", "#ff4d41"))
  #pal <- colorRampPalette(c('#D5D5E3',eia_colors))
  terra::plot(ctys, mar=c(4,2,2,1), clip=F, col='white', main='Acidity saturation (% of ECEC)', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
  terra::plot(hp, breaks=c(0, 10, 20, Inf), col=pal(3), legend=F, main='', add=T)
  # Add a horizontal legend outside the plot
  legend("bottom", inset = -0.18, # Place legend outside the bottom of the plot
         bty = 'n', cex = 1.2,  # Horizontal layout (ncol=3)
         legend = c('< 10', '10 - 20', '> 20'), 
         fill = pal(3), horiz = TRUE, xpd = TRUE) # xpd allows drawing outside plot area
  
  terra::plot(ctys, axes=F, add=T, border='#024b7a', lwd=1)
  
  pal_2 <- c("firebrick","#ff4d41", "#ebebeb", "gold" )
  
  terra::plot(ctys, mar=c(4,2,2,1), clip=F, col='white', main='Soil pH', legend=F, pax=list(cex.axis=1.6), cex.main=1.5)
  terra::plot(ph, breaks=c(0, 4.5, 5.5, 6.5, Inf), col=pal_2, legend=F, main='', add=T)
  legend("bottom", inset = -0.18, bty='n', cex=1.2,  box.col="white", title="", 
         legend=c('< 4.5', '4.5 - 5.5',  '5.5 - 6.5',  '> 6.5'), 
         fill = pal_2, horiz=T, xpd = TRUE)
  terra::plot(ctys, axes=F, add=T, border='#024b7a', lwd=1)
  
  dev.off()
  
  message("Successfully created and saved the plot for acidic cropland distribution.")
}


generate_profitability_maps <- function(country_iso3,
                                        country_name,
                                        figures_and_tables_path,
                                        output_path) {
 
  ctys <- geodata::gadm(country_iso3, level = 1, path = input_path)
  
  if(country_iso3 %in% c("ETH", "NGA")){
    height <- 6
    inset2 <- -0.22
  } else {
    height <- 7
    inset2 <- -0.19
  }
  
  
  # yield_factor <- 1 and lp <- 100
  profitability_map_100_1 <- get_profitability_results(yield_factor=1, lp=100)
  profitability_map_100_2 <- get_profitability_results(yield_factor=2, lp=100)
  profitability_map_50_1 <- get_profitability_results(yield_factor=1, lp=50)
  profitability_map_50_2 <- get_profitability_results(yield_factor=2, lp=50)
  
  pal_2<- colorRampPalette(c('#D5D5E3', '#E27B00', "#1D84FF", "#ADD8E6", "#00640D"))
  
  png(paste0(figures_and_tables_path, country_iso3, "_profitability_sensetivity.png"), units="in", width=7, height=6, res=1000,pointsize = 7 )
  
  par(mfrow=c(2,2), mar=c(4,1,4,1),  xaxs='i', yaxs='i', family=my_font_2,col.main = "#7BC142"  )
  terra::plot(ctys, mar=c(4,2,4,2), clip=F, col='#F7F9F7', main='@ current price and current productivity', legend=F, pax=list(cex.axis=1.6), cex.main=1.6)
  terra::plot(profitability_map_100_1, breaks=c(0, 20, 40, 60, 80, 100), legend=F, main='', add=T, col=pal_2(6)) 
  terra::plot(ctys, axes=F, add=T)
  
  terra::plot(ctys,  mar=c(4,2,4,2),clip=F, col='#F7F9F7', main='@ current lime price and doubling productivity', legend=F, pax=list(cex.axis=1.6), cex.main=1.6)
  terra::plot(profitability_map_100_2, breaks=c(0, 20, 40, 60, 80, 100), legend=F, main='', add=T, col=pal_2(6))
  terra::plot(ctys, axes=F, add=T)
  
  terra::plot(ctys, mar=c(6,2,4,2),  clip=F, col='#F7F9F7', main='@ 50% reduction in lime price and current productivity', legend=F, pax=list(cex.axis=1.6), cex.main=1.6)
  terra::plot(profitability_map_50_1, breaks=c(0, 20, 40, 60, 80, 100), legend=F, main='', add=T, col=pal_2(6))
  terra::plot(ctys, axes=F, add=T)
  
  terra::plot(ctys, mar=c(6,2,4,2), clip=F, col='#F7F9F7', main='@ 50% reduction in lime price and doubling productivity', legend=F, pax=list(cex.axis=1.6), cex.main=1.6)
  terra::plot(profitability_map_50_2, breaks=c(0, 20, 40, 60, 80, 100), legend=F, main='', add=T, col=pal_2(6))
  terra::plot(ctys, axes=F, add=T)
  legend("bottom", inset = -0.25, bty='n', cex=1.5,  box.col="white", title="Area profitable (%)", legend=c('0 - 20', '20 - 40', '40 - 60', '60 - 80', '80 - 100'), fill = pal_2(6), horiz=T, xpd = TRUE)
  
  
  dev.off()
  
  profitability_map_amount_100_1 <- get_profitability_results(yield_factor=1, lp=100, is_percent=FALSE)
  profitability_map_amount_100_2 <- get_profitability_results(yield_factor=2, lp=100, is_percent=FALSE)
  profitability_map_amount_50_1 <- get_profitability_results(yield_factor=1, lp=50, is_percent=FALSE)
  profitability_map_amount_50_2 <- get_profitability_results(yield_factor=2, lp=50, is_percent=FALSE)
  
  pal_3 <- colorRampPalette(c('#D5D5E3', '#E27B00', "#1D84FF", "#ADD8E6", "#00640D"))
  
  png(paste0(figures_and_tables_path, country_iso3, "_profitability_sensetivity_amount.jpg"), units="in", width=7, height=height, res=1000,pointsize = 7 )
  
  par(mfrow=c(2,2), mar=c(4,1,4,1),  xaxs='i', yaxs='i', family=my_font_2,col.main = "#7BC142"   )
  terra::plot(ctys, mar=c(4,2,4,2), clip=F, col='#F7F9F7', main='@ current price and current productivity', legend=F, pax=list(cex.axis=1.6), cex.main=1.8)
  terra::plot(profitability_map_amount_100_1,breaks=c(0, 100, 200, 300, Inf), legend=F, main='', add=T, col=pal_3(4))
  terra::plot(ctys, axes=F, add=T)
  
  terra::plot(ctys,  mar=c(4,2,4,2),clip=F, col='#F7F9F7', main='@ current lime price and doubling productivity', legend=F, pax=list(cex.axis=1.6), cex.main=1.8)
  terra::plot(profitability_map_amount_100_2, breaks=c(0, 100, 200, 300, Inf),legend=F, main='', add=T, col=pal_3(4))
  terra::plot(ctys, axes=F, add=T)
  
  terra::plot(ctys, mar=c(6,2,4,2),  clip=F, col='#F7F9F7', main='@ 50% reduction in lime price and current productivity', legend=F, pax=list(cex.axis=1.6), cex.main=1.6)
  terra::plot(profitability_map_amount_50_1, breaks=c(0, 100, 200, 300, Inf),legend=F, main='', add=T, col=pal_3(4))
  terra::plot(ctys, axes=F, add=T)
  
  terra::plot(ctys, mar=c(6,2,4,2), clip=F, col='#F7F9F7', main='@ 50% reduction in lime price and doubling productivity', legend=F, pax=list(cex.axis=1.6), cex.main=1.6)
  terra::plot(profitability_map_amount_50_2, breaks=c(0, 100, 200, 300, Inf),legend=F, main='', add=T, col=pal_3(4))
  terra::plot(ctys, axes=F, add=T)
  legend("bottom", inset = inset2, bty='n', cex=1.3,  box.col="white", title="Area Weighted Profit (USD/ha)", legend=c('0 - 100', '100 - 200', '200 - 300', '>=300'), fill = pal_3(4), horiz=T, xpd = TRUE)
  
  dev.off()
  
  
  
  
  message("Successfully created and saved the plot for the profitability map.")
}

get_profitability_results <- function(yield_factor, lp, is_percent=TRUE) {
  ctys <- geodata::gadm(country_iso3, level = 1, path = input_path)
  profitable_raster <- terra::rast(Sys.glob(
    paste0(output_path1, 'crop-rasters-final/','*_',lp,'_',yield_factor,'_profit_rasters.tif')
  ))
  all_crops_ha <- profitable_raster[[grep('_ha_spam', names(profitable_raster))]]
  all_crops_ha <- sum(all_crops_ha, na.rm = T)
  all_crops_ha_profit <- profitable_raster[[grep('area_profitable_ha', names(profitable_raster))]]
  all_crops_ha_profit <- sum(all_crops_ha_profit, na.rm = T)
  a_p <- 100 * all_crops_ha_profit / all_crops_ha
  a_p <- terra::crop(a_p, ctys, mask = TRUE)
  
  all_crops_ha <- profitable_raster[[grep('_ha_spam', names(profitable_raster))]]
  all_crops_gm <- profitable_raster[[grep('actual_profit', names(profitable_raster))]]
  w_p <- terra::weighted.mean(all_crops_gm, all_crops_ha, na.rm = T)
  w_p <- terra::crop(w_p,ctys, mask = TRUE)
  
  if (is_percent) {
    return(a_p)
  } else {
    return(w_p)
  }
}




# Example usage:
# create_and_save_raster_plots("KEN", "path/to/input/", "path/to/save/plots/", "path/to/output/")
#' Generate Summary Visuals for Various Indicators
#'
#' This function processes and generates summary visuals, including plots for acidic cropland, crop area,
#' additional production, lime requirements, profitable production, and raster-based indicators like lime rate,
#' yield loss, yield response, and ROI for specific crops in a specified country.
#'
#' @param country_iso3 A character string representing the ISO3 code of the country.
#' @param country_name A character string representing the name of the country, used in plot titles.
#' @param csv_list A list of data frames containing relevant CSV data.
#' @param input_path A character string specifying the directory path where the input raster files are located.
#' @param figures_and_tables_path A character string specifying the directory path where the plots should be saved.
#' @param output_path A character string specifying the directory path where the crop raster files are located.
#' @param crop_types A data frame containing information about crop types used to map crop codes to crop names.
#'
#' @return None. The function generates and saves various plots and tables.
#'
#' @export
generate_summary_visuals <- function(country_iso3,
                                     country_name,
                                     csv_list,
                                     input_path,
                                     figures_and_tables_path,
                                     output_path,
                                     crop_types,
                                     soil_data) {
  cat(
    "Starting the generation of summary visuals for",
    country_name,
    "(",
    country_iso3,
    ")\n"
  )
  
  # Initialize progress bar
  total_steps <- 6
  pb <- txtProgressBar(min = 0, max = total_steps, style = 3)
  
  # 1. Create and save plots for acidic cropland
  cat("\nStep 1: Creating and saving plots for acidic cropland...\n")
  gen_acidic_crop_area(country_iso3,
                                       csv_list,
                                       figures_and_tables_path)
  cat("Step 1 completed.\n")
  setTxtProgressBar(pb, 1)
  
  # 2. Create and save plots and tables for crop area distribution in acidic cropland
  cat(
    "\nStep 2: Creating and saving plots and tables for crop area distribution in acidic cropland...\n"
  )
  create_and_save_crop_area_acidic(country_iso3,
                                   country_name,
                                   csv_list,
                                   figures_and_tables_path,
                                   crop_types)
  cat("Step 2 completed.\n")
  setTxtProgressBar(pb, 2)
  
  # 3. Create and save additional production plots
  cat("\nStep 3: Creating and saving additional production plots...\n")
  create_and_save_additional_production_plots(country_iso3,
                                              country_name,
                                              csv_list,
                                              figures_and_tables_path,
                                              crop_types)
  cat("Step 3 completed.\n")
  setTxtProgressBar(pb, 3)
  
  # 4. Create and save plots and tables for lime requirements
  cat("\nStep 4: Creating and saving plots and tables for lime requirements...\n")
  create_and_save_lime_requirements(country_iso3,
                                    country_name,
                                    csv_list,
                                    figures_and_tables_path,
                                    crop_types)

  cat("Step 4 completed.\n")
  setTxtProgressBar(pb, 4)
  
  # 5. Create and save plots for profitable production
  cat("\nStep 5: Creating and saving plots for profitable production...\n")
  create_and_save_profitable_production_plots(country_iso3,
                                              country_name,
                                              csv_list,
                                              figures_and_tables_path,
                                              crop_types)
  cat("Step 5 completed.\n")
  setTxtProgressBar(pb, 5)
  
  # 6. Create and save raster-based plots for various indicators
  cat("\nStep 6: Creating and saving raster-based plots for various indicators...\n")
  create_and_save_raster_plots(country_iso3,
                               input_path,
                               figures_and_tables_path,
                               output_path)
  cat("Step 6 completed.\n")
  setTxtProgressBar(pb, 6)
  
  # 7. Generate and save the plot for acidic cropland distribution
  cat("\nStep 7: Generating and saving the plot for acidic cropland distribution...\n")
  generate_acidity_distribution(country_iso3,
                                country_name,
                                soil_data,
                                figures_and_tables_path)
  
  # 8. Generate and save the plot for profitability maps
  cat("\nStep 8: Generating and saving the plot for profitability maps...\n")
  generate_profitability_maps(country_iso3,
                               country_name,
                               figures_and_tables_path,
                               output_path)
  
  
  
  # Close the progress bar
  close(pb)
  
  cat(
    "\nSummary visuals generation completed for",
    country_name,
    "(",
    country_iso3,
    ").\n"
  )
}

