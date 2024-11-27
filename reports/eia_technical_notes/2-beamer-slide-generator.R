#' Generate a LaTeX Slide Deck for a Country's Summary Data
#'
#' This function generates a LaTeX slide deck summarizing data on acid cropland and lime requirements
#' for a specified country. The generated slide deck includes various sections such as acidic cropland,
#' crop area distribution, yield loss, lime requirements, yield response, additional production,
#' ROI for lime application, and profitability of lime application.
#'
#' @param country_code A character string representing the ISO3 code of the country (e.g., "KEN" for Kenya).
#' @param report_path A character string specifying the directory path where the generated LaTeX file should be saved.
#' @param level_name A character string representing the administrative level name (e.g., "counties", "regions").
#'
#' @return None. The function writes a LaTeX file to the specified directory.
#'
#' @examples
#' \dontrun{
#' write_latex_slidedeck("KEN", "path/to/report/directory/", "counties")
#' }
#' 
write_latex_slidedeck <- function(country_code, report_path, level_name, figures_and_tables_path) {
  title_text <- paste("Summary Data on Acid Cropland and Lime Requirements in", country_name)
  subtitle_text <- paste(country_name, "-Summary", sep = "-")
  second_crop <- main_crop$crop
  second_crop_name <- main_crop$crop_type
  # Path for the images and tables
  # figures_and_tables_path <- file.path("../../data-output/figures_and_tables/")
  # "\\usepackage{fouriernc}\n\n",
  # "\\usefonttheme{serif}\n\n",

  # Content of the LaTeX document
  latex_content <- paste0(
    "\\documentclass[xcolor=table, aspectratio=1610]{beamer}\n\n",
    "\\newcommand{\\CountryName}{", country_name, "}\n",
    "\\newcommand{\\BackgroundImagePath}{",  paste0(here::here(),"/reports/eia_technical_notes/figures/slide_cover_eia.png"), "}\n",
    "\\newcommand{\\DashboardImagePath}{",  paste0(here::here(),"/reports/eia_technical_notes/figures/dashboard_2.png"), "}\n",
    "\\input{",  paste0(here::here(),"/reports/eia_technical_notes/eia_presentation_templet.tex"), "}\n",
    "\\begin{document}\n\n",
    "\\CustomTitlePage\n",
    "\\usebackgroundtemplate{}\n",
    "\\begin{frame}\n",
    "\\frametitle{Outlook}\n",
    "\\tableofcontents \n",
    "\\end{frame}\n\n",
    "\\section{Introduction}\n",
    "\\begin{frame}{Introduction}\n",
    "    \\begin{itemize}\n",
    "        \\item The slide deck provides a brief summary of \\textbf{data-driven}, \\textbf{ex-ante analysis} of soil acidity across \\textbf{", country_name, "}, evaluating the scale of affected areas, potential crop yield impacts, and the expected benefits of lime application for proactive acid soil remediation.\n",
    "        \\bigskip\n",
    "        \\item \\textbf{Highlights}:\n",
    "        \\begin{itemize}\n",
    "            \\item \\textbf{Spatial Distribution of Acidic Cropland}: Identifies regions with high acidity levels impacting crop yields.\n",
    "            \\item \\textbf{Yield Loss}: Quantifies productivity losses in key crops due to acidic soils.\n",
    "            \\item \\textbf{Lime Requirements}: Estimates the lime amounts needed to remediate acid soils in these areas.\n",
    "            \\item \\textbf{Economic Potential}: Highlights estimated increases in production, production value and economic benefit from lime application.\n",
    "        \\end{itemize}\n",
    "        \\bigskip\n",
    "        \\item Our goal is to provide policymakers, farmers, and stakeholders with actionable insights to prioritize areas with high potential for productivity gains and economic return from acid soil remediation.\n",
    "    \\end{itemize}\n",
    "\\end{frame}\n",
    "\\section{Spatial Distribution of Soil Acidity}\n",
    "\\begin{frame}{Distribution of exchangeable acidity, as a \\% of the effective cation exchange capacity (ECEC), across ", country_name, "}\n",
    "    \\begin{figure}\n",
    "    \\includegraphics[width=0.99\\textwidth,height=0.70\\textheight]{", file.path(figures_and_tables_path, paste0(country_code, "_acidic_cropland_distribution.jpg")), "}\n",
    " \\end{figure}\n",
    "\\end{frame}\n\n",
    "\\subsection*{Top 10 ", level_name, " in ", country_name, " with the highest acidic cropland}\n\n",
    "\\begin{frame}{Top 10 ", level_name, " in ", country_name, " with the highest acidic cropland}\n",
    "    \\begin{figure}\n",
    "    \\includegraphics[width=0.98\\textwidth, height=0.80\\textheight]{", file.path(figures_and_tables_path, paste0(country_code, "_plt_acidic_cropland_both.png")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    
    "\\section*{Total crop area distribution (ha)}\n",
    "\\subsection*{Total crop area in the top-10 most acidic ", level_name, ", not segregated by acidic cropland}\n",
    "\\begin{frame}{Total crop area in the top-10 most acidic ", level_name, " (not segregated by acidic cropland)}\n",
    "    \\begin{center}\n",
    "        \\includegraphics[width=0.98\\textwidth, height=0.85\\textheight]{", file.path(figures_and_tables_path, paste0(country_code, "_plt_crop_area_acidic.png")), "}\n",
    "    \\end{center}\n",
    "\\end{frame}\n\n",
    "\\begin{frame}{Total crop area in the top-10 most acidic ", level_name, " (not segregated by acidic cropland)}\n",
    "    \\scriptsize\n",
    "    \\input{", file.path(figures_and_tables_path, paste0(country_code, "_table_crop_area_acidic.tex")), "}\n",
    "\\end{frame}\n\n",
    
    "\\section{Additional Production}\n",
    "\\subsection*{Potential for additional production (tone) in top 10 acidic ", level_name, " in ", country_name, "}\n",
    "\\begin{frame}{Potential for additional production (1000 tonnes) in top 10 acidic ", level_name, " in ", country_name, "}\n",
    "    \\begin{figure}\n",
    "    \\includegraphics[width=0.98\\textwidth, height=0.85\\textheight]{", file.path(figures_and_tables_path, paste0(country_code, "_plt_additional_production.png")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    
    "\\subsection*{Potential for additional production (tone) in top 10 acidic ", level_name, " in ", country_name, "}\n",
    "\\begin{frame}{Potential for additional production (1000 tonnes) in top 10 acidic ", level_name, " in ", country_name, "}\n",
    "    \\scriptsize\n",
    "    \\input{", file.path(figures_and_tables_path, paste0(country_code, "_table_additional_production.tex")), "}\n",
    "\\end{frame}\n\n",
    
    "\\subsection*{Potential for additional production value (1000 US\\$) in top 10 acidic ", level_name, " in ", country_name, "}\n",
    "\\begin{frame}{Potential for additional production value (1000 US\\$) in top 10 acidic ", level_name, " in ", country_name, "}\n",
    "    \\begin{figure}\n",
    "    \\includegraphics[width=0.98\\textwidth, height=0.85\\textheight]{", file.path(figures_and_tables_path, paste0(country_code, "_plt_additional_production_value.png")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    
    "\\begin{frame}{Potential for additional production value (1000 US\\$) in top 10 acidic ", level_name, " in ", country_name, "}\n",
    "    \\scriptsize\n",
    "    \\input{", file.path(figures_and_tables_path, paste0(country_code, "_table_additional_production_value.tex")), "}\n",
    "\\end{frame}\n\n",
    
    "\\section{Yield Loss}\n",
    "\\subsection*{Yield loss for Maize and ", second_crop_name , "(\\%) in ", country_name, "}\n",
    "\\begin{frame}{Estimated yield loss due to soil acidity in ", country_name, "}\n",
    "    \\begin{figure}\n",
    "    \\includegraphics[width=0.95\\textwidth]{", file.path(figures_and_tables_path, paste0(country_code, "_yield_loss.png")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    "\\section{Yield Response}\n",
    "\\subsection*{Yield response to lime application in ", country_name, "}\n",
    "\\begin{frame} {Yield response to lime application in ", country_name, "}\n",
    "    \\begin{figure}\n",
    "        \\centering\n",
    "        \\includegraphics[width=0.98\\textwidth]{", file.path(figures_and_tables_path, paste0(country_code, "_yield_response.png")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    "\\section{Lime Requirement}\n",
    "\\subsection*{Lime requirements (t) for acid soil remediation}\n",
    "\\begin{frame} {Lime requirements (1000 tonnes) for acid soil remediation top 10 acidic ", level_name, " in ", country_name, "}\n",
    "    \\begin{figure}\n",
    "        \\centering\n",
    "        \\includegraphics[width=0.95\\textwidth]{", file.path(figures_and_tables_path, paste0(country_code, "_plt_lime_requirements.png")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    "\\subsection*{Lime requirements (t) for acid soil remediation (table)}\n",
    "\\begin{frame} {Lime requirements (1000 tonnes) for acid soil remediation in top 10 acidic ", level_name, " in ", country_name, "}\n",
    "    \\scriptsize\n",
    "    \\input{", file.path(figures_and_tables_path, paste0(country_code, "_table_lime_requirements.tex")), "}\n",
    "\\end{frame}\n\n",
    "\\subsection*{Lime rate for Maize and ", second_crop_name, " in ", country_name, "}\n",
    "\\begin{frame} {Lime rate for Maize and ", second_crop_name, " in ", country_name, "}\n",
    "    \\begin{figure}\n",
    "        \\centering\n",
    "        \\includegraphics[width=0.98\\textwidth]{", file.path(figures_and_tables_path, paste0(country_code, "_lime_rate.png")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    
    "\\section{Economic Benefits}\n",
    
    "\\begin{frame}{Spatial Distribution of crop-area weighted average profitability (US\\$/ha) Under Varying Prices and Productivity assumptions",  " in ", country_name,".", "}\n",
    "   \\begin{figure}\n",
    "        \\centering\n",
    "        \\includegraphics[height=0.85\\textheight]{", file.path(figures_and_tables_path, paste0(country_code, "_profitability_sensetivity_amount.jpg")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    
    
    "\\section{Appendix}\n",
    "\\begin{frame}\n",
    "\\vspace{3cm}\n",
    "         \\centering\\textcolor{my-green}{\\Huge{\\textbf{Appendix}}}\n",
    "\\end{frame}\n",
    "\\begin{frame}\n",
    "    \\frametitle{Modeling approach}\n",
    "    \\begin{figure}\n",
    "        \\centering\n",
    "        \\includegraphics[width=0.95\\textwidth]{", file.path(output_path, paste0("modelling.png")), "}\n",
    "    \\end{figure}\n",
    "\\end{frame}\n\n",
    
    "\\subsection*{Modeling approach}\n",
    "\\begin{frame}\n",
    "    \\frametitle{Modeling approach: Three-Step Approach}\n",
    "    \\begin{itemize}\n",
    "        \\item \\textbf{Lime Requirements Estimation}:\n",
    "        \\begin{itemize}\n",
    "            \\item Estimated using Aramburu Merlos et al. (2023) lime requirement model.\n",
    "            \\item Calculated lime rates to achieve crop-specific Target Acidity Saturation (TAS).\n",
    "            \\item Applied to gridded soil data on exchangeable acidity and ECEC from Hengl et al. (2017).\n",
    "            \\item Converted lime rates to tons of $CaCO{_3}/ha$, assuming a 20 cm soil depth and using soil bulk density data.\n",
    "        \\end{itemize}\n",
    "        \\item \\textbf{Crop Yield Response Estimation}:\n",
    "        \\begin{itemize}\n",
    "            \\item Plateau-linear decay models representing yield loss due to acidity (estimated current yield losses (\\% of maximum yield) due to high acidity saturation).\n",
    "        \\end{itemize}\n",
    "        \\item \\textbf{Economic Benefit Estimation}:\n",
    "        \\begin{itemize}\n",
    "            \\item Return is calculated as the additional yield from liming multiplied by the median crop price.\n",
    "            \\item Cost is determined by multiplying the estimated lime rate by a fixed lime price.\n",
    "        \\end{itemize}\n",
    "    \\end{itemize}\n",
    "\\centering\n",
    "For further details on the methodological approach, visit \\href{www.acidsoils.africa}{www.acidsoils.africa}.\n",
    "\\end{frame}\n",
    
    "\\begin{frame}\n",
    "\\frametitle{Main assumptions and parameters}\n",
    "\\small\n",
    "\\begin{itemize}\n
    \\item Acidic cropland is defined as having acidity saturation levels above 10\\% of the Effective Cation Exchange Capacity (ECEC).
    \\item Crop yields are unaffected by soil acidity until a specific acidity saturation threshold Target Acidity Saturation(TAS) is reached; beyond this threshold, yields decrease linearly, eventually reaching zero at high acidity levels.
    \\item The TAS that separates maximum yield from yield decline is used as a critical value for making liming recommendations.
    \\item Actual yields in acid tropical soils are a fraction of the potential yields in non-acidic soils, determined by the estimated yield loss due to acidity.
    \\item Economic benefits from liming are calculated by multiplying the additional yield after acidic soil remediation by median crop prices from sub-Saharan Africa (2016-2020 FAOSTAT data). The cost of liming is calculated using estimated lime application rates and a fixed lime price of 100 US\\$/ton.
    \\item The benefits of liming are assessed only for the year of application due to a lack of robust, large-scale data on long-term effects in sub-Saharan Africa.
\\end{itemize}\n",
    "\\centering\n",
    "For details, visit \\href{www.acidsoils.africa}{www.acidsoils.africa}.\n",
    "\\end{frame}\n\n",
    "\\CustomEndPage\n",
    "\\usebackgroundtemplate{}\n",
    "\\begin{frame}\n",
    "    \\frametitle{Acknowledgements}\n",
    "\\small\n",
    "\\begin{itemize}\n",
    
    "  \\item  This work was supported by the Bill \\& Melinda Gates Foundation (BMGF) through the Guiding Acid Soil Management Investments in Africa (GAIA) project (Grant no: INV-029117), and the CGIAR Research Excellence in Agronomy Initiative (INV-005431).\n",
    "  \\item  We would like to thank all funders supporting research through contributions to the CGIAR Trust Fund: \\url{https://www.cgiar.org/funders/}.\n",
    " \\item We would like to acknowledge the following national partnerships:\n",
    "\\begin{itemize}\n",
    "\\item Tanzania Agricultural Research Institute (TARI)\n",
    "\\item Rwanda Agriculture and Animal Resources Development Board (RAB)\n",
    "\\item Ethiopian Institute of Agricultural Research (EIAR)\n",
    "\\item Ministry of Agriculture, Ethiopia\n",
    " \\item Zambian Agricultural Research Institute (ZARI) \n",
    "\\end{itemize}\n",
  "\\end{itemize}\n",
    
    "\\end{frame}\n",
    "\\begin{frame}\n",
    

    "       \\textbf{Data and code availability:}\n",
    "        \\begin{itemize}\n",
    "            \\item The data used in this analysis are available from: \\href{www.acidsoils.africa}{www.acidsoils.africa}.\n",
    "            \\item R scripts developed for data manipulation, analysis and visualization are available through the GitHub repository: \\href{https://github.com/EiA2030-ex-ante/gaia-exante-framework}{https://github.com/EiA2030-ex-ante/gaia-exante-framework}.\n",
    "        \\end{itemize}\n",
    "         \\textbf{For further information please contact:}:\n",
    "        \\begin{itemize}\n",
    "            \\item \\href{mailto:b.gebrekidan@cgiar.org}{GEBREKIDAN, Bisrat Haile (CIMMYT-Ethiopia)}\n",
    "            \\item \\href{mailto:j.siliva@cgiar.org}{SILVA, Joao Vasco (CIMMYT-Zimbabwe)}\n",
    "            \\item \\href{mailto:t.sida@cgiar.org}{SIDA, Tesfaye Shiferaw (CIMMYT-Ethiopia) }\n",  
    "            \\item \\href{mailto:J.chamberlin@cgiar.org}{CHAMBERLIN, Jordan (CIMMYT-Kenya)}\n",
    "        \\end{itemize}\n",
    
    "\\end{frame}\n",
    "\\begin{frame}\n",
    "    \\frametitle{References}\n",
    "\\scriptsize\n",
    "    \\begin{itemize}\n",
    "        \\item Hengl, T., Mendes de Jesus, J., Heuvelink, G.B.M., Ruiperez Gonzalez, M., Kilibarda, M., Blagotic, A., Shangguan, W., Wright, M.N., Geng, X., Bauer-Marschallinger, B., Guevara, M.A., Vargas, R., MacMillan, R.A., Batjes, N.H., Leenaars, J.G.B., Ribeiro, E., Wheeler, I., Mantel, S. \\& Kempen, B. (2017). SoilGrids250m: Global gridded soil information based on machine learning. PLOS ONE, 12, 1 – 40.\n",
    "        \\item Hijmans, R.J., Barbosa, M., Ghosh, A., Mandel, A. (2023). geodata: Download Geographic Data. R package version 0.5-9.\n",
    "\\item Walsh, M.G., Wu, W. \\& Walsh, B. (2022). Geosurvey data prediction workflows. Tech. rep., OSF.\n",
    "\\item Yu, Q., You, L., Wood-Sichra, U., Ru, Y., Joglekar, A.K.B., Fritz, S., Xiong, W., Lu, M., Wu, W., \\& Yang, P. (2020). A cultivated planet in 2010: 2. the global gridded agricultural production maps. Earth System Science Data, 34, 252 – 254.\n",
    "\\item IFPRI, 2019. Global Spatially-Disaggregated Crop Production Statistics Data for 2010 Version 2.0. \\url{https://doi.org/10.7910/DVN/PRFF8V}, Harvard Dataverse, V4.\n",
    "\\item FAO, 2023. FAOSTAT. \\url{http://www.fao.org/faostat/en/data/QC}.\n",
    "\\item Aramburu Merlos, F., Silva, J.V., Baudron, F. \\& Hijmans, R.J. (2023). Estimating lime requirements for tropical soils: Model comparison and development. Geoderma, 432, 116421.\n",
    "    \\end{itemize}\n",
    "\\end{frame}\n",
    "\\end{document}"
  )
  
  # Write the LaTeX content to a .tex file
  latex_file_path <- file.path(report_path, paste0("Technical_Brief_Targeting_Agricultural_Lime_for_Acid_Croplands_in_",country_name,".tex"))
  writeLines(latex_content, latex_file_path)
  
  cat("LaTeX slide deck written to:", latex_file_path, "\n")
}

write_rmd_file <- function(country_code, country_name, level_name, 
                           second_crop_name, figures_and_tables_path) {
  
  title_text <- paste("Summary Data on Acid Cropland and Lime Requirements in", country_name)
  subtitle_text <- paste(country_name, "-Summary", sep = "-")
  second_crop <- main_crop$crop
  second_crop_name <- main_crop$crop_type
  
  # Create the RMarkdown content
  rmd_content <- paste0(
    "---\n",
    "title: \"", country_name, " Summary Slide Deck\"\n",
    "output:\n",
    "  xaringan::moon_reader:\n",
    "    self_contained: true\n",
    "---\n\n",
    
    "```{=latex}\n",
    "\\newcommand{\\CountryName}{", country_name, "}\n",
    "\\newcommand{\\BackgroundImagePath}{",  paste0(here::here(),"/reports/eia_technical_notes/figures/slide_cover_eia.png"), "}\n",
    "\\newcommand{\\DashboardImagePath}{",  paste0(here::here(),"/reports/eia_technical_notes/figures/dashboard_2.png"), "}\n",
    "\\input{",  paste0(here::here(),"/reports/eia_technical_notes/eia_presentation_templet.tex"), "}\n",
    "```\n\n",
    
    "# Introduction\n\n",
    "## Outlook\n\n",
    "```{=latex}\n",
    "\\CustomTitlePage\n",
    "\\usebackgroundtemplate{}\n",
    "```\n\n",
    
    "- The slide deck provides a brief summary of **data-driven**, **ex-ante analysis** of soil acidity across **", country_name, "**, evaluating the scale of affected areas, potential crop yield impacts, and the expected benefits of lime application for proactive soil remediation.\n\n",
    "- **Highlights**:\n",
    "  - **Spatial Distribution of Acidic Cropland**: Identifies regions with high acidity levels impacting crop yields.\n",
    "  - **Yield Loss**: Quantifies productivity losses in key crops due to acidic soils.\n",
    "  - **Lime Requirements**: Estimates the lime amounts needed to improve soil health in these areas.\n",
    "  - **Economic Potential**: Highlights estimated increases in production, production value and economic benefit from lime application.\n\n",
    "- Provide policymakers, farmers, and stakeholders with actionable insights to prioritize areas with high potential for productivity gains and economic return from soil remediation.\n\n",
    "---\n\n",
    
    "# Spatial Distribution of Soil Acidity\n\n",
    "## Distribution of exchangeable acidity, as a % of the effective cation exchange capacity (ECEC), across ", country_name, "\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_acidic_cropland_distribution.png")), "){width=99% height=70%}\n\n",
    "---\n\n",
    
    "## Top 10 ", level_name, " in ", country_name, " with the highest acidic cropland\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_plt_acidic_cropland_both.png")), "){width=98% height=80%}\n\n",
    "---\n\n",
    
    "# Total crop area distribution (ha)\n\n",
    "## Total crop area in the top-10 most acidic ", level_name, " (not segregated by acidic cropland)\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_plt_crop_area_acidic.png")), "){width=98% height=85%}\n\n",
    "---\n\n",
    
    "## Total crop area in the top-10 most acidic ", level_name, " (not segregated by acidic cropland (table))\n\n",
    "```{=latex}\n",
    "\\scriptsize\n",
    "\\input{", file.path(figures_and_tables_path, paste0(country_code, "_table_crop_area_acidic.tex")), "}\n",
    "```\n\n",
    "---\n\n",
    
    "# Additional Production\n\n",
    "## Potential for additional production (ton) in top 10 acidic ", level_name, " in ", country_name, "\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_plt_additional_production.png")), "){width=98% height=85%}\n\n",
    "---\n\n",
    
    "## Potential for additional production (ton) in top 10 acidic ", level_name, " in ", country_name, " (Table)\n\n",
    "```{=latex}\n",
    "\\scriptsize\n",
    "\\input{", file.path(figures_and_tables_path, paste0(country_code, "_table_additional_production.tex")), "}\n",
    "```\n\n",
    "---\n\n",
    
    "## Potential for additional production value (US\\$) in top 10 acidic ", level_name, " in ", country_name, "\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_plt_additional_production_value.png")), "){width=98% height=85%}\n\n",
    "---\n\n",
    
    "## Potential for additional production value (1000 US\\$) in top 10 acidic ", level_name, " in ", country_name, " (Table)\n\n",
    "```{=latex}\n",
    "\\scriptsize\n",
    "\\input{", file.path(figures_and_tables_path, paste0(country_code, "_table_additional_production_value.tex")), "}\n",
    "```\n\n",
    "---\n\n",
    
    "# Yield Loss\n\n",
    "## Yield loss for Maize and ", second_crop_name, " (%) in ", country_name, "\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_yield_loss.png")), "){width=95%}\n\n",
    "---\n\n",
    
    "# Yield Response\n\n",
    "## Yield response to lime application in ", country_name, "\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_yield_response.png")), "){width=98%}\n\n",
    "---\n\n",
    
    "# Lime Requirement\n\n",
    "## Lime requirements (t) for acid soil remediation\n\n",
    "### Lime requirements (1000 t) for acid soil remediation in ", country_name, "\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_plt_lime_requirements.png")), "){width=95%}\n\n",
    "---\n\n",
    
    "## Lime requirements (t) for acid soil remediation (table)\n\n",
    "```{=latex}\n",
    "\\scriptsize\n",
    "\\input{", file.path(figures_and_tables_path, paste0(country_code, "_table_lime_requirements.tex")), "}\n",
    "```\n\n",
    "---\n\n",
    
    "## Lime rate for Maize and ", second_crop_name, " in ", country_name, "\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_lime_rate.png")), "){width=98%}\n\n",
    "---\n\n",
    
    "# Economic Benefits\n\n",
    "## Spatial Distribution of Lime Profitability (US\\$/ha) Under Varying Prices and Productivity Levels in ", country_name, ".\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_profitability_sensetivity_amount.png")), "){height=85%}\n\n",
    "---\n\n",
    
    "## Spatial Distribution of Lime Profitability... (% of Profitable Crop Area)\n\n",
    "![](", file.path(figures_and_tables_path, paste0(country_code, "_profitability_sensetivity.png")), "){height=85%}\n\n",
    "---\n\n",
    
    "# Appendix\n\n",
    "```{=latex}\n",
    "\\vspace{3cm}\n",
    "         \\centering\\textcolor{my-green}{\\Huge{\\textbf{Appendix}}}\n",
    "```\n\n",
    
    "## Modeling approach\n\n",
    "![](", file.path(output_path, paste0("modelling.png")), "){width=95%}\n\n",
    "---\n\n",
    
    "## Modeling approach: Three-Step Approach\n\n",
    "- **Lime Requirements Estimation**:\n",
    "  - Estimated using Aramburu Merlos et al. (2022) lime requirement model.\n",
    "  - Calculated lime rates to achieve crop-specific Target Acidity Saturation (TAS).\n",
    "  - Applied to gridded soil data on exchangeable acidity and ECEC from Hengl et al. (2017).\n",
    "  - Converted lime rates to tons of $CaCO{_3}/ha$, assuming a 20 cm soil depth and using soil bulk density data.\n\n",
    "- **Crop Yield Response Estimation**:\n",
    "  - Plateau-linear decay models representing yield loss due to acidity (estimated current yield losses (\\% of maximum yield) due to high acidity saturation).\n\n",
    "- **Economic Benefit Estimation**:\n",
    "  - Return is calculated as the additional yield from liming multiplied by the median crop price.\n",
    "  - Cost is determined by multiplying the estimated lime rate by a fixed lime price.\n\n",
    "---\n\n",
    
    "## Main assumptions and parameters\n\n",
    "- Acidic cropland is defined as having acidity saturation levels above 10\\% of the Effective Cation Exchange Capacity (ECEC).\n",
    "- Crop yields are unaffected by soil acidity until a specific acidity saturation threshold Target Acidity Saturation (TAS) is reached; beyond this threshold, yields decrease linearly, eventually reaching zero at high acidity levels.\n",
    "- The TAS that separates maximum yield from yield decline is used as a critical value for making liming recommendations.\n",
    "- Actual yields in acid tropical soils are a fraction of the potential yields in non-acidic soils, determined by the estimated yield loss due to acidity.\n",
    "- Economic benefits from liming are calculated by multiplying the additional yield after soil remediation by median crop prices from sub-Saharan Africa (2016-2020 FAOSTAT data). The cost of liming is calculated using estimated lime application rates and a fixed lime price of 100 US\\$/ton.\n",
    "- The benefits of liming are assessed only for the year of application due to a lack of robust, large-scale data on long-term effects in sub-Saharan Africa.\n\n",
    "For details, visit \\href{www.acidsoils.africa}{www.acidsoils.africa}.\n\n",
    
    "```{=latex}\n",
    "\\CustomEndPage\n",
    "```\n"
  )
  
  # Write the RMarkdown content to a .Rmd file
  rmd_file_path <- file.path(report_path, paste0(country_code, "_summary_slidedeck.Rmd"))
  writeLines(rmd_content, rmd_file_path)
  cat("RMarkdown slide deck written to:", rmd_file_path, "\n")
  
}
# compile the latex file

#' Compile a LaTeX Slide Deck
#' 

#' This function compiles a LaTeX slide deck file to generate a PDF presentation.

#' @param country_name A character string representing the ISO3 code of the country (e.g., "KEN" for Kenya).
#' @param report_path A character string specifying the directory path where the generated LaTeX file is located.
#' 
#' @return None. The function compiles the LaTeX file to generate a PDF presentation.


compile_latex_slidedeck <- function(country_name, report_path) {
  
  cat("Compiling the LaTeX slide deck for:", country_name, "\n")
  # Compile the LaTeX slide deck to generate a PDF presentation
  latex_file_path <- file.path(report_path, paste0("Technical_Brief_Targeting_Agricultural_Lime_for_Acid_Croplands_in_",country_name,".tex"))
  
  # clear auxiliary files
  
  tinytex::latexmk(file = latex_file_path)
  
  cat("PDF slide deck generated for:", country_name, "\n")
}
