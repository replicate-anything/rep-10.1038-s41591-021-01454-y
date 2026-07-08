# Vaccine Data from WGM, WHO — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/studies/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_1.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_tab_1 <- function(data){
  # Call data from WGM
  dfwgm <- data[[1]] #"3_rep_data/table_wgm.csv"
  
  # Call data from WHO
  df_vacc_coveragebis <-  data[[2]] #"3_rep_data/vacc_cov.csv"
  
  # Put together and order labels
  
  table_1b <- dfwgm |>
    left_join(df_vacc_coveragebis) |>
    mutate(country = as.factor(country),
           country = forcats::fct_relevel(country, "Russia", "USA", after = Inf)) |> 
    arrange(country) |>
    select(country, Effectiveness, Safety, Important, BCG, DTP1, MCV1, Coverage)
  
  tab <- 
    knitr::kable(
      table_1b,
      caption =  "Vaccination beliefs and coverage for the countries in our sample",
      col.names = c("",
                    "Effective","Safe","Important for children to have",
                    "Tuberculosis (BCG)", "Diphtheria, Tetanus and Pertussis (DTP1)",
                    "Measles (MCV1)",
                    "% of parents with any child that was ever vaccinated"),
      format = "html", booktabs = T, linesep = "", align = c("l", rep("c", 7)), label = "otherv") |> 
    kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"), 
                              full_width = FALSE, font_size = 10)  |>
    kableExtra::row_spec(0, bold = TRUE) |> 
    kableExtra::add_header_above(c(" " = 1, 
                                   "% Respondents agreeing Vaccines are..." = 3,
                                   "Vaccine coverage in 2019 (% of infants)" = 3,
                                   " " = 1), bold = TRUE) |>
    kableExtra::column_spec(1:5, width = "9em") |> 
    kableExtra::column_spec(6:7, width = "5em") |>
    kableExtra::column_spec(8, width = "9em") |> 
    kableExtra::footnote(
      general_title = "",
      general = "Table 2 presents an overview of vaccination beliefs and incidence 
      across countries in our sample. Columns 2-4 and 8 use data from the 
      Wellcome Global Monitor 2018. Column 8 shows the percentage of respondents 
      who are parents and report having had any of their children ever vaccinated. 
      Columns 2-4 show the percentage of all respondents that either strongly 
      agree or somewhat agree with the statement above each column. All 
      percentages are obtained using national weights. Columns 5-7 use data from
      the World Health Organization on vaccine incidence. Columns 5-7 report the
      percentage of infants per country receiving the vaccine indicated in each column.", 
      threeparttable = T) |>
    kableExtra::landscape()
  
  as.character(tab)
}

make_tab_1(list(
  utils::read.csv("../data/table_wgm.csv", stringsAsFactors = FALSE),
  utils::read.csv("../data/vacc_cov.csv", stringsAsFactors = FALSE)
))
