# Summary of Studies' Sampling — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/studies/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_2.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_tab_2 <- function(data){

  tab_sampling <-
    data |> 
    dplyr::select("Study" = "country", "Date" = "date",
                  "Geographic Scope" = "Geographic.scope",
                  "Sampling Methodology" = "Sampling.methodology",
                  "Survey Modality" = "Survey.modality",
                  "Weights" = "Weights")  |> 
    knitr::kable(
      caption =  "Summary of studies sampling",
      format = "html", booktabs = T, linesep = "", label = "sampling") |>
    kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"),
                              font_size = 10) |>
    kableExtra::row_spec(0, bold = TRUE) |>
    kableExtra::column_spec(1:2, width = "8em") |>
    kableExtra::column_spec(3, width = "12em")  |>
    kableExtra::column_spec(4, width = "30em") |>
    kableExtra::landscape()

  as.character(tab_sampling)

}

make_tab_2(utils::read.csv("../data/studies_info_sample.csv", stringsAsFactors = FALSE))
