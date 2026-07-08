# Summary Stats — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/studies/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_11.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_tab_11 <- function(data){

  # Ensure cluster ids are distinct across studies
  df <- 
    data[[1]] %>% 
    dplyr::group_by(study) %>% 
    dplyr::mutate(
      cluster = ifelse(is.na(cluster), paste(1:n()), cluster),
      cluster = paste0(gsub(" ", "_", tolower(country)), "_", cluster))
  
  # Weights sum to 1 in each study and recode age and education into bins
  df <- 
    df %>% 
    dplyr::group_by(study) %>% 
    dplyr::mutate(
      weight_replace = mean(weight, rm.na = TRUE),
      weight = if_else(is.na(weight), 
                       if_else(is.na(weight_replace), 1, weight_replace), 
                       weight),
      weight = weight/sum(weight)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      age_groups = 
        as.character(cut(x = age, breaks = c(-Inf, 18, 30, 45, 60, +Inf), right = F)),
      age_groups_binary = ifelse(age >= 55, "55+", NA),
      age_groups_binary = ifelse(age < 55, "<55", age_groups_binary),
      age_less24 = ifelse(age <= 24, 1, 0),
      age_25_54 = ifelse(age >= 25 & age <= 54, 1, 0),
      age_55_more = ifelse(age >= 55, 1, 0),
      age_groups_three = ifelse(age <= 24, "<25", NA),
      age_groups_three = ifelse(age >= 25 & age <= 54, "25-54", age_groups_three),
      age_groups_three = ifelse(age >= 55, "55+", age_groups_three),
      educ_binary = if_else(educ == "More than secondary", "> Secondary", "Up to Secondary")) 
  
  
  # We create a new dataframe with countries and with "All" (only LMICs). Countries are clusters in "All" analysis
  # USA and Russia excluded from "All" set
  
  df2 <- 
    dplyr::bind_rows(
      mutate(df, group = country),
      mutate(filter(df, country != "USA" & country != "Russia"), group = "All")) %>% 
    mutate(
      cluster = if_else(group == "All", 
                        gsub(pattern = " ", replacement = "_", x = tolower(country)), 
                        cluster)) 
  
  # transform the categorical variables into dummy variables
  df2 <- 
    df2 |>
    fastDummies::dummy_cols(select_columns = c("age_groups","educ_binary","gender"))
  
  df2 <-
    df2 |> 
      dplyr::mutate(
        trust_recode_1 = ifelse(trust_vaccine_1 == 1 | trust_vaccine_2 == 1, 1, 0),
        trust_recode_1 = ifelse((country == "Nigeria" | country == "USA") &
                                  is.na(trust_recode_1), 0, trust_recode_1),
        trust_recode_2 = ifelse(trust_vaccine_8 == 1 | trust_vaccine_9 == 1, 1, 0),
        trust_recode_2 = ifelse((country == "Sierra Leone 2") & 
                                  is.na(trust_recode_2), 0, trust_recode_2),
        trust_recode_3 = ifelse(trust_vaccine_3 == 1 | 
                                  trust_vaccine_7 == 1 | 
                                  trust_vaccine_4 == 1, 1, 0),
        trust_recode_3 = 
          ifelse((country == "Nigeria" | country == "USA" | country == "Russia") & 
                   is.na(trust_recode_3), 0, trust_recode_3),
        trust_recode_4 = ifelse(trust_vaccine_666 == 1 | trust_vaccine_other == 1, 1, 0),
        trust_recode_4 = 
          ifelse((country == "Burkina Faso" | 
                    country == "Sierra Leone 2" | 
                    country == "Russia") & 
                   is.na(trust_recode_4), 0, trust_recode_4),
        trust_recode_5 = ifelse(trust_vaccine_dk == 1 | 
                                  trust_vaccine_refuse == 1 | 
                                  trust_vaccine_nr == 1, 1, 0),
        trust_recode_5 = 
          ifelse((country == "Nigeria" | country == "Sierra Leone 2" | country == "USA") &
                   is.na(trust_recode_5), 0, trust_recode_5))
    get_stat <- function(.var, .data, ...) {
      return(
        paste0("`", .var, "` ~ 1") %>% 
          as.formula() %>% 
          estimatr::lm_robust(formula = ., data = .data, ...) %>% 
          coef()
      )
    }
  
  data_sumstat <- 
    df2 %>% 
    dplyr::nest_by(group) %>% 
    dplyr::mutate(
      Female = get_stat("gender_Female", .data = data),
      age_18_30 = get_stat("age_groups_[18,30)", data),
      age_30_45 = get_stat("age_groups_[30,45)", data),
      age_45_60 = get_stat("age_groups_[45,60)", data),
      age_60    = get_stat("age_groups_[60, Inf)", data),
      Less_than_secondary = get_stat("educ_binary_Up to Secondary", data),
      More_than_secondary = get_stat("educ_binary_> Secondary", data)
    ) %>% 
    dplyr::select(-data, country = group) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(across(where(is.double), ~ . * 100)) 
  
  wgmdata <- 
    data[[2]] %>%
    dplyr::filter(WP5 %in% c(1,9,31,35,41,63,65,76,78,80,105,157),
                  Age >= 18) %>% 
    dplyr::mutate(
      country = 
        plyr::mapvalues(WP5, 
                        from = c(1, 9, 31, 35, 41, 63, 65, 76, 78, 80, 105, 157),
                        to = c("USA","Pakistan","India","Nigeria","Uganda",
                               "Mozambique","Rwanda","Russia", 
                               "Burkina Faso", "Sierra Leone","Colombia","Nepal")),
      age_groups = cut(x = Age, breaks = c(-Inf, 18, 30, 45, 60, +Inf), right = F)) %>% 
    dplyr::select(country, wgt, gender = Gender, age = Age, educ = Education,
                  age_groups) %>%
    fastDummies::dummy_cols(select_columns = "age_groups") %>% 
    dplyr::mutate(
      gender_Female = if_else(gender == 2, 1, 0), 
      `educ_binary_Up to Secondary` = if_else(educ == 1 | educ == 2, 1, 0),
      `educ_binary_> Secondary` = if_else(educ == 3, 1, 0))
  
  wgmdata_sumstat <- 
    wgmdata %>% 
    dplyr::nest_by(country) %>% 
    dplyr::mutate(
      Female = get_stat("gender_Female", data, weight = wgt),
      age_18_30 = get_stat("age_groups_[18,30)", data, weight = wgt),
      age_30_45 = get_stat("age_groups_[30,45)", data, weight = wgt),
      age_45_60 = get_stat("age_groups_[45,60)", data, weight = wgt),
      age_60    = get_stat("age_groups_[60, Inf)", data, weight = wgt),
      Less_than_secondary = get_stat("educ_binary_Up to Secondary", data, weight = wgt),
      More_than_secondary = get_stat("educ_binary_> Secondary", data, weight = wgt)
    ) %>% 
    dplyr::select(-data) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(across(where(is.double), ~ . * 100)) 
  
  wgmdata_sumstat_label <-
    wgmdata_sumstat %>% 
    dplyr::mutate(country = paste0(country, " (WGM)"))
  
  sum_stat_row <- 
    dplyr::bind_rows(data_sumstat, wgmdata_sumstat_label) %>%
    arrange(country) %>% 
    .[c(1:13,15,16,14,19,20,22,23,21,25,26,24,17,18,27,28),]

  tab <- knitr::kable(
    sum_stat_row,
    caption = "Summary statistics for gender, age, education",
    col.names = c("Study", "% Women",
                  "% Age in [18,30)", "% Age in [30,45)", "% Age in [45,60)", "% Age 60+",
                  "% Up to Secondary", "% More than Secondary"),
    booktabs = T, linesep = "", align = c("l", rep("c", 7)), digits = 1) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::footnote(
      general = "This table presents summarys statistics for our data and compares it with estimates from other sources of data. Data for Russia comes from census data from the Statistical Agency. For the USA, we use data from the 2019 American Community Survey. For all other countries, the Wellcome Global Monitor 2018 was used. Statistics for our surveys are not weighted, while estimates from benchmark sources are obtained using sampling weights.") %>%
    kableExtra::column_spec(1:8, width = "5em")
  
  as.character(tab)
}

make_tab_11(list(
  utils::read.csv("../data/combined.csv", stringsAsFactors = FALSE),
  utils::read.csv("../data/wgm_2018_publiccsv.csv", stringsAsFactors = FALSE)
))
