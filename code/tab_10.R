# Vaccination Decision-making: most trusted source. — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/studies/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_10.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_tab_10 <- function(data){
  
  
  
  # define helper functions
  study_weighting <- function(data){ 
    data = data |> 
      dplyr::group_by(country) |> 
      dplyr::mutate(weight = weight/sum(weight)) |> 
      dplyr::ungroup() 
    
    return(data)
  }
  
  lm_helper <- function(data, ...) {
    data <- study_weighting(data)
    fit  <- estimatr::lm_robust(data = data, ...)
    out  <- dplyr::bind_cols(broom::tidy(fit), n = nobs(fit))
    return(out)
  }

  reasons_together <- function(data, 
                               reason, 
                               num = "Yes") {
    data <- data |>
      dplyr::filter(take_vaccine %in% num, 
                    if_all(c(all_of(reason), cluster, weight), ~ !is.na(.))) |>
      dplyr::nest_by(group) |>
      dplyr::summarize(
        lm_helper(data = data, 
                  formula = as.formula(paste0(reason, "~ 1")), 
                  cluster = cluster,
                  weight = weight, se_type = "stata"), .groups = "drop")
    
    return(data)
  }
  
  # Ensure cluster ids are distinct across studies
  df <- 
    data %>% 
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
  
  trust_names <- c("trust_recode_1", 
                   "trust_recode_2", 
                   "trust_recode_3", 
                   "trust_recode_4", 
                   "trust_recode_5", 
                   "trust_vaccine_5", 
                   "trust_vaccine_6")
  
  trust_vacc <- 
    plyr::ldply(
      .data = list("Yes", "No", "All"), 
      .fun = function(take_vac) {
        list(Yes = "Yes", 
             No = c("No", "DK"), 
             All = c("Yes", "No", "DK")) %>% 
          .[[take_vac]] %>% 
          plyr::ldply(trust_names, reasons_together, data = df2, num = .) %>%
          dplyr::mutate(
            across(c(conf.low, conf.high, estimate), 
                   ~ format(round(. * 100, digits = 1), nsmall = 1)),        
            conf_int = paste0("(", conf.low, ", ", conf.high, ")")) %>%
          dplyr::select(group, estimate, conf_int, outcome, n) %>%
          tidyr::pivot_wider(names_from = outcome, values_from = c(estimate, conf_int, n), 
                             names_sep = "__") %>%
          tidyr::pivot_longer(cols = c(starts_with("estimate__"), starts_with("conf_int__")),
                              names_to = c("type", ".value"),
                              names_pattern = "(.*)__(.*)") %>%
          dplyr::rowwise() %>% 
          dplyr::mutate(
            n = ifelse(group == "All", NA, unique(na.omit(c_across(starts_with("n__")))))) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate("Take vaccine?" = take_vac) %>%
          dplyr::select(group, n, type, "Take vaccine?", starts_with("trust_")) %>%
          dplyr::filter(!(group %in% c("Mozambique", "Pakistan 1", 
                                       "Pakistan 2", "Uganda 1", "India")))
      }) %>% 
    dplyr::mutate(
      group = as.factor(group),
      group = forcats::fct_relevel(group, "All", "Russia", "USA", after = Inf)) %>% 
    dplyr::arrange(group) %>% 
    dplyr::mutate(across(c(group, n, `Take vaccine?`), ~ifelse(type == "conf_int", "", as.character(.))),
                  group = ifelse(group == "All", "All LMICs", group)) %>% 
    dplyr::select(-type)



  tab <- trust_vacc %>%
    dplyr::select("group", "n",
                  "Take vaccine?", "trust_vaccine_5",
                  "trust_vaccine_6", "trust_recode_1",
                  "trust_recode_3", "trust_recode_2",
                  "trust_recode_4", "trust_recode_5") %>%
    knitr::kable(
      col.names = c("Study", "N", "Take vaccine?", "Health workers",
                    "Government or \n Ministry of Health",
                    "Family or friends",
                    "Famous person, \n religious leader or \n traditional healers",
                    "Newspapers, radio \n or online groups", "Other",
                    "Don't know or Refuse"),
      
      caption = "COVID-19 Vaccination Decision-making: most trusted source",
      format.args = list(big.mark = ",", scientific = FALSE),
      align = c("l", rep("c", 9))) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, width = "7em") %>%
    kableExtra::column_spec(2:10, width = "4em") %>%
    kableExtra::column_spec(4:10, width = "6em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S6 shows percentage of respondents that mention actors who they would trust the most to help them decide whether to get a COVID-19 vaccine. For all countries the questions was asked regardless if respondent would take a vaccine, would not take it, does not know or does not respond. For India respondents were able to mention more than one actor, for the rest of countries only one actor was allowed. While rows should sum to 100%, rounding makes number slightly above or below. A 95% confidence interval is shown between parentheses.",
      threeparttable = T)
  
  as.character(tab)

}


make_tab_10(utils::read.csv("../data/combined.csv", stringsAsFactors = FALSE))
