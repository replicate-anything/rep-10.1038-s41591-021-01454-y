# Reason to take the vaccine. All categories. — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_7.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_tab_7 <- function(data){
  
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
  #There are idiosyncratic reasons why people would take the vaccine. I recoded them. But we keep only the core, which is common almost in all studies.
  yes_vars <-
    df2 %>%
    dplyr::select(yes_vaccine_1, yes_vaccine_2, yes_vaccine_3) %>%
    names
  
  ## Generate data for analysis of yes reasons
  yes_vacc1 <-
   lapply(yes_vars, function(v) reasons_together(data = df2, reason = v, num = "Yes")) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 0)))
  
  # All LMIcs estimate of self protection
  yes_all <- 
    yes_vacc1 %>% 
    dplyr::filter(group == "All" & outcome == "yes_vaccine_1")
  
  # Top and bottom LMICs countries for category self protection
  yes_ <- 
    yes_vacc1 %>% 
    dplyr::filter(group != "All" & group != "USA" & group != "Russia" & outcome == "yes_vaccine_1") 
  
  yes_top <- yes_ %>% dplyr::arrange(desc(estimate))
  yes_bottom <- yes_ %>% dplyr::arrange(estimate)
  
  # All LMICs estimate of protection of family
  yes_all_2 <- 
    yes_vacc1 %>% 
    dplyr::filter(group == "All" & outcome == "yes_vaccine_2")
  
  # Top and bottom LMICs countries for category protection of family
  yes_2 <- 
    yes_vacc1 %>% 
    dplyr::filter(group != "All" & group != "USA" & group != "Russia" & outcome == "yes_vaccine_2") 
  
  yes2_top <- yes_2 %>% dplyr::arrange(desc(estimate))
  yes2_bottom <- yes_2 %>% dplyr::arrange(estimate)
  
  # Estimate of self protection for Russia and the US
  yes_usa <- 
    yes_vacc1 %>% 
    dplyr::filter(group != "All" & group == "USA" & outcome == "yes_vaccine_1") 
  
  yes_rus <- 
    yes_vacc1 %>% 
    dplyr::filter(group != "All" & group == "Russia" & outcome == "yes_vaccine_1") 
  
  # Estimate of protection of family for Russia and the US
  yes2_usa <- 
    yes_vacc1 %>% 
    dplyr::filter(group != "All" & group == "USA" & outcome == "yes_vaccine_2")
  
  yes2_rus <- 
    yes_vacc1 %>% 
    dplyr::filter(group != "All" & group == "Russia" & outcome == "yes_vaccine_2") 
  
  
  
  yes_vars <- 
    df2 %>% 
    dplyr::select(yes_vaccine_1, yes_vaccine_2, 
                  yes_vaccine_3, yes_vaccine_4, 
                  yes_vaccine_5, yes_vaccine_666) %>% 
    names
  
  ## Generate data for analysis of yes reasons
  yes_vacc1 <- 
    lapply(yes_vars, function(v) reasons_together(data = df2, reason = v, num = "Yes")) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 0)))
  
  
  #Get percentage per yes reason category and make wide table
  yes_vacc2 <- 
    yes_vacc1 %>%
    dplyr::mutate(estimate = format(estimate, nsmall = 0),
                  conf_int = paste0("(", conf.low, 
                                    ", ", conf.high, ")")) %>%
    dplyr::select(group, estimate, conf_int, outcome, n) %>%
    tidyr::pivot_wider(names_from = outcome, 
                       values_from = c(estimate, conf_int, n), 
                       names_sep = "__") %>%
    tidyr::pivot_longer(cols = c(starts_with("estimate__"), starts_with("conf_int__")),
                        names_to = c("type", ".value"),
                        names_pattern = "(.*)__(.*)") %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(
      n = ifelse(group == "All", NA, unique(na.omit(c_across(starts_with("n__")))))) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      group = forcats::fct_relevel(as.factor(group), "All", "Russia", "USA", after = Inf)) %>% 
    dplyr::arrange(group) %>% 
    dplyr::mutate(across(c(group, n), ~ifelse(type == "conf_int", "", as.character(.))),
                  group = ifelse(group == "All", "All LMICs", group)) %>% 
    dplyr::select(group, n, type, starts_with("yes_vaccine_"), -starts_with("n_yes_vaccine"), -type)
  
  cnames <- c("Study", "N", "Self", "Family", 
              "Community", "Health workers", "Government", "Other")

  tab <- yes_vacc2 %>%
    knitr::kable(col.names = cnames,
                 caption = "Reasons to take the vaccine. All categories", 
                 booktabs = T, linesep = "", align = c("l", rep("c", 7)),
                 format.args = list(big.mark = ",", scientific = FALSE)) %>% 
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>% 
    kableExtra::add_header_above(c(" " = 2, "Protection" = 3, "If recommended by" = 2, " " = 1), 
                                 bold = TRUE) %>% 
    kableExtra::column_spec(1:8, width = "7em") %>% 
    kableExtra::footnote(
      general_title = "",
      general = "Table S3 shows percentage of respondents mentioning reasons why they would take the Covid-19 vaccine. The number of observations and percentage correponds only to people who would take the vaccine. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses.",
      threeparttable = T) 
  
  as.character(tab)
}


make_tab_7(utils::read.csv("../data/combined.csv", stringsAsFactors = FALSE))
