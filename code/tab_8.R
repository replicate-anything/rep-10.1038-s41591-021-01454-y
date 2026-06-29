# Reason to take the vaccine: by age. — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_8.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_tab_8 <- function(data){
  
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
  
  lm_helper <- function(data, ...) {
    fit <- estimatr::lm_robust(data = data, ...)
    out <- dplyr::bind_cols(broom::tidy(fit), n = nobs(fit))
    return(out)
  }

  age_analysis <- function(df,
                           reason,
                           num = "Yes",
                           filter_by = NA) {
    df %>%
      dplyr::filter(.data[[filter_by]] == 1) %>%
      dplyr::filter(take_vaccine %in% num, 
                    if_all(c(all_of(reason), cluster, weight), ~ !is.na(.))) %>%
      dplyr::nest_by(group) %>%
      dplyr::summarize(
        lm_helper(data = data, 
                  formula = as.formula(paste0(reason, "~ 1")), 
                  cluster = cluster,
                  weight = weight, se_type = "stata"), .groups = "drop")
  }

  yes_vars <-
    df2 %>%
    dplyr::select(yes_vaccine_1, yes_vaccine_2, yes_vaccine_3) %>%
    names

  ## Generate data for analysis of yes reasons for different age groups
  yes_vacc_age_1 <-
    lapply(yes_vars, function(v) age_analysis(df = df2, reason = v, num = "Yes", filter_by = "age_less24")) %>%
    dplyr::bind_rows() %>%
    mutate(age = "<25")

  yes_vacc_age_2 <-
    lapply(yes_vars, function(v) age_analysis(df = df2, reason = v, num = "Yes", filter_by = "age_25_54")) %>%
    dplyr::bind_rows() %>%
    mutate(age = "25-54")

  yes_vacc_age_3 <-
    lapply(yes_vars, function(v) age_analysis(df = df2, reason = v, num = "Yes", filter_by = "age_55_more")) %>%
    dplyr::bind_rows() %>%
    mutate(age = "55+")

  #Get percentage per yes reason category and make wide table
  yes_vacc_age <-
    rbind(yes_vacc_age_1, yes_vacc_age_2, yes_vacc_age_3) %>%
    dplyr::mutate(across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 0))) %>%
    dplyr::mutate(estimate = format(estimate, nsmall = 0),
                  conf_int = paste0("(", conf.low,
                                    ", ", conf.high, ")"),
                  n = as.character(n)) %>%
    dplyr::select(group, estimate, conf_int, outcome, age, n) %>%
    tidyr::pivot_wider(names_from = c(outcome, age), values_from = c(estimate, conf_int, n), names_sep = "__") %>%
    tidyr::pivot_longer(cols = c(starts_with("estimate__"), starts_with("conf_int__"), starts_with("n__")),
                        names_to = c("type", ".value"),
                        names_pattern = "(.*)__(.*)")

  y1 <- yes_vacc_age %>%
    dplyr::filter(grepl("yes_vaccine_1", type)) %>%
    dplyr::mutate(type = ifelse(grepl("estimate", type), "estimate", type),
                  type = ifelse(grepl("conf_int", type), "conf_int", type),
                  type = ifelse(grepl("n__", type), "n", type))


  y2 <- yes_vacc_age %>%
    dplyr::filter(grepl("yes_vaccine_2", type)) %>%
    dplyr::mutate(type = ifelse(grepl("estimate", type), "estimate", type),
                  type = ifelse(grepl("conf_int", type), "conf_int", type),
                  type = ifelse(grepl("n__", type), "n", type))

  y3 <- yes_vacc_age %>%
    dplyr::filter(grepl("yes_vaccine_3", type)) %>%
    dplyr::mutate(type = ifelse(grepl("estimate", type), "estimate", type),
                  type = ifelse(grepl("conf_int", type), "conf_int", type),
                  type = ifelse(grepl("n__", type), "n", type))

  yes_vacc_age <-
    dplyr::left_join(y1, y2, by = c("group", "type")) %>%
    dplyr::left_join(y3, by = c("group", "type")) %>%
    dplyr::mutate(
      group = forcats::fct_relevel(as.factor(group), "All", "Russia", "USA", after = Inf)) %>%
    dplyr::arrange(group) %>%
    dplyr::mutate(
      across(c(group),
             ~ifelse(type == "conf_int", "Conf. interval", as.character(.))),
      across(c(group), ~ifelse(type == "n", "n", as.character(.))),
      group = ifelse(group == "All", "All LMICs", group)) %>%
    dplyr::select(-type)

  cnames <- c("Study", "<25", "25-54", "55+", "<25", "25-54", "55+", "<25", "25-54", "55+")

  tab <- 
    yes_vacc_age %>%
    knitr::kable(
      col.names = cnames,
      caption = "\\label{yes1}Reasons to take the vaccine",
      booktabs = T, linesep = "",
      format = "html",
      format.args = list(big.mark = ",", scientific = FALSE),
      align = c("l", rep("c", 9))) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::add_header_above(c(" " = 1, "Self" = 3, "Family" = 3, "Community" = 3),
                                 bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, width = "8em") %>%
    kableExtra::column_spec(2:10, width = "4em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S4 shows percentage of respondents mentioning reasons why they would take the Covid-19 vaccine by age groups. The number of observations and percentage correponds only to people who would take the vaccine. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses.",
      threeparttable = T)
  
  as.character(tab)


}


make_tab_8(utils::read.csv("../data/combined.csv", stringsAsFactors = FALSE))
