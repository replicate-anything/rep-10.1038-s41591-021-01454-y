# Differences between groups within studies (Summary) — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/studies/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_5.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_tab_5 <- function(data){
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
  
  
  
  country_differences <-
    unique(df$country) %>%
    lapply(function(j){{
      dff <- filter(df, country == j)
      
      lapply(c("gender", "age_groups_three", "educ_binary"), function(i){
        if (length(table(dff[[i]])) < 2)  {
          return(NULL)
        } else {
          m <- estimatr::lm_robust(as.formula(paste("take_vaccine_num ~", i)),
                                   weight = weight,
                                   cluster = cluster,
                                   se_type = "stata",
                                   data = dff)
          m %>%
            broom::tidy() %>%
            dplyr::select(estimate, std.error, p.value, df, term) %>%
            dplyr::mutate(n = m$nobs)
        }}
      ) } %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(country = j)}) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(term, country) %>%
    dplyr::relocate(country, term) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(significant = p.value <= .05) %>%
    dplyr::mutate(
      term = ifelse(term == "age_groups_three25-54", "25-54", term),
      term = ifelse(term == "age_groups_three55+", "55+", term),
      term = ifelse(term == "educ_binaryUp to Secondary", "Up to secondary", term),
      term = ifelse(term == "genderMale", "Male", term))
  
  
  country_differences_summary <- 
    country_differences %>% 
    dplyr::filter(!(country %in% c("Russia", "USA"))) %>% 
    dplyr::group_by(term) %>% summarize(
      "positive " = sum(estimate > 0),
      "positive and significant" = sum(estimate > 0 & significant),
      "negative and significant" = sum(estimate < 0 & significant),
      "not significant" = sum(!significant),
      n = n()) 
  
  
  tab <- knitr::kable(country_differences_summary,
                      digits = 2, 
                      format = "html", 
                      caption = "Differences between groups within studies (Summary)")
  
  as.character(tab)
  
}


make_tab_5(utils::read.csv("../data/combined.csv", stringsAsFactors = FALSE))
