# Differences in Means — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_3.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_tab_3 <- function(data){
  
    study_weighting <- function(data)
      data %>% 
      dplyr::group_by(country) %>% 
      dplyr::mutate(weight = weight/sum(weight)) %>% 
      dplyr::ungroup() 
    
    # If no cluster information given for a study then individuals are clusters 
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

  # Analysis of differences in means only LMICs
  # Notice that Uganda 1 is dropped, because it does not have reference categories for gender or age
  # Notice that we are using df (and not df2) as data, since it does not include "All"


  # Population estimate (clustering on country)

  differences_means_gen_age <-
    lapply(c("gender", "age_groups_binary", "age_groups_three"), function(i) {
      df %>%
        dplyr::filter(country != "USA" & country != "Russia" & country != "Uganda 1") %>%
        dplyr::filter(if_all(c(all_of(i), all_of("take_vaccine_num")), ~ !is.na(.))) %>%
        study_weighting() %>%
        estimatr::lm_robust(as.formula(paste("take_vaccine_num ~", i)),
                            fixed_effects = ~country,
                            weight = weight,
                            cluster = country,
                            se_type = "stata",
                            data = .) %>%
        broom::tidy() %>%
        dplyr::select(estimate, std.error, p.value, df, term)
    }) %>%
    dplyr::bind_rows(.)

  differences_means_educ <-
    df %>%
    dplyr::filter(country != "USA" & country != "Russia") %>%
    dplyr::filter(if_all(c(all_of("educ_binary"), all_of("take_vaccine_num")), ~ !is.na(.))) %>%
    study_weighting() %>%
    estimatr::lm_robust(
      take_vaccine_num ~educ_binary,
      fixed_effects = ~country,
      weight = weight,
      cluster = country,
      se_type = "stata",
      data = .) %>%
    broom::tidy() %>%
    dplyr::select(estimate, std.error, p.value, df, term)

  diffmeans <-
    rbind(differences_means_gen_age, differences_means_educ) %>%
    dplyr::rename(Estimate = estimate,
                  Std.error = std.error,
                  `P-value` = p.value,
                  "Degrees of freedom" = df,
                  "Baseline category" = term) %>%
    dplyr::mutate(
      Variable = ifelse(`Baseline category` == "genderMale",
                        "Gender (Female)", ""),
      Variable = ifelse(`Baseline category` == "age_groups_binary55+",
                        "Age", Variable),
      Variable = ifelse(`Baseline category` == "educ_binaryUp to Secondary",
                        "Education (Secondary +)", Variable),
      `Baseline category` = ifelse(`Baseline category` == "genderMale",
                                   "Male", `Baseline category`),
      `Baseline category` = ifelse(`Baseline category` == "age_groups_binary55+",
                                   "55+", `Baseline category`),
      `Baseline category` = ifelse(`Baseline category` == "educ_binaryUp to Secondary",
                                   "Up to secondary", `Baseline category`))


  diffmeans <-
    diffmeans %>%
    filter(Variable != "Age") %>%
    dplyr::mutate(
      Variable = ifelse(`Baseline category` == "age_groups_three25-54",
                        "Age (25-54)", Variable),
      Variable = ifelse(`Baseline category` == "age_groups_three55+",
                        "Age (55+)", Variable),
      `Baseline category` = ifelse(`Baseline category` == "age_groups_three25-54",
                                   "<25", `Baseline category`),
      `Baseline category` = ifelse(`Baseline category` == "age_groups_three55+",
                                   "<25", `Baseline category`))

  dmeans <- diffmeans %>%
    filter(Variable != "") %>%
    knitr::kable(
      digits = 2,
      caption =  "Differences in means",
      format = "html", booktabs = T, linesep = "", label = "dmeans", align = "c") %>%
    kableExtra::kable_styling(latex_options = c("hold_position"),
                              font_size = 10, full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S7 shows the results of subgroup mean differences. Subgroup differences were generated considering only LMICs. p-values come from a two-sided t-test from a linear regression.",
      threeparttable = T)

  
  as.character(dmeans)

}

make_tab_3(utils::read.csv("../data/combined.csv", stringsAsFactors = FALSE))
