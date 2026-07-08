# Trust — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/studies/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript fig_4.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)

make_fig_4 <- function(data){
  
  
  # define functions
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

  data <- 
    data |> 
    dplyr::group_by(study) |> 
    dplyr::mutate(
      cluster = ifelse(is.na(cluster), paste(1:dplyr::n()), cluster),
      cluster = paste0(gsub(" ", "_", tolower(country)), "_", cluster))

  data <- 
    data |> 
    dplyr::group_by(study) |> 
    dplyr::mutate(
      weight_replace = mean(weight, rm.na = TRUE),
      weight = if_else(is.na(weight), 
                       if_else(is.na(weight_replace), 1, weight_replace), 
                       weight),
      weight = weight/sum(weight)) |> 
    dplyr::ungroup() |>
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
  
  data2 <- 
    dplyr::bind_rows(
      mutate(data, group = country),
      mutate(filter(data, country != "USA" & country != "Russia"), group = "All")) |> 
    mutate(
      cluster = if_else(group == "All", 
                        gsub(pattern = " ", replacement = "_", x = tolower(country)), 
                        cluster)) 
  
  
  #Group together categories
    
  data2 <- data2 |> 
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
  
  #Recoded groups
  trust_names <- c("trust_recode_1", "trust_recode_2", "trust_recode_3", 
                   "trust_recode_4", "trust_recode_5", "trust_vaccine_5", "trust_vaccine_6")
  
  studies_levels <- 
    c("Burkina Faso", "Colombia", "India", "Mozambique",
      "Nepal", "Nigeria", "Pakistan 1", "Rwanda",
      "Sierra Leone 1", "Sierra Leone 2", "Uganda 2",
      "All", "Russia", "USA" )
  
  dictionary <- data.frame(
    outcome = c(
      "study", "country", "take_vaccine", "take_vaccine_num",
      "age", "age_groups", "age_groups_binary", "educ", "educ_binary",
      "gender", "cluster", "weight",
      "yes_vaccine_1", "yes_vaccine_2", "yes_vaccine_3", "yes_vaccine_4",
      "yes_vaccine_5", "yes_vaccine_666",
      "no_vaccine_1", "no_vaccine_2", "no_vaccine_3", "no_vaccine_4",
      "no_vaccine_5", "no_vaccine_6", "no_vaccine_7", "no_vaccine_8",
      "no_vaccine_9", "no_vaccine_666",
      "trust_vaccine_1", "trust_vaccine_2", "trust_vaccine_3", "trust_vaccine_4",
      "trust_vaccine_5", "trust_vaccine_6", "trust_vaccine_7", "trust_vaccine_8",
      "trust_vaccine_9", "trust_vaccine_dk", "trust_vaccine_refuse",
      "trust_vaccine_nr", "trust_vaccine_666", "trust_vaccine_other",
      "trust_recode_1", "trust_recode_2", "trust_recode_3",
      "trust_recode_4", "trust_recode_5"
    ),
    tag = c(
      "Study code", "Study name", "Respondent would take the vaccine if available?",
      "Respondent would take the vaccine if available? Yes = 1",
      "Age", "Age grouped", "Age recoded", "Education", "Education recoded",
      "Male", "Survey clusters", "Survey weights",
      "Protection: self", "Protection: family", "Protection: community",
      "If recommended by: Health workers", "If recommended by: Government", "Other",
      "Concerned about side effects", "Concerned about getting coronavirus from the vaccine",
      "Not concerned about getting seriously ill", "Doesn't think vaccines are effective",
      "Doesn't think Coronavirus outbreak is as serious as people say",
      "Doesn't like needles", "Allergic to vaccines", "Won't have time to get vaccinated",
      "Mentions a conspiracy theory", "Other reasons",
      "Family", "Friends", "Religious leader", "Famous person",
      "Health workers", "Government or MoH", "Traditional healers",
      "Media", "Online medical groups", "Don't know", "Refuse",
      "No response", "Other (specify)", "Other (category)",
      "Family or Friends", "Newspapers, radio or online groups",
      "Famous person, religious leader or traditional healers",
      "Other", "Don't know or Refuse"
    ),
    stringsAsFactors = FALSE
  )
  
  #Get estimates
  trust_vacc_together <-
    list(
      All = lapply(trust_names, reasons_together, 
                   data = data2, num = c("Yes", "No", "DK")) |> 
        dplyr::bind_rows(),
      Yes = lapply(trust_names, reasons_together, 
                   data = data2, num = c("Yes")) |> 
        dplyr::bind_rows(),
      No = lapply(trust_names, reasons_together, 
                  data = data2, num = c("No", "DK")) |> 
        dplyr::bind_rows()) |> 
    dplyr::bind_rows(.id = "sub") |>
    dplyr::filter(!is.nan(statistic)) |>
    dplyr::mutate(
      across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1)),
      n_sub = round(n * estimate, 0),
      n_sub = ifelse(n_sub == 0, NA_integer_, n_sub),
      group = factor(group, levels = studies_levels)) |>
    dplyr::left_join(dictionary, by = "outcome") |>
    dplyr::mutate(
      size = cut(n_sub, c(0, 50, 500, Inf), include.lowest = TRUE),
      size = forcats::fct_recode(size, "500+" = "(500,Inf]"),
      tag = as.factor(tag),
      tag = forcats::fct_relevel(tag, 
                                 "Health workers", 
                                 "Government or MoH", 
                                 "Family or Friends", 
                                 "Famous person, religious leader or traditional healers", 
                                 "Newspapers, radio or online groups", 
                                 "Other", 
                                 "Don't know or Refuse"),
      sub = forcats::fct_relevel(as.factor(sub), "No", "Yes", "All"),
      sub = plyr::mapvalues(sub, from = c("No", "Yes", "All"),
                            to = c("No, Don't know", "Yes", "Any")))
  
  safe_colorblind_palette <- c(
    "#CC6677","#DDCC77","#117733","#332288",
    "#AA4499","#44AA99","#999933","#882255",
    "#661100","#6699CC","#888888","#88CCEE")
  
  #Plot
  fig_hist2 <- 
    trust_vacc_together |>
    dplyr::mutate(group = plyr::mapvalues(group, "All", "All LMICs")) |> 
    dplyr::filter(sub == "Any") |> 
    ggplot2::ggplot(aes(estimate, tag)) + 
    ggplot2::geom_bar(stat = "identity", position = "dodge", fill = "#DDCC77") + 
    ggplot2::facet_wrap(~group, ncol = 2, strip.position = "left")  +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      name = "Answer", 
      values = safe_colorblind_palette[c(1,3,2)]) + 
    ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 16), 
                     guide = guide_axis(angle = 90)) +
    ggplot2::labs(title = "Which of the following people would you trust MOST to help you decide whether you would get a COVID-19 vaccine?",
         y = "") +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "bottom",
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          axis.text.y = element_text(hjust = 0))
  
  return(fig_hist2)
}

make_fig_4(utils::read.csv("../data/combined.csv", stringsAsFactors = FALSE))
