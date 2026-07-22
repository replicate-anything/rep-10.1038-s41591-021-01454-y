# Shared analysis helpers for Solís Arce et al. (2021)
# Used by prep steps and figure make_* functions.

study_root <- function() {
  root <- Sys.getenv("REPLICATE_STUDY_ROOT", unset = "")
  if (nzchar(root)) {
    return(root)
  }
  normalizePath(file.path(".."), winslash = "/", mustWork = FALSE)
}

study_output_dir <- function() {
  out_dir <- file.path(study_root(), "outputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

safe_colorblind_palette <- c(
  "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
  "#44AA99", "#999933", "#882255", "#661100", "#6699CC",
  "#888888", "#88CCEE"
)

study_weighting <- function(data) {
  data |>
    dplyr::group_by(country) |>
    dplyr::mutate(weight = weight / sum(weight)) |>
    dplyr::ungroup()
}

lm_helper <- function(data, ...) {
  data <- study_weighting(data)
  fit <- estimatr::lm_robust(data = data, ...)
  dplyr::bind_cols(broom::tidy(fit), n = nobs(fit))
}

# Matches author 2_tables.Rmd: nest_by(group, get(x)) then rename
grp_analysis <- function(df, y, x) {
  df |>
    dplyr::filter(dplyr::if_all(c(dplyr::all_of(x), dplyr::all_of(y), cluster, weight), ~ !is.na(.))) |>
    dplyr::nest_by(group, get(x)) |>
    dplyr::summarize(
      lm_helper(
        data = data,
        formula = stats::as.formula(paste0(y, "~ 1")),
        cluster = cluster,
        weight = weight,
        se_type = "stata"
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(!!x := "get(x)")
}

reasons_together <- function(df, reason, num = "Yes") {
  df |>
    dplyr::filter(
      take_vaccine %in% num,
      dplyr::if_all(c(dplyr::all_of(reason), cluster, weight), ~ !is.na(.))
    ) |>
    dplyr::nest_by(group) |>
    dplyr::summarize(
      lm_helper(
        data = data,
        formula = stats::as.formula(paste0(reason, "~ 1")),
        cluster = cluster,
        weight = weight,
        se_type = "stata"
      ),
      .groups = "drop"
    )
}

reasons_together_subgroup <- function(df, reason, num = "Yes", dem_group = NA, dem_subgroup = NA) {
  if (identical(dem_group, "gender")) {
    df <- dplyr::filter(df, gender %in% dem_subgroup)
  }
  df |>
    dplyr::filter(take_vaccine %in% num, !is.na(.data[[reason]])) |>
    dplyr::nest_by(group) |>
    dplyr::summarize(
      lm_helper(
        data = data,
        formula = stats::as.formula(paste0(reason, "~ 1")),
        cluster = cluster,
        weight = weight,
        se_type = "stata"
      ),
      .groups = "drop"
    )
}

loo_helper <- function(
    data,
    sample_var,
    loo_n = 1,
    loo_fun = function(dat) {
      lm_helper(
        data = dat,
        formula = take_vaccine_num ~ 1,
        cluster = cluster,
        weight = weight,
        se_type = "stata"
      )
    }) {
  .var <- data[[sample_var]]
  plyr::adply(
    .data = utils::combn(unique(.var), loo_n),
    .margins = 2,
    .fun = function(x) loo_fun(data[!(.var %in% x), , drop = FALSE])
  )
}
