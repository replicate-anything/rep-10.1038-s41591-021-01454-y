DOI <- "10.1038/s41591-021-01454-y"
FOLDER <- "10.1038_s41591-021-01454-y"
STUDY_REPO <- "replicate-anything/rep-10.1038-s41591-021-01454-y"

study_test_context <- function() {
  study_root <- normalizePath(
    testthat::test_path("..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
  registry_root <- normalizePath(
    file.path(study_root, "..", "registry"),
    winslash = "/",
    mustWork = FALSE
  )
  monorepo_root <- normalizePath(
    file.path(study_root, ".."),
    winslash = "/",
    mustWork = FALSE
  )

  local_index <- data.frame(
    folder = FOLDER,
    doi = paste0("https://doi.org/", DOI),
    title = "COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries",
    journal = "Nature Medicine",
    year = 2021,
    authors = "Solís Arce et al.",
    repo = STUDY_REPO,
    stringsAsFactors = FALSE
  )

  list(
    study_root = study_root,
    registry_root = registry_root,
    monorepo_root = monorepo_root,
    local_index = local_index
  )
}

run_with_study_options <- function(expr) {
  ctx <- study_test_context()
  testthat::skip_if_not(dir.exists(ctx$registry_root), "registry checkout missing")
  withr::with_options(
    list(
      replicateEverything.registry_root = ctx$registry_root,
      replicateEverything.index = ctx$local_index,
      replicateEverything.use_sibling_packages = TRUE,
      replicateEverything.study_folders_root = ctx$monorepo_root
    ),
    expr
  )
}

test_that("run_replication executes fig_1", {
  testthat::skip_if_not_installed("replicateEverything")
  testthat::skip_if_not_installed("ggplot2")

  run_with_study_options({
    invisible(suppressMessages(capture.output({
      plot <- replicateEverything::run_replication(DOI, "fig_1")
    })))
    testthat::expect_true(inherits(plot, "ggplot"))
  })
})

test_that("run_replication executes tab_2", {
  testthat::skip_if_not_installed("replicateEverything")

  run_with_study_options({
    invisible(suppressMessages(capture.output({
      tab <- replicateEverything::run_replication(DOI, "tab_2")
    })))
    testthat::expect_true(is.character(tab) && grepl("Burkina Faso", tab[1], fixed = TRUE))
  })
})

test_that("slimmed study exposes the six paper targets", {
  testthat::skip_if_not_installed("replicateEverything")
  testthat::skip_if_not_installed("yaml")

  meta <- yaml::read_yaml(testthat::test_path("..", "..", "replication.yml"))
  ids <- vapply(meta$steps, `[[`, character(1), "id")
  testthat::expect_setequal(
    ids,
    c("tab_2", "fig_1", "fig_2", "fig_3", "ext_fig_1", "ext_fig_2")
  )
})
