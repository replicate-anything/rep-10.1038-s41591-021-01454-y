# Substantive checks for tab_2 — published WGM/WHO aggregates

tab_2_benchmark_rows <- function() {
  data.frame(
    country = c("Burkina Faso", "Colombia"),
    Effectiveness = c("87", "83"),
    Safety = c("72", "84"),
    Important = c("95", "99"),
    BCG = c(98, 89),
    DTP1 = c(95, 92),
    MCV1 = c(88, 95),
    Coverage = c("97", "95"),
    stringsAsFactors = FALSE
  )
}

#' @param object data.frame from make_tab_2()
substantive_check_tab_2 <- function(object, tolerance = 0) {
  stopifnot(is.data.frame(object))
  expected <- tab_2_benchmark_rows()
  got <- as.data.frame(object)
  got$country <- as.character(got$country)
  for (i in seq_len(nrow(expected))) {
    row <- got[got$country == expected$country[[i]], , drop = FALSE]
    testthat::expect_equal(nrow(row), 1L, info = expected$country[[i]])
    for (col in setdiff(names(expected), "country")) {
      testthat::expect_equal(
        as.character(row[[col]][[1]]),
        as.character(expected[[col]][[i]]),
        info = paste(expected$country[[i]], col)
      )
    }
  }
  invisible(TRUE)
}
