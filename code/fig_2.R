# Fig. 2 — Reasons not to take the vaccine
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y
# Note: precomputed estimates currently live in data/fig_3.csv (legacy filename).

library(dplyr)
library(ggplot2)
library(plyr)

make_fig_2 <- function(data) {
  safe_colorblind_palette <- c(
    "#CC6677", "#DDCC77", "#117733", "#332288",
    "#AA4499", "#44AA99", "#999933", "#882255",
    "#661100", "#6699CC", "#888888", "#88CCEE"
  )

  data |>
    dplyr::filter(!is.na(n_sub)) |>
    dplyr::mutate(name = plyr::mapvalues(name, "All", "All LMICs")) |>
    ggplot2::ggplot(aes(name, estimate, color = tag)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      size = .5, width = .2, position = position_dodge(0.6)
    ) +
    ggplot2::geom_point(shape = 16, position = ggplot2::position_dodge(0.6)) +
    ggplot2::facet_grid(. ~ tag, space = "free", labeller = label_wrap_gen(width = 15)) +
    ggplot2::scale_size_discrete(range = c(1, 3), name = "Number of observations") +
    ggplot2::geom_vline(xintercept = 3.5, color = "darkgrey") +
    ggplot2::geom_vline(xintercept = 2.5, color = "darkgrey") +
    ggplot2::guides(color = FALSE) +
    ggplot2::scale_colour_manual(values = safe_colorblind_palette) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Why would you not take the COVID-19 vaccine?",
      x = ""
    ) +
    ggplot2::theme_bw() +
    ggplot2::ylim(c(-2, 100)) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text.y = element_text(hjust = 0)
    )
}
