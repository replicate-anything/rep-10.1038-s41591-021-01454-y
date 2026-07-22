# Fig. 1 — Acceptance rates, overall and by respondent characteristics
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y

library(ggplot2)

make_fig_1 <- function(data) {
  safe_colorblind_palette <- c(
    "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
    "#44AA99", "#999933", "#882255", "#661100", "#6699CC",
    "#888888", "#88CCEE"
  )

  fig_1_ages <- ggplot2::ggplot(data, ggplot2::aes(x = tag, y = estimate, color = cat)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      size = .5,
      width = .2,
      position = ggplot2::position_dodge(0.6)
    ) +
    ggplot2::geom_point(position = ggplot2::position_dodge(0.6)) +
    ggplot2::facet_grid(. ~ var, scales = "free_x", space = "free") +
    ggplot2::coord_flip() +
    ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE, nrow = 2)) +
    ggplot2::geom_vline(xintercept = 4.5, color = "darkgrey") +
    ggplot2::geom_vline(xintercept = 3.5, color = "darkgrey") +
    ggplot2::geom_vline(xintercept = 2.5, color = "darkgrey") +
    ggplot2::scale_colour_manual(values = safe_colorblind_palette) +
    ggplot2::labs(
      title = "If a COVID-19 vaccine becomes available in [country], would you take it?",
      color = "Subgroups",
      x = ""
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::ylim(0, 100) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text.y = ggplot2::element_text(hjust = 0)
    )

  fig_1_ages
}
