# Extended Data Fig. 2 — Average vaccine acceptance leaving one or two study samples out
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y

library(dplyr)
library(ggplot2)
library(scales)

make_ext_fig_2 <- function(data) {
  hist_data <- data[[1]]
  ref_data <- data[[2]]

  # Drop rows with missing facet levels so facet_grid does not show an NA column.
  hist_data <- hist_data |> dplyr::filter(!is.na(m), !is.na(var), !is.na(tag))
  ref_data <- ref_data |> dplyr::filter(!is.na(m), !is.na(var), !is.na(tag))

  safe_colorblind_palette <- c(
    "#CC6677", "#DDCC77", "#117733", "#332288",
    "#AA4499", "#44AA99", "#999933", "#882255",
    "#661100", "#6699CC", "#888888", "#88CCEE"
  )

  hist_data$estimate <- hist_data$estimate * 100

  ggplot2::ggplot(hist_data, ggplot2::aes(estimate, color = tag, fill = tag)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 200,
      position = "dodge",
      alpha = .3,
      linewidth = .3
    ) +
    ggplot2::geom_vline(
      data = ref_data,
      ggplot2::aes(xintercept = estimate, color = cat, linetype = group),
      linewidth = .9
    ) +
    ggplot2::facet_grid(var + tag ~ m) +
    ggplot2::scale_color_manual(values = safe_colorblind_palette) +
    ggplot2::scale_fill_manual(values = safe_colorblind_palette) +
    ggplot2::scale_linetype_manual(values = c("solid", "11", "dashed")) +
    ggplot2::scale_x_continuous(n.breaks = 8) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1, suffix = "", accuracy = 0.1)
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}
