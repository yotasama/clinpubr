#' Importance plot
#' @description Creates an importance plot from a named vector of values.
#' @param x A named vector of values, typically importance scores from models.
#' @param top_n The number of top values to show. If NULL, all values are shown.
#' @param color A length-2 vector of low and high colors, or a single color for the bars.
#' @param show_legend A logical value indicating whether to show the legend.
#' @param split_at The index at which to split the plot into two halves, usually used to illustrate
#'   variable selection. If NULL, no split is made.
#' @param show_labels A logical value indicating whether to show the labels on the bars.
#' @param label_nsmall The number of decimal places to show in the labels.
#' @param label_color The color of the labels.
#' @param label_size The size of the labels.
#' @param label_hjust The horizontal justification of the labels.
#' @param save_plot A logical value indicating whether to save the plot.
#' @param filename The filename to save the plot as.
#' @details The importance plot is a bar plot that shows the importance of each variable in a model.
#'   The variables are sorted in descending order of importance, and the top_n variables are shown.
#'   If top_n is NULL, all variables are shown. The plot can be split into two halves at a specified
#'   index, which is useful for illustrating variable selection.
#'
#' @returns A `ggplot` object
#' @export
#' @examples
#' set.seed(1)
#' dummy_importance <- runif(20)^5
#' names(dummy_importance) <- paste0("var", 1:20)
#' importance_plot(dummy_importance, top_n = 15, split_at = 10, save_plot = FALSE)
importance_plot <- function(x, top_n = NULL, color = c("#56B1F7", "#132B43"), show_legend = FALSE, split_at = NULL,
                            show_labels = TRUE, label_nsmall = 3, label_color = "black", label_size = 3,
                            label_hjust = 0.1, save_plot = TRUE, filename = "importance.png") {
  x <- sort(x, decreasing = TRUE)
  tmp <- data.frame(name = names(x), value = x)
  if (length(color) == 1) color <- rep(color, 2)
  if (!is.null(top_n) && top_n < length(x)) {
    tmp <- tmp[1:(top_n + 1), ]
    tmp$name[top_n + 1] <- "..."
    tmp$label <- format(tmp$value, nsmall = label_nsmall, digits = 1)
    tmp$label[top_n + 1] <- "..."
    tmp$name <- factor(tmp$name,
      levels = rev(tmp$name),
      labels = rev(c(tmp$name[1:top_n], "..."))
    )
    tmp$value[top_n + 1] <- 0
  } else {
    tmp$label <- format(tmp$value, nsmall = label_nsmall, digits = 1)
    tmp$name <- factor(tmp$name,
      levels = rev(tmp$name),
      labels = rev(tmp$name)
    )
  }

  p <- ggplot(data = tmp, mapping = aes(x = name, y = value)) +
    geom_bar(aes(fill = value), stat = "identity", alpha = 1, show.legend = show_legend) +
    scale_fill_gradient(low = color[1], high = color[2]) +
    labs(x = "", y = "Importance", fill = "Importance") +
    scale_y_continuous(expand = c(0.1, 0)) +
    coord_flip() +
    theme_classic()
  if (show_labels) {
    p <- p + geom_text(aes(label = label), color = label_color, nudge_y = label_hjust, size = 3)
  }
  if (!is.null(split_at)) {
    h <- nrow(tmp) - split_at + 0.5
    p <- p + geom_vline(xintercept = h, linetype = 3)
  }
  if (save_plot) ggsave(filename, p, width = 4, height = 4)
  return(p)
}
