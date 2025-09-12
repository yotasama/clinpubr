#' Plot restricted cubic spline
#' @description This function is a wrapper for `predictor_effect_plot` with `method = "rcs"`.
#' It plots a restricted cubic spline for a predictor in a regression model.
#' @inheritParams predictor_effect_plot
#' @param rcs_color The color for the restricted cubic spline. This is passed to `line_color` in `predictor_effect_plot`.
#' @returns A `ggplot` object, or a list containing the `ggplot` object and other details if `return_details` is `TRUE`.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' # coxph model with time assigned
#' rcs_plot(cancer, x = "age", y = "status", time = "time", covars = "ph.karno", save_plot = FALSE)
#'
#' # logistic model with time not assigned
#' cancer$dead <- cancer$status == 2
#' rcs_plot(cancer, x = "age", y = "dead", covars = "ph.karno", save_plot = FALSE)
rcs_plot <- function(data, x, y, time = NULL, time2 = NULL, covars = NULL, cluster = NULL, knot = 4, add_hist = TRUE,
                     ref = "x_median", ref_digits = 3, show_total_n = TRUE,
                     group_by_ref = TRUE, group_title = NULL, group_labels = NULL, group_colors = NULL,
                     breaks = 20, rcs_color = "#e23e57", print_p_ph = TRUE, trans = "identity", save_plot = FALSE,
                     create_dir = FALSE, filename = NULL, y_lim = NULL, hist_max = NULL, xlim = NULL, height = 6,
                     width = 6, return_details = FALSE) {
  predictor_effect_plot(
    data = data, x = x, y = y, time = time, time2 = time2, covars = covars, cluster = cluster,
    method = "rcs", knot = knot, add_hist = add_hist, ref = ref, ref_digits = ref_digits,
    show_total_n = show_total_n, group_by_ref = group_by_ref, group_title = group_title,
    group_labels = group_labels, group_colors = group_colors, breaks = breaks,
    line_color = rcs_color, print_p_ph = print_p_ph, trans = trans,
    save_plot = save_plot, create_dir = create_dir, filename = filename,
    y_lim = y_lim, hist_max = hist_max, xlim = xlim, height = height,
    width = width, return_details = return_details
  )
}

#' Generate breaks for histogram
#' @description Generate breaks for histogram that covers xlim and includes a ref_val.
#' @param xlim A vector of length 2.
#' @param breaks The number of breaks.
#' @param ref_val The reference value to include in breaks.
#'
#' @returns A vector of breaks of length `breaks + 1`.
#' @export
#' @examples
#' break_at(xlim = c(0, 10), breaks = 12, ref_val = 3.12)
break_at <- function(xlim, breaks, ref_val = NULL) {
  if (length(xlim) != 2) stop("`xlim` must be a vector of length 2")
  bks <- seq(xlim[1], xlim[2], length.out = breaks + 1)
  if (!is.null(ref_val) && !ref_val %in% bks) {
    bks <- seq(xlim[1], xlim[2], length.out = breaks)
    h <- (xlim[2] - xlim[1]) / (breaks - 1)
    bks <- c(bks[1] - h, bks)
    tmp <- ref_val - bks
    tmp <- tmp[tmp > 0]
    bks <- bks + tmp[length(tmp)]
  }
  bks
}

#' Filter predictors for RCS
#' @description Filter predictors that can be used to fit for RCS models.
#' @param data A data frame.
#' @param predictors A vector of predictor names to be filtered.
#'
#' @returns A vector of predictor names. These variables are numeric and have more than 5 unique values.
#' @export
#' @examples
#' filter_rcs_predictors(mtcars)
filter_rcs_predictors <- function(data, predictors = NULL) {
  if (is.null(predictors)) {
    predictors <- colnames(data)
  }
  res <- c()
  for (x in predictors) {
    if (is.numeric(data[[x]]) && length(na.omit(unique(data[[x]]))) > 5) {
      res <- union(res, x)
    }
  }
  return(res)
}
