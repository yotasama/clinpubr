% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classif_model_compare.r
\name{classif_model_compare}
\alias{classif_model_compare}
\title{Performance comparison of classification models}
\usage{
classif_model_compare(
  data,
  target_var,
  model_names,
  colors = NULL,
  save_output = FALSE,
  figure_type = "png",
  output_prefix = "model_compare",
  as_probability = FALSE,
  auto_order = TRUE
)
}
\arguments{
\item{data}{A data frame containing the target variable and the predicted values.}

\item{target_var}{A string specifying the name of the target variable in the data frame.}

\item{model_names}{A vector of strings specifying the names of the models to compare.}

\item{colors}{A vector of colors to use for the plots. The last 2 colors are used for the
"Treat all" and "Treat none" lines in the DCA plot.}

\item{save_output}{A logical value indicating whether to output the results to files.}

\item{figure_type}{A character string of the figure type. Can be \code{"png"}, \code{"pdf"}, and other types that
\code{ggplot2::ggsave()} support.}

\item{output_prefix}{A string specifying the prefix for the output files.}

\item{as_probability}{A logical or a vector of variable names. The logical value indicates
whether to convert variables not in range 0 to 1 into this range.
The vector of variable names means to convert these variables to the range of 0 to 1.}

\item{auto_order}{A logical value indicating whether to automatically order the models by their AUCs.
If \code{TRUE}, the models will be ordered by their AUCs in descending order. If \code{FALSE}, the order
in \code{model_names} will be retained.}
}
\value{
A list of various results. If the output files are not in desired format,
these results can be modified for further use.
\itemize{
\item metric_table: A data frame containing the performance metrics for each model.
\item roc_plot: A \code{ggplot} object of ROC curves.
\item dca_plot: A \code{ggplot} object of decision curve analysis plots.
\item calibration_plot: A \code{ggplot} object of calibration plots.
}
}
\description{
Compare the performance of classification models by commonly used
metrics, and generate commonly used plots including receiver operating characteristic
curve plot, decision curve analysis plot, and calibration plot.
}
\section{Metrics}{

\itemize{
\item AUC: Area Under the Receiver Operating Characteristic Curve
\item Accuracy: Overall accuracy
\item Sensitivity: True positive rate
\item Specificity: True negative rate
\item Pos Pred Value: Positive predictive value
\item Neg Pred Value: Negative predictive value
\item F1: F1 score
\item Kappa: Cohen's kappa
\item Brier: Brier score
\item cutoff: Optimal cutoff for classification, metrics that require a cutoff are
based on this value.
\item Youden: Youden's J statistic
\item HosLem: Hosmer-Lemeshow test p-value
}
}

\examples{
data(cancer, package = "survival")
df <- kidney
df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
df <- na.omit(df[, -c(1:3)])

model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
model <- glm(dead ~ ., family = binomial(), data = df)
df$base_pred <- predict(model0, type = "response")
df$full_pred <- predict(model, type = "response")

classif_model_compare(df, "dead", c("base_pred", "full_pred"), save_output = FALSE)
}
