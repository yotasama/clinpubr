% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unit_standardize.r
\name{unit_view}
\alias{unit_view}
\title{Generate a table of conflicting units.}
\usage{
unit_view(
  df,
  subject_col,
  value_col,
  unit_col,
  quantiles = c(0.025, 0.975),
  save_table = FALSE,
  filename = NULL,
  conflicts_only = TRUE
)
}
\arguments{
\item{df}{A data frame of medical records that contains test subject, value, and unit cols.}

\item{subject_col}{The name of the subject column.}

\item{value_col}{The name of the value column.}

\item{unit_col}{The name of the unit column.}

\item{quantiles}{A vector of quantiles to be shown in the table.}

\item{save_table}{A logical value indicating whether to save the table to a csv file.}

\item{filename}{The name of the csv file to be saved.}

\item{conflicts_only}{A logical value indicating whether to only show the conflicting units.}
}
\value{
A data frame of conflicting units.
}
\description{
Get a table of conflicting units for the clinical data, along with the some useful information,
this table could be labeled and used for unit standardization.
}
\examples{
df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
df$unit <- NA
df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
  sum(df$subject == "a"),
  replace = TRUE
)
df$value[df$subject == "a" & df$unit == "mg/L"] <-
  df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
df$unit[df$subject == "b"] <- sample(c(NA, "g", "mg"), sum(df$subject == "b"), replace = TRUE)
df$value[df$subject == "b" & df$unit \%in\% "mg"] <-
  df$value[df$subject == "b" & df$unit \%in\% "mg"] * 1000
df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
  sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)
unit_view(
  df = df, subject_col = "subject",
  value_col = "value", unit_col = "unit", save_table = FALSE
)
}
