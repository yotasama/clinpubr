load_all()
library(clinpubr)
library(rms)

set.seed(123)
data(cancer, package = "survival")
# Create a dummy time2 variable
cancer$time2 <- cancer$time + runif(nrow(cancer), 0, 100)
cancer=cancer[complete.cases(cancer[,c("time","inst","age")]),]

p1=rcs_plot(cancer, x = "age", y = "status", time = "time", time2="time2", covars = "ph.karno", save_plot = FALSE)
p2=rcs_plot(cancer, x = "age", y = "status", time = "time", time2="time2", cluster="inst",covars = "ph.karno", save_plot = FALSE)

model=cph(Surv(time, time2, status) ~ rcs(age,4)+ ph.karno, data = cancer,x=T,y=T)
model
model2=robcov(model, cluster = cancer$inst)
model2
dd <- rms::datadist(cancer, q.display = c(0.025, 0.975))
old_datadist <- getOption("datadist")
on.exit(
  {
    options(datadist = old_datadist)
  },
  add = TRUE
)
options(datadist = dd)
x1=Predict(model,
  name = "age", fun = exp, type = "predictions", conf.int = 0.95, digits = 2
)
x2=Predict(model2,
  name = "age", fun = exp, type = "predictions", conf.int = 0.95, digits = 2
)
head(x1)
head(x2)


cohort <- data.frame(
  age = c(17, 25, 30, NA, 50, 60),
  sex = c("M", "F", "F", "M", "F", "M"),
  value = c(1, NA, 3, 4, 5, NA),
  dementia = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
)
exclusion_count(
  cohort,
  age < 18,
  is.na(value),
  dementia == TRUE,
  .criteria_names = c(
    "Age < 18 years",
    "Missing value",
    "History of dementia"
  )
)

data(cancer, package = "survival")
data = cancer
y = "status"
predictor = "age"
time = "time"
time2 = NULL
rcs_knots = NULL
covars = NULL
cluster = NULL
returned = c("full", "predictor_split", "predictor_combined")

returned = "full"
if (!is.null(rcs_knots) && rcs_knots == 0) rcs_knots <- NULL
analysis_type <- if (!is.null(time)) {
  "cox"
} else if (length(levels(as.factor(data[[y]]))) == 2) {
  "logistic"
} else {
  "linear"
}
covars <- remove_conflict(covars, c(y, predictor, time, time2))

predictor_type <- if (is.factor(data[[predictor]]) && length(levels(data[[predictor]])) > 2) {
  "multi_factor"
} else {
  "num_or_binary"
}

formula <- create_formula(y, predictor, time = time, time2 = time2, covars = covars, rcs_knots = rcs_knots)

# need to check `rms` argument
model <- fit_model(formula, data = data, analysis_type = analysis_type, cluster = cluster)
model2 <- fit_model(formula, data = data, analysis_type = analysis_type, rms=T,cluster = cluster)
full_res <- broom::tidy(model, conf.int = TRUE, exponentiate = analysis_type %in% c("logistic", "cox"))
if (analysis_type %in% c("linear", "logistic")) full_res <- full_res[-1, ]

if (returned == "full") {
  return(as.data.frame(full_res))
} else if (predictor_type == "num_or_binary" && is.null(rcs_knots)) {
  return(as.data.frame(full_res[1, ]))
} else if (returned == "predictor_split") {
  if (is.numeric(data[[predictor]])) {
    res <- as.data.frame(full_res[seq_len(rcs_knots - 1), ])
  } else {
    res <- as.data.frame(full_res[seq_len(length(levels(data[[predictor]])) - 1), ])
  }
  if (any(!grepl(predictor, res$term))) stop("predictor not found in the regression result")
  return(res)
} else {
  if (is.numeric(data[[predictor]]) && !is.null(rcs_knots)) {
    model <- fit_model(formula, data = data, analysis_type = analysis_type, rms = TRUE, cluster = cluster)
    ps <- unname(anova(model)[, "P"])
    return(list(estimate = NA, p_overall = ps[1], p_nonlinear = ps[2]))
  } else {
    return(list(
      estimate = NA,
      p.value = broom::tidy(car::Anova(model, type = 2, test.statistic = "Wald"))$p.value[1]
    ))
  }
}

library(Greg)
library(broom)
tidy(model2)
dd <- rms::datadist(data, q.display = c(0.025, 0.975))
old_datadist <- getOption("datadist")
on.exit(
  {
    options(datadist = old_datadist)
  },
  add = TRUE
)
options(datadist = dd)
options(datadist = NULL)
