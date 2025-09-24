load_all()
library(rms)

data(cancer, package = "survival")
regression_fit(data = cancer, y = "status", predictor = "age", time = "time", rcs_knots = 4)

for(v in c("sex", "ph.ecog")) {
  cancer[,v] <- as.factor(cancer[,v])
}
cancer$status = cancer$status-1

data = cancer
y = "status"
predictor = "age"
time = "time"
time2 = NULL
covars = c("ph.ecog","wt.loss")
group_var = "sex"
rcs_knots = 4
cluster = "inst"
returned = c("full", "predictor_split", "predictor_combined")
returned = "full"

if(!is.null(cluster)){
  data = data %>% 
    dplyr::select(all_of(c(y,predictor,time,time2,group_var,covars,cluster))) %>% 
    na.omit()
}

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

options(datadist=datadist(data))

formula <- create_formula(y, predictor, group_var = group_var , time = time, time2 = time2, covars = covars, rcs_knots = rcs_knots)
formula2 <- create_formula(y, predictor, group_var = group_var ,time = time, time2 = time2, covars = covars, rcs_knots = rcs_knots, interaction = TRUE)
# need to check `rms` argument
model <- fit_model(formula, data = data, analysis_type = analysis_type,rms=T)
model2<- fit_model(formula2, data = data, analysis_type = analysis_type,rms=T)
lrtest(model,model2)$stats[["P"]]
anova(model,model2)

model2 <- fit_model(formula, data = data, analysis_type = analysis_type, cluster=cluster,rms=T)
model3=geeglm(formula, data = data, id = data[[cluster]], family = binomial)
model4 <- fit_model(formula, data = data, analysis_type = analysis_type)

broom::tidy(model, conf.int = TRUE, exponentiate =  analysis_type %in% c("logistic", "cox"))
as.data.frame(broom::tidy(model2, conf.int = TRUE, exponentiate =  analysis_type %in% c("logistic", "cox")))
regression_fit(data = data, y = y, predictor = predictor, time = NULL, time2 = time2,
               covars = covars, rcs_knots = NULL, cluster = cluster, returned = "full")

formula <- create_formula(y, predictor, time = time, time2 = time2, covars = covars, rcs_knots = NULL)
formula2 <- create_formula(y, predictor, time = time, time2 = time2, covars = covars, rcs_knots = 4)
model3 <- fit_model(formula, data = data, analysis_type = analysis_type, cluster=cluster)
model2 <- fit_model(formula2, data = data, analysis_type = analysis_type, cluster=cluster)
model <- fit_model(formula2, data = data, analysis_type = analysis_type,rms=T)
car::Anova(model2, type = 2, test.statistic = "Wald")
formula_glm_full <- as.formula(
  paste(y, "~", paste(c(predictor, sprintf("ns(%s, df=3)", predictor), covars), collapse = " + "))
)
model4 <- fit_model(formula_glm_full, data = data, analysis_type = analysis_type, cluster=cluster)
car::Anova(model4, type = 2)



model4=robcov(model4, cluster = data[[cluster]])
full_res <- broom::tidy(model, conf.int = TRUE, exponentiate =  F)#analysis_type %in% c("logistic", "cox"))
full_res = data.frame(term = names(coef(model2)),
                      estimate = coef(model2),
                      std.error = sqrt(diag(vcov(model2))),
                      p.value = anova(model2,test='LR'),
                      conf.low = NA,
                      conf.high = NA)

if (analysis_type %in% c("linear", "logistic")) full_res <- full_res[-1, ]

model2 <- fit_model(formula, data = data, analysis_type = analysis_type, rms = TRUE) #, cluster = cluster)


sqrt(diag(vcov(model2)))
if (returned == "full") {
  as.data.frame(full_res)
} else if (predictor_type == "num_or_binary" && is.null(rcs_knots)) {
  as.data.frame(full_res[1, ])
} else if (returned == "predictor_split") {
  if (is.numeric(data[[predictor]])) {
    res <- as.data.frame(full_res[seq_len(rcs_knots - 1), ])
  } else {
    res <- as.data.frame(full_res[seq_len(length(levels(data[[predictor]])) - 1), ])
  }
  if (any(!grepl(predictor, res$term))) stop("predictor not found in the regression result")
  res
} else {
  if (is.numeric(data[[predictor]]) && !is.null(rcs_knots)) {
    model <- fit_model(formula, data = data, analysis_type = analysis_type, rms = TRUE)
    ps <- unname(anova(model)[, "P"])
    list(estimate = NA, p_overall = ps[1], p_nonlinear = ps[2])
  } else {
    list(
      estimate = NA,
      p.value = broom::tidy(car::Anova(model, type = 2, test.statistic = "Wald"))$p.value[1]
    )
  }
}


data(cancer, package = "survival")
# Recode status to 0/1 for logistic regression
cancer$status <- ifelse(cancer$status == 2, 1, 0) # 1=dead, 0=alive
# Cox plot

interaction_p_value(cancer, y = "status", predictor = "age", group_var = "sex", time = "time")

formula1 <- create_formula("status", "age", "sex",
  time ="time", 
  interaction = FALSE
)
formula2 <- create_formula("status", "age", "sex",
  time ="time", 
  interaction = TRUE
)
cancer$sex=to_factor(cancer$sex)
model <- fit_model(formula1, data = cancer, analysis_type = "cox",rms=T)
model2<- fit_model(formula2, data = cancer, analysis_type = "cox",rms=T)
lrtest(model,model2)$stats[["P"]]
model
model2
data = cancer
y = "status"
predictor = "age"
group_var = "sex"
time = "time"
time2 = NULL
covars = NULL
rcs_knots = NULL
cluster = NULL
interaction_p_value <- function(
    data, y, predictor, group_var, time = NULL, time2 = NULL, covars = NULL,
    rcs_knots = NULL, cluster = NULL) {
  analysis_type <- if (!is.null(time)) {
    "cox"
  } else if (length(levels(as.factor(data[[y]]))) == 2) {
    "logistic"
  } else {
    "linear"
  }
  data[[group_var]] <- to_factor(data[[group_var]])
  covars <- remove_conflict(covars, c(y, predictor, group_var, time, time2))

  formula1 <- create_formula(y, predictor, group_var,
    time = time, time2 = time2, covars = covars, rcs_knots = rcs_knots,
    interaction = FALSE
  )
  formula2 <- create_formula(y, predictor, group_var,
    time = time, time2 = time2, covars = covars, rcs_knots = rcs_knots,
    interaction = TRUE
  )

  # if(!is.null(cluster)){
  #   model1 <- fit_model(formula1, data = data, analysis_type = analysis_type, cluster=cluster, rms=TRUE)
  #   model2 <- fit_model(formula2, data = data, analysis_type = analysis_type, cluster=cluster, rms=TRUE)
  #   return(lrtest(model,model2)$stats[["P"]])
  # }else{
  #   model1 <- fit_model(formula1, data = data, analysis_type = analysis_type)
  #   model2 <- fit_model(formula2, data = data, analysis_type = analysis_type)
  #   return(broom::tidy(anova(model1, model2, test = "LRT"))$p.value[2])
  # }
  model1 <- fit_model(formula1, data = data, analysis_type = analysis_type, cluster=cluster, rms=TRUE)
  model2 <- fit_model(formula2, data = data, analysis_type = analysis_type, cluster=cluster, rms=TRUE)
  return(lrtest(model1,model2)$stats[["P"]])
}
data(cancer, package = "survival")
rcs_plot(cancer, x = "age", y = "status", time = "time", covars = "ph.karno", save_plot = FALSE)


df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p <- ggplot(df, aes(trt, resp, colour = "red"))
p+geom_crossbar(aes(ymin = lower, ymax = upper, fill = group),alpha=0.1 , width = 0.2,linewidth=0.03,fatten=50)

library(tictoc)
x=data.frame(matrix(rnorm(1e7),ncol=100))
l=list(x,x,x,x,x,x,x)
tic()
y=data.table::rbindlist(l)
toc()

tic()
y=dplyr::bind_rows(l)
toc()

tic()
y=do.call(rbind,l)
toc()