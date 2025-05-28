set.seed(1)
load_all()
library(knitr)
var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
baseline_table(mtcars, var_types = var_types, contDigits = 1, filename = "baseline.csv")

t1=read.csv("baseline.csv", check.names = FALSE)
kable(t1)


data(cancer, package = "survival")
df <- kidney
df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
df <- na.omit(df[, -c(1:3)])

model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
df$base_pred <- predict(model0, type = "response")
model <- glm(dead ~ ., family = binomial(), data = df)
df$full_pred <- predict(model, type = "response")

x=classif_model_compare(df, target_var ="dead", model_names =c("base_pred", "full_pred"),return_results = T)
kable(x$metric_table)

set.seed(5)
dummy_importance <- runif(20,0.2,0.6)^5
names(dummy_importance) <- paste0("var", 1:20)
importance_plot(dummy_importance, top_n = 15, split_at = 10)

