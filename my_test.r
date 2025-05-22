load_all()
# Example 1: Using the list as change_rules is more convenient for small datasets.
df <- data.frame(
  subject = c("a", "a", "b", "b", "b", "c", "c"), value = c(1, 2, 3, 4, 5, 6, 7),
  unit = c(NA, "x", "x", "x", "y", "a", "b")
)
change_rules <- list(
  list(subject = "a", target_unit = "x", units2change = c(NA), coeffs = c(20)),
  list(subject = "b"),
  list(subject = "c", target_unit = "b")
)
unit_standardize(df,
  subject_col = "subject", value_col = "value", unit_col = "unit",
  change_rules = change_rules
)

# Example 2: Using the labeled result from `unit_view()` as the input is more robust for large datasets.
df <- data.frame(subject = sample(c("a", "b"), 1000, replace = T), value = runif(1000))
df$unit <- NA
df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"), sum(df$subject == "a"), replace = T)
df$value[df$subject == "a" & df$unit == "mg/L"] <- df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
df$unit[df$subject == "b"] <- sample(c(NA, "m.g", "mg"), sum(df$subject == "b"),prob = c(0.3,0.05,0.65), replace = T)
df$value[df$subject == "b" & df$unit %in% "mg"] <- df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
  sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = T)

unit_table <- unit_view(df = df, subject_col = "subject", value_col = "value", unit_col = "unit", save_table = FALSE)
unit_table$label <- c("t", NA, 1e-3, NA, NA, "r") # labeling the units

df_standardized <- unit_standardize(df = df, subject_col = "subject", value_col = "value",
                                    unit_col = "unit", change_rules = unit_table)
unit_view(df = df_standardized, subject_col = "subject", value_col = "value", unit_col = "unit",
          save_table = FALSE, conflicts_only = FALSE)


df <- data.frame(subject = "a", value = 1, unit = "x")
change_rules <- list(list(subject = "a", target_unit = c("x", "y")))
expect_error(unit_standardize(df, "subject", "value", "unit", change_rules), "too many targets!")
# Mismatched coeffs and units2change
change_rules <- list(list(subject = "a", target_unit = "x", units2change = c("z", "y"), coeffs = 1))
expect_error(unit_standardize(df, "subject", "value", "unit", change_rules), "`coeffs` should have the same length as `units2change`!")
# Remove units (set to NA)
df <- data.frame(subject = "a", value = 1, unit = "bad")
change_rules <- list(list(subject = "a", units2remove = "bad"))
result <- unit_standardize(df, "subject", "value", "unit", change_rules)
expect_true(all(is.na(result$value[result$unit == "bad"])))

df <- data.frame(subject = "a", value = 1, unit = "bad")
change_rules <- list(list(subject = "a", units2remove = "bad"))