source('R/data_overview.R')

# Create a test dataset with various quality issues
test_df <- data.frame(
  id = 1:100,
  # Numeric stored as character
  age = c(as.character(1:98), '100+', 'missing'),
  # Normal numeric with outliers
  income = c(rnorm(95, 50000, 10000), rep(200000, 5)),
  # Character with some numeric values
  score = c(as.character(1:98), 'A+', 'B-'),
  # Factor with missing values
  gender = sample(c('M', 'F', NA), 100, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
  # Constant variable
  constant = rep('same', 100),
  # Logical with missing values
  active = sample(c(TRUE, FALSE, NA), 100, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  # Variable with high missingness
  rare_var = c(rnorm(20), rep(NA, 80)),
  # Date variable
  date = as.Date('2020-01-01') + sample(0:365, 100, replace = TRUE),
  # Binary variable with dominant category
  almost_constant = sample(c('Y', 'N'), 100, replace = TRUE, prob = c(0.98, 0.02)),
  stringsAsFactors = FALSE
)
# Add duplicate rows
test_df <- rbind(test_df, test_df[1:5,])

# Test the function
overview <- data_overview(test_df)
