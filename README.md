# ezmedpub: Tools for Accelerating Medical Publication

## Overview
`ezmedpub` (Easy Medical Publication) is an R package designed to streamline the workflow from clinical data processing to publication-ready outputs. It provides tools for clinical data cleaning, significant result screening, and generating tables/figures suitable for medical journals.

## Key Features
- **Clinical Data Cleaning**: Functions to handle missing values, standardize units, convert dates, and clean numerical/categorical variables.
- **Significant Result Screening**: Tools for interaction analysis, model comparison, and regression result extraction to identify key findings.
- **Publication-Ready Outputs**: Generate baseline characteristic tables, forest plots, RCS curves, and other visualizations formatted for medical publications.

## Installation
Install the development version from GitHub:

```r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("yotasama/ezmedpub")
```

## Basic Usage
### Example 1: Clean Numerical Data
```r
library(ezmedpub)

# Sample messy numerical data
messy_data <- data.frame(values = c("123", "45.6 kg", "78.9", "NA"))
clean_data <- num_simple_cleaning(messy_data$values)
print(clean_data)
```

### Example 2: Generate Baseline Table
```r
# Using example clinical data
baseline_table(data = example_clinical_data,
               variables = c("age", "gender", "bmi"),
               group = "treatment_group")
```

## Documentation
For detailed usage, refer to the package vignettes (coming soon) or the [GitHub repository](https://github.com/yotasama/ezmedpub).

## Contributing
Bug reports and feature requests are welcome via the [issue tracker](https://github.com/yotasama/ezmedpub/issues).

## License
`ezmedpub` is licensed under GPL (>= 3).