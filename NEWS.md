# clinpubr 1.1.0

* Added `time2` argument to functions that support cox models.

* Added `cluster` argument to functions to utilize robust Robust Covariance Matrix Estimates in cox models and Generalized Estimating Equations in glm models.

* Added `PRAUC` results and PR plot to `classif_model_compare()`.

* Refactored `rcs_plot()` and added a new `predictor_effect_plot()` function which can plot linear and categorical effects.

* Added `exclusion_count()` which counts the number of excluded samples at each exclusion criteria.

* Added `na_min()` and `na_max()`, similar to `base::min()` and `base::max()`, but return `NA` if all values are `NA` and omit `NA`s by default.

* Fixed `ggplot2` 4.0.0 compatibility.

* Minor improvements.

# clinpubr 1.0.2

* Moved some dependencies from "Imports" to "Suggests".

* Added `combine_multichoice()` which combines multichoice results.

* Added `common_prefix()` to find common prefixes in character vectors.

* Added `group` argument to `cut_by()` to cut data by quantiles with groupings.

* Added `char_initial_cleaning()` to clean character vectors.

* Added `indicate_duplicates()` to determine duplicate elements, including their first occurrence.

* Minor bug fixes and improvements.

# clinpubr 1.0.1

* Fixed potential colname conflict in `df_view_nonnum()`

* Modified code according to the CRAN cookbook.

# clinpubr 1.0.0

* Initial CRAN submission.
