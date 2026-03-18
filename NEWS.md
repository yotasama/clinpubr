# clinpubr 1.3.1

* Added `merge_by_range()` that merges data frames by keys and range matching. Can be used to merge data frames by time ranges, such as merging patient visits with exam results.
* Improved return type consistency of `first_mode()`.
* Minor bug fixes and improvements.

# clinpubr 1.3.0

* Added `screen_data_list()` that screens and joins multi-table clinical data by expression.
* Added `to_wide()` that converts long-format clinical data in database to wide format for analysis and publication.
* Added `keep_by_keyword()` to keep string segment by regex keyword position. Could be used to desensitize character vectors.
* Minor bug fixes and improvements.

# clinpubr 1.2.0

* Added `data_overview()` to get a quick overview of a data frame. It includes diagnostic information about common data issues and gives recommendations for data cleaning.
* Added several outlier detection methods: `mad_outlier()`, `zscore_outlier()`, `iqr_outlier()`.
* Added `merge_by_substring()` to merge data frames by substring matching.
* Added `recursive` argument to `combine_files()`.

# clinpubr 1.1.1

* Fixed PR plot visual inconsistency in `classif_model_compare()`.
* Added `remove_inequal` argument to `value_initial_cleaning()`.
* Changed the output of `vec2code()` to a more robust format.
* Added a wrapper of `vec2code()`, where `name2code(x)` is equivalent to `vec2code(names(x))`.
* Added `time_roc_plot()` that plot time-dependent ROC curve using `timeROC::timeROC()`.
* `baseline_table()` now uses invisible return if `save_table` is `TRUE`.
* No longer showing the default row ids in auto-saved csv tables.
* Minor bug fixes and improvements.

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
