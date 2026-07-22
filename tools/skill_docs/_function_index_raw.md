# clinpubr 函数索引（原始数据）

> 自动生成，共 71 个导出函数
> 此文件包含完整 description，供构建精简索引时参考

- **`add_lists`** → [add_lists.md](add_lists.md)  
  Combine lists by adding element-wise.
- **`answer_check`** → [answer_check.md](answer_check.md)  
  Check answers of multiple choice questions by matching the answers with the correct sequence.
- **`auto_encoding_repair`** → [auto_encoding_repair.md](auto_encoding_repair.md)  
  Detects non-UTF-8 encoded characters and attempts to repair them. Uses base::validUTF8() for fast detection and stringi for conversion. This is useful for handling data imported from various sources with mixed encodings.
- **`baseline_table`** → [baseline_table.md](baseline_table.md)  
  Create a baseline table and a table of missing values. If the strata variable has more than 2 levels, a pairwise comparison table will also be created.
- **`break_at`** → [break_at.md](break_at.md)  
  Generate breaks for histogram that covers xlim and includes a ref_val.
- **`calc_cindex`** → [calc_cindex.md](calc_cindex.md)  
  Calculate C-index for survival data. It's a wrapper function for `Hmisc::rcorr.cens()`.
- **`calculate_index`** → [calculate_index.md](calculate_index.md)  
  Calculate an index based on multiple conditions. Each condition is evaluated and the result is weighted and summed to produce the final index.
- **`char_initial_cleaning`** → [value_initial_cleaning.md](value_initial_cleaning.md)  
  Cleaning illegal characters in string vectors that store numerical values. The function is useful for cleaning electrical health records in Chinese.  `char_initial_cleaning()` will convert full-width characters to half-width characters, removes whitespace at the start and end, replaces all internal whitespace with a single space, and replace empty strings with `NA`.  `value_initial_cleaning()` will additionally remove all spaces and extra dots.
- **`check_nonnum`** → [check_nonnum.md](check_nonnum.md)  
  Finds the elements that cannot be converted to numeric in a character vector. Useful when setting the strategy to clean numeric values.
- **`classif_model_compare`** → [classif_model_compare.md](classif_model_compare.md)  
  Compare the performance of classification models by commonly used metrics, and generate commonly used plots including receiver operating characteristic curve plot, decision curve analysis plot, and calibration plot.
- **`combine_files`** → [combine_files.md](combine_files.md)  
  combine multiple data files into a single data frame
- **`combine_multichoice`** → [combine_multichoice.md](combine_multichoice.md)  
  Combine multi-choice columns into one, each column consists of booleans whether a choice is presented.
- **`common_prefix`** → [common_prefix.md](common_prefix.md)  
  Get common prefix of a string vector
- **`cut_by`** → [cut_by.md](cut_by.md)  
  Divide numeric data into different groups. Easier to use than `base::cut()`.
- **`data_overview`** → [data_overview.md](data_overview.md)  
  This function provides a comprehensive overview of a data.frame, including variable types, summary statistics, and potential data quality issues. It serves as a starting point for data cleaning by identifying problems that need attention.
- **`detect_outliers`** → [detect_outliers.md](detect_outliers.md)  
  Detect outliers in a numeric vector using various methods.
- **`df_view_nonnum`** → [df_view_nonnum.md](df_view_nonnum.md)  
  Shows the non-numeric elements in a data frame. Only character columns are checked. Useful when setting the strategy to clean numeric values.
- **`emp_colors`** → [emp_colors.md](emp_colors.md)  
  default color palette for `clinpubr` plots
- **`exclusion_count`** → [exclusion_count.md](exclusion_count.md)  
  This function sequentially applies exclusion criteria to a data frame and counts the number of samples removed at each step.
- **`extract_history`** → [extract_history.md](extract_history.md)  
  Extract medical history information (disease, smoking, drinking, surgery, etc.) from clinical text records, now only supports Chinese text. The function uses vectorized string operations for high performance on large datasets.
- **`extract_num`** → [extract_num.md](extract_num.md)  
  Extract numerical values from strings. Can be used to filter out the unwanted information coming along with the numbers.
- **`fill_with_last`** → [fill_with_last.md](fill_with_last.md)  
  Fill NA values with the last valid value. Can be used to fill excel combined cells.
- **`filter_rcs_predictors`** → [filter_rcs_predictors.md](filter_rcs_predictors.md)  
  Filter predictors that can be used to fit for RCS models.
- **`first_mode`** → [first_mode.md](first_mode.md)  
  Calculate the first mode of a vector. Ignore NA values. Can be used if any mode is acceptable.
- **`format_pval`** → [format_pval.md](format_pval.md)  
  Format p-value with modified default settings suitable for publication.
- **`formula_add_covs`** → [formula_add_covs.md](formula_add_covs.md)  
  Add covariates to a formula. Support both formula and character string.
- **`get_samples`** → [get_samples.md](get_samples.md)  
  Generate a string summary of a vector by picking samples.
- **`get_valid`** → [get_valid.md](get_valid.md)  
  Extract one valid (non-NA) value from a vector.
- **`get_valid_subset`** → [get_valid_subset.md](get_valid_subset.md)  
  Get the subset of a data frame that satisfies the missing rate condition using a greedy algorithm.
- **`get_var_types`** → [get_var_types.md](get_var_types.md)  
  Automatic variable type and method determination for baseline table.
- **`group_by_range`** → [group_by_range.md](group_by_range.md)  
  Divide a numeric vector into groups such that the range (`max - min`) within each group does not exceed a given threshold. This is useful for clustering continuous values into contiguous bins controlled by a maximum span.
- **`group_by_range_cpp`** → [group_by_range_cpp.md](group_by_range_cpp.md)  
  Divide a sorted numeric vector into groups such that `max(x[group]) - min(x[group]) <= max_range`.
- **`importance_plot`** → [importance_plot.md](importance_plot.md)  
  Creates an importance plot from a named vector of values.
- **`indicate_duplicates`** → [indicate_duplicates.md](indicate_duplicates.md)  
  If an element is duplicated, all of its occurrence will be labeled `TRUE`. Useful to list and compare all duplicates.
- **`interaction_p_value`** → [interaction_p_value.md](interaction_p_value.md)  
  This function calculates the interaction p-value between a predictor and a group variable in a linear, logistic, or Cox proportional hazards model.
- **`interaction_plot`** → [interaction_plot.md](interaction_plot.md)  
  Plot interactions between variables. Both logistic and Cox proportional hazards regression models are supported. The predictor variables in the model are can be used both in linear form or in restricted cubic spline form.
- **`interaction_scan`** → [interaction_scan.md](interaction_scan.md)  
  Scan for interactions between variables and output results. Both logistic and Cox proportional hazards regression models are supported. The predictor variables in the model are can be used both in linear form or in restricted cubic spline form.
- **`iqr_outlier`** → [mad_outlier.md](mad_outlier.md)  
  Mark possible outliers in a numeric vector using various methods. These functions return a logical vector indicating which values are outliers.
- **`keep_by_keyword`** → [keep_by_keyword.md](keep_by_keyword.md)  
  Desensitize a character vector by removing the unneeded part of each string. The retained part is determined by keyword matches from a regular expression.
- **`mad_outlier`** → [mad_outlier.md](mad_outlier.md)  
  Mark possible outliers in a numeric vector using various methods. These functions return a logical vector indicating which values are outliers.
- **`max_missing_rates`** → [max_missing_rates.md](max_missing_rates.md)  
  Get the maximum missing rate of rows and columns.
- **`merge_by_range`** → [merge_by_range.md](merge_by_range.md)  
  Merge two data frames where shared keys in `by` must match exactly and the value in `y[[y_val]]` must fall within the range defined by `x[[x_start]]` and `x[[x_end]]`.  This function is particularly useful for date-based matching scenarios, where you need to match events (e.g., examinations, treatments) to time intervals (e.g., hospital admissions, visits). While the function accepts any ordered values (numeric, Date, POSIXt), date matching is the primary use case.  This avoids constructing the full Cartesian product that would be produced by a regular equality join followed by range filtering.  Merge two data frames where shared keys in `by` must match exactly and the value in `y[[y_val]]` must fall within the range defined by `x[[x_start]]` and `x[[x_end]]`.  This function is particularly useful for date-based matching scenarios, where you need to match events (e.g., examinations, treatments) to time intervals (e.g., hospital admissions, visits). While the function accepts any ordered values (numeric, Date, POSIXt), date matching is the primary use case.  This avoids constructing the full Cartesian product that would be produced by a regular equality join followed by range filtering.
- **`merge_by_substring`** → [merge_by_substring.md](merge_by_substring.md)  
  This function merges two data frames based on string key matching. It searches for keys from `key_df[[key_col]]` in `data[[search_col]]` and adds corresponding columns from `key_df` to `data`.
- **`merge_ordered_vectors`** → [merge_ordered_vectors.md](merge_ordered_vectors.md)  
  Merge multiple vectors into one while trying to maintain the order of elements in each vector. The relative order of elements is compared by their first occurrence in the vectors in the list. This function is useful when merging slightly different vectors, such as questionnaires of different versions.
- **`na_max`** → [na_max.md](na_max.md)  
  Instead of returning `-Inf` or `Inf`, returns `NA` if all values are `NA`. It also ignores `NA` values by default, which is different from base R functions. This is useful when summarizing data frames with `dplyr::summarise()`.
- **`na_min`** → [na_max.md](na_max.md)  
  Instead of returning `-Inf` or `Inf`, returns `NA` if all values are `NA`. It also ignores `NA` values by default, which is different from base R functions. This is useful when summarizing data frames with `dplyr::summarise()`.
- **`na2false`** → [na2false.md](na2false.md)  
  Replace `NA` values with `FALSE` in logical vectors. For other vectors, the behavior relies on R's automatic conversion rules.
- **`name2code`** → [vec2code.md](vec2code.md)  
  Generate the code that can be used to generate the string vector. `name2code()` is a wrapper of `vec2code(names(x))` to generate code for names of a vector, list, data frame, or any object with names.
- **`predictor_effect_plot`** → [predictor_effect_plot.md](predictor_effect_plot.md)  
  This is a versatile function to plot the relationship between a predictor variable and the outcome. It supports numeric (linear or RCS) and categorical predictors for logistic, linear, and Cox models. It can display the distribution of the predictor variable as a histogram (for numeric) or bar plot (for categorical).
- **`qq_show`** → [qq_show.md](qq_show.md)  
  QQ plot for a sample.
- **`rcs_plot`** → [rcs_plot.md](rcs_plot.md)  
  This function is a wrapper for `predictor_effect_plot` with `method = "rcs"`. It plots a restricted cubic spline for a predictor in a regression model.
- **`regression_basic_results`** → [regression_basic_results.md](regression_basic_results.md)  
  Generate the result table of logistic or Cox regression with different settings of the predictor variable and covariates. Also generate KM curves for Cox regression.
- **`regression_fit`** → [regression_fit.md](regression_fit.md)  
  This function fit the regression of a predictor in a linear, logistic, or Cox proportional hazards model.
- **`regression_forest`** → [regression_forest.md](regression_forest.md)  
  Generate the forest plot of logistic or Cox regression with different models.
- **`regression_scan`** → [regression_scan.md](regression_scan.md)  
  Scan for significant regression predictors and output results. Both logistic and Cox proportional hazards regression models are supported. The predictor variables in the model are can be used both in linear form or in restricted cubic spline form.
- **`replace_elements`** → [replace_elements.md](replace_elements.md)  
  Replacing elements in a vector
- **`screen_data_list`** → [screen_data_list.md](screen_data_list.md)  
  One-call cohort screening pipeline with expression stages:   entry stage: evaluate `entry_expr` and decide which keys enter downstream;  anchor stage (optional): evaluate `anchor_expr` and keep records from first anchor onward;  optional follow-up visit filtering;  optional outer-join integration.   `entry_expr` and `anchor_expr` support boolean combinations of grouped terms, for example: `any(Hb > 10) & all(icd != "J18")` or `mean(Hb, na.rm = TRUE) > 10 & any(icd == "I10")`. `&` is applied as set intersection and `|` as set union on keys defined by level.
- **`split_multichoice`** → [split_multichoice.md](split_multichoice.md)  
  Split multi-choice data into columns, each new column consists of booleans whether a choice is presented.
- **`str_match_replace`** → [str_match_replace.md](str_match_replace.md)  
  Partially match a string and replace with corresponding value. This function is useful to recover the original names of variables after legalized using `make.names` or modified by other functions.
- **`subgroup_forest`** → [subgroup_forest.md](subgroup_forest.md)  
  Create subgroup forest plot with `glm` or `coxph` models. The interaction p-values are calculated using likelihood ratio tests.
- **`subject_view`** → [subject_view.md](subject_view.md)  
  Get a table of subject details for the clinical data. This table could be labeled and used for subject name standardization.
- **`test_normality`** → [test_normality.md](test_normality.md)  
  Perform multiple normality tests on a numeric variable and determine if it follows normal distribution.
- **`time_roc_plot`** → [time_roc_plot.md](time_roc_plot.md)  
  Calculate time-dependent ROC curves using the `timeROC` package and plot them using `ggplot2`.
- **`to_date`** → [to_date.md](to_date.md)  
  Convert numerical (especially Excel date) or character date to date. Can deal with common formats and allow different formats in one vector.
- **`to_wide`** → [to_wide.md](to_wide.md)  
  Convert long-format data to wide format by grouping keys, using one column as item names and one column as values. When there are multiple values under the same key-item combination, values are reduced by `agg_fun`. Designed to convert long-format clinical data in database to wide format for analysis and publication.
- **`unit_standardize`** → [unit_standardize.md](unit_standardize.md)  
  Standardize units of numeric data, especially for data of medical records with different units.
- **`unit_view`** → [unit_view.md](unit_view.md)  
  Get a table of conflicting units for the clinical data, along with the some useful information, this table could be labeled and used for unit standardization.
- **`unmake_names`** → [unmake_names.md](unmake_names.md)  
  Inverse function of `make.names`. You can use `make.names` to make colnames legal for subsequent processing and analysis in R. Then use this function to switch back for publication.
- **`value_initial_cleaning`** → [value_initial_cleaning.md](value_initial_cleaning.md)  
  Cleaning illegal characters in string vectors that store numerical values. The function is useful for cleaning electrical health records in Chinese.  `char_initial_cleaning()` will convert full-width characters to half-width characters, removes whitespace at the start and end, replaces all internal whitespace with a single space, and replace empty strings with `NA`.  `value_initial_cleaning()` will additionally remove all spaces and extra dots.
- **`vec2code`** → [vec2code.md](vec2code.md)  
  Generate the code that can be used to generate the string vector. `name2code()` is a wrapper of `vec2code(names(x))` to generate code for names of a vector, list, data frame, or any object with names.
- **`zscore_outlier`** → [mad_outlier.md](mad_outlier.md)  
  Mark possible outliers in a numeric vector using various methods. These functions return a logical vector indicating which values are outliers.
