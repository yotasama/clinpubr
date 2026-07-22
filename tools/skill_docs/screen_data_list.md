# `screen_data_list()`

> **Screen and Join Multi-Table Clinical Data by Expression**

## Description

One-call cohort screening pipeline with expression stages:

 entry stage: evaluate `entry_expr` and decide which keys enter downstream;
 anchor stage (optional): evaluate `anchor_expr` and keep records from first anchor onward;
 optional follow-up visit filtering;
 optional outer-join integration.


`entry_expr` and `anchor_expr` support boolean combinations of grouped terms,
for example: `any(Hb > 10) & all(icd != "J18")` or
`mean(Hb, na.rm = TRUE) > 10 & any(icd == "I10")`.
`&` is applied as set intersection and `|` as set union on keys defined by level.

## Usage

```r
screen_data_list(
  data_list,
  entry_expr,
  entry_level = c("patient_id", "visit_id", "date"),
  anchor_expr = NULL,
  anchor_level = c("date", "visit_id"),
  anchor_window = c("from_first_anchor", "none"),
  patient_id_map,
  visit_id_map = NULL,
  date_map = NULL,
  followup_min_visits = NULL,
  followup_table = NULL,
  output = c("list", "joined"),
  return_audit = FALSE,
  verbose = FALSE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data_list` | A named list of data frames. If `output = "joined"`, all tables will be outer-joined in the order of `data_list` after filtering. If `output = "list"`, tables are filtered but not joined. |
| `entry_expr` | Entry expression for key selection. Supports grouped terminal expressions combined by `&`, `\|`, and parentheses. |
| `entry_level` | Granularity used to build entry keys: `"patient_id"`, `"visit_id"`, or `"date"`. |
| `anchor_expr` | Optional anchor expression. Same grammar as `entry_expr`. |
| `anchor_level` | Granularity used for anchor order: `"date"` or `"visit_id"`. |
| `anchor_window` | Anchor window strategy: `"none"` or `"from_first_anchor"`. |
| `patient_id_map, visit_id_map, date_map` | Join key column mappings. Each can be either:   a single column name (character of length 1) that is used as the patient/visit/date ID column for all tables that contain it, or  a named vector where names are table names and values are column names specific to each table. |
| `followup_min_visits` | Optional minimum number of distinct visits per patient. |
| `followup_table` | Table used to count follow-up visits. Only used when `followup_min_visits` is not `NULL`. If missing, defaults to the first table that has both `patient_id` and `visit_id` mappings. |
| `output` | Output format: `"list"` or `"joined"`. If `"joined"`, all tables will be outer-joined after filtering, which works best when join keys are unique and tables are in "wide" format. |
| `return_audit` | Logical, whether to return audit logs. |
| `verbose` | Logical, whether to print progress messages. |

## Value

If `return_audit = FALSE`, returns filtered list or joined data frame.
If `return_audit = TRUE`, returns a list with:

 `data`: filtered list or joined data frame
 `audit$entry_scope`: entry key scope application log
 `audit$anchor_scope`: anchor window application log
 `audit$followup`: follow-up filtering log
 `audit$join`: join step log

## Examples

```r
patient <- data.frame(pid = 1:3)
admission <- data.frame(
  pid = c(1, 1, 2, 2, 3),
  vid = c(11, 12, 21, 22, 31),
  admit_day = c(1, 5, 2, 8, 3)
)
diagnosis <- data.frame(
  pid = c(1, 1, 2, 3),
  vid = c(11, 12, 21, 31),
  dx_day = c(1, 5, 2, 3),
  icd = c("I10", "I11", "I10", "J18")
)
lab <- data.frame(
  pid = c(1, 1, 2, 2, 3),
  vid = c(11, 12, 21, 22, 31),
  lab_day = c(1, 5, 2, 8, 3),
  Hb = c(9.8, 11.3, 10.8, 9.2, 8.6)
)

# Scenario 1: any target diagnosis, keep all records of matched patients.
res_s1 <- screen_data_list(
  data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
  entry_expr = any(icd == "I10"),
  entry_level = "patient_id",
  patient_id_map = "pid",
  output = "list"
)

# Scenario 2: any target diagnosis, keep diagnosis-index admission and after.
res_s2 <- screen_data_list(
  data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
  entry_expr = any(icd == "I10"),
  entry_level = "patient_id",
  anchor_expr = icd == "I10",
  anchor_level = "date",
  anchor_window = "from_first_anchor",
  patient_id_map = "pid",
  visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
  date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
  output = "list"
)

# Scenario 3: target diagnosis patients, then abnormal indicator visit and after.
res_s3 <- screen_data_list(
  data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
  entry_expr = any(icd == "I10"),
  entry_level = "patient_id",
  anchor_expr = Hb > 10,
  anchor_level = "date",
  anchor_window = "from_first_anchor",
  patient_id_map = "pid",
  visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
  date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
  output = "list"
)
```

