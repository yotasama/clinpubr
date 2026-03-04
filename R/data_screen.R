#' Screen and Join Multi-Table Clinical Data by Expression
#'
#' @description
#' One-call cohort screening pipeline with expression stages:
#' 1) entry stage: evaluate `entry_expr` and decide which keys enter downstream;
#' 2) anchor stage (optional): evaluate `anchor_expr` and keep records from first anchor onward;
#' 3) optional follow-up visit filtering;
#' 4) optional outer-join integration.
#'
#' `entry_expr` and `anchor_expr` support boolean combinations of grouped terms,
#' for example: `any(Hb > 10) & all(icd != "J18")` or
#' `mean(Hb, na.rm = TRUE) > 10 & any(icd == "I10")`.
#' `&` is applied as set intersection and `|` as set union on keys defined by level.
#'
#' @param data_list A named list of data frames. If `output = "joined"`, all tables
#'   will be outer-joined in the order of `data_list` after filtering.
#'   If `output = "list"`, tables are filtered but not joined.
#' @param entry_expr Entry expression for key selection. Supports grouped terminal
#'   expressions combined by `&`, `|`, and parentheses.
#' @param entry_level Granularity used to build entry keys: `"patient_id"`,
#'   `"visit_id"`, or `"date"`.
#' @param anchor_expr Optional anchor expression. Same grammar as `entry_expr`.
#' @param anchor_level Granularity used for anchor order: `"date"` or `"visit_id"`.
#' @param anchor_window Anchor window strategy: `"none"` or `"from_first_anchor"`.
#' @param patient_id_map Patient ID mapping. Either one default column name
#'   or a named vector by table.
#' @param visit_id_map Visit ID mapping. Either one default column name
#'   or a named vector by table.
#' @param date_map Date/order column mapping. Either one default column name
#'   or a named vector by table.
#' @param followup_min_visits Optional minimum number of distinct visits per patient.
#' @param followup_table Table used to count follow-up visits. Only used when
#'   `followup_min_visits` is not `NULL`. If missing, defaults to the first table
#'   that has both `patient_id` and `visit_id` mappings.
#' @param output Output format: `"list"` or `"joined"`.
#' @param return_audit Logical, whether to return audit logs.
#' @param verbose Logical, whether to print progress messages.
#'
#' @returns
#' If `return_audit = FALSE`, returns filtered list or joined data frame.
#' If `return_audit = TRUE`, returns a list with:
#' - `data`: filtered list or joined data frame
#' - `audit$entry_scope`: entry key scope application log
#' - `audit$anchor_scope`: anchor window application log
#' - `audit$followup`: follow-up filtering log
#' - `audit$join`: join step log
#'
#' @examples
#' patient <- data.frame(pid = 1:3)
#' admission <- data.frame(
#'   pid = c(1, 1, 2, 2, 3),
#'   vid = c(11, 12, 21, 22, 31),
#'   admit_day = c(1, 5, 2, 8, 3)
#' )
#' diagnosis <- data.frame(
#'   pid = c(1, 1, 2, 3),
#'   vid = c(11, 12, 21, 31),
#'   dx_day = c(1, 5, 2, 3),
#'   icd = c("I10", "I11", "I10", "J18")
#' )
#' lab <- data.frame(
#'   pid = c(1, 1, 2, 2, 3),
#'   vid = c(11, 12, 21, 22, 31),
#'   lab_day = c(1, 5, 2, 8, 3),
#'   Hb = c(9.8, 11.3, 10.8, 9.2, 8.6)
#' )
#'
#' # Scenario 1: any target diagnosis, keep all records of matched patients.
#' res_s1 <- screen_data_list(
#'   data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
#'   entry_expr = any(icd == "I10"),
#'   entry_level = "patient_id",
#'   patient_id_map = "pid",
#'   output = "list"
#' )
#'
#' # Scenario 2: any target diagnosis, keep diagnosis-index admission and after.
#' res_s2 <- screen_data_list(
#'   data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
#'   entry_expr = any(icd == "I10"),
#'   entry_level = "patient_id",
#'   anchor_expr = any(icd == "I10"),
#'   anchor_level = "date",
#'   anchor_window = "from_first_anchor",
#'   patient_id_map = "pid",
#'   visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
#'   date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
#'   output = "list"
#' )
#'
#' # Scenario 3: target diagnosis patients, then abnormal indicator visit and after.
#' res_s3 <- screen_data_list(
#'   data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
#'   entry_expr = any(icd == "I10"),
#'   entry_level = "patient_id",
#'   anchor_expr = any(Hb > 10),
#'   anchor_level = "date",
#'   anchor_window = "from_first_anchor",
#'   patient_id_map = "pid",
#'   visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
#'   date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
#'   output = "list"
#' )
#'
#' @export
screen_data_list <- function(data_list,
                             entry_expr,
                             entry_level = c("patient_id", "visit_id", "date"),
                             anchor_expr = NULL,
                             anchor_level = c("date", "visit_id"),
                             anchor_window = c("none", "from_first_anchor"),
                             patient_id_map,
                             visit_id_map = NULL,
                             date_map = NULL,
                             followup_min_visits = NULL,
                             followup_table = NULL,
                             output = c("list", "joined"),
                             return_audit = FALSE,
                             verbose = FALSE) {
  entry_level <- match.arg(entry_level)
  anchor_level <- match.arg(anchor_level)
  anchor_window <- match.arg(anchor_window)
  output <- match.arg(output)

  entry_expr <- substitute(entry_expr)
  anchor_expr <- if (missing(anchor_expr)) NULL else substitute(anchor_expr)
  if (!is.null(anchor_expr) && identical(anchor_expr, quote(NULL))) {
    anchor_expr <- NULL
  }

  .validate_data_list(data_list)
  if (is.null(entry_expr)) {
    stop("`entry_expr` is required")
  }

  patient_id_map <- .normalize_column_map(patient_id_map, data_list, "patient_id_map", required = TRUE)
  visit_id_map <- .normalize_column_map(visit_id_map, data_list, "visit_id_map", required = FALSE)
  date_map <- .normalize_column_map(date_map, data_list, "date_map", required = FALSE)

  if (output == "joined") {
    .check_join_key_uniqueness(
      data_list = data_list,
      patient_id_map = patient_id_map,
      visit_id_map = visit_id_map,
      date_map = date_map
    )
  }

  if (!is.null(followup_min_visits)) {
    if (is.null(followup_table)) {
      candidates <- names(data_list)
      if (!is.null(patient_id_map)) {
        candidates <- candidates[candidates %in% names(patient_id_map)]
      } else {
        candidates <- character()
      }
      if (!is.null(visit_id_map)) {
        candidates <- candidates[candidates %in% names(visit_id_map)]
      } else {
        candidates <- character()
      }

      if (length(candidates) == 0) {
        stop("No table has both patient_id and visit_id mappings for follow-up counting")
      }
      followup_table <- candidates[[1]]
    }

    if (!followup_table %in% names(data_list)) {
      stop("`followup_table` not found in `data_list`")
    }
  }

  entry_res <- .apply_entry_stage(
    data_list = data_list,
    entry_expr = entry_expr,
    entry_level = entry_level,
    patient_id_map = patient_id_map,
    visit_id_map = visit_id_map,
    date_map = date_map,
    verbose = verbose
  )
  filtered <- entry_res$data

  anchor_res <- .apply_anchor_stage(
    data_list = filtered,
    anchor_expr = anchor_expr,
    anchor_level = anchor_level,
    anchor_window = anchor_window,
    patient_id_map = patient_id_map,
    visit_id_map = visit_id_map,
    date_map = date_map,
    verbose = verbose
  )
  filtered <- anchor_res$data

  followup_res <- .apply_followup_filter(
    data_list = filtered,
    followup_min_visits = followup_min_visits,
    followup_table = followup_table,
    patient_id_map = patient_id_map,
    visit_id_map = visit_id_map,
    verbose = verbose
  )
  filtered <- followup_res$data

  join_log <- data.frame(
    step = integer(),
    table = character(),
    by = character(),
    before = integer(),
    after = integer(),
    stringsAsFactors = FALSE
  )

  if (output == "list") {
    result <- filtered
  } else {
    join_res <- .full_join_all(
      data_list = filtered,
      patient_id_map = patient_id_map,
      visit_id_map = visit_id_map,
      date_map = date_map,
      verbose = verbose
    )
    result <- join_res$data
    join_log <- join_res$log
  }

  audit <- list(
    entry_scope = entry_res$scope_log,
    anchor_scope = anchor_res$scope_log,
    followup = followup_res$log,
    join = join_log
  )

  if (return_audit) {
    return(list(data = result, audit = audit))
  }
  result
}

.validate_data_list <- function(data_list) {
  if (!is.list(data_list) || length(data_list) == 0) {
    stop("`data_list` must be a non-empty named list of data frames")
  }
  if (is.null(names(data_list)) || any(names(data_list) == "")) {
    stop("`data_list` must be a named list")
  }
  if (!all(vapply(data_list, is.data.frame, logical(1)))) {
    stop("All elements of `data_list` must be data frames")
  }
}

.normalize_column_map <- function(col_map, data_list, map_name, required) {
  if (is.null(col_map)) {
    if (required) {
      stop("`", map_name, "` is required")
    }
    return(NULL)
  }
  if (!is.character(col_map)) {
    stop("`", map_name, "` must be character")
  }

  if (length(col_map) == 1 && is.null(names(col_map))) {
    default_col <- col_map[[1]]
    hit_tables <- names(data_list)[vapply(data_list, function(df) default_col %in% names(df), logical(1))]
    if (required && length(hit_tables) == 0) {
      stop("Default column '", default_col, "' from `", map_name, "` not found in any table")
    }
    col_map <- stats::setNames(rep(default_col, length(hit_tables)), hit_tables)
  }
  if (is.null(names(col_map)) || any(names(col_map) == "")) {
    stop("`", map_name, "` must be a named character vector, or one default column name")
  }

  unknown <- setdiff(names(col_map), names(data_list))
  if (length(unknown) > 0) {
    stop("Unknown table names in `", map_name, "`: ", paste(unknown, collapse = ", "))
  }

  if (required) {
    missing_tbl <- setdiff(names(data_list), names(col_map))
    if (length(missing_tbl) > 0) {
      stop("`", map_name, "` is missing mappings for tables: ", paste(missing_tbl, collapse = ", "))
    }
  }

  for (tbl in names(col_map)) {
    if (!col_map[[tbl]] %in% names(data_list[[tbl]])) {
      stop("Column '", col_map[[tbl]], "' not found in table '", tbl, "' from `", map_name, "`")
    }
  }
  col_map
}

.check_join_key_uniqueness <- function(data_list, patient_id_map, visit_id_map, date_map) {
  problems <- character()

  for (tbl in names(data_list)) {
    key_cols <- c(
      patient_id_map[[tbl]],
      if (!is.null(visit_id_map) && tbl %in% names(visit_id_map)) visit_id_map[[tbl]] else NULL,
      if (!is.null(date_map) && tbl %in% names(date_map)) date_map[[tbl]] else NULL
    )
    key_cols <- unique(key_cols)
    key_cols <- key_cols[!is.na(key_cols) & nzchar(key_cols)]

    if (length(key_cols) == 0) {
      next
    }

    df <- data_list[[tbl]]
    if (nrow(df) == 0) {
      next
    }

    if (any(duplicated(df[, key_cols, drop = FALSE]))) {
      problems <- c(
        problems,
        paste0("table '", tbl, "' has non-unique join keys [", paste(key_cols, collapse = "+"), "]")
      )
    }
  }

  if (length(problems) > 0) {
    stop(
      "Join key uniqueness check failed before joining: ",
      paste(problems, collapse = "; "),
      ". Consider using output = 'list' instead of 'joined'."
    )
  }
}

.make_token <- function(...) {
  paste(..., sep = "::")
}

.extract_patient_from_token <- function(tokens) {
  sub("::.*$", "", tokens)
}

.split_token <- function(tokens) {
  parts <- strsplit(tokens, "::", fixed = TRUE)
  data.frame(
    patient_id = vapply(parts, `[`, character(1), 1),
    order_value = vapply(parts, `[`, character(1), 2),
    stringsAsFactors = FALSE
  )
}

.collect_level_tokens <- function(df, level, pid_col, vid_col = NULL, dcol = NULL) {
  if (nrow(df) == 0) {
    return(character())
  }
  if (level == "patient_id") {
    vals <- as.character(df[[pid_col]])
    vals <- vals[!is.na(vals)]
    return(unique(vals))
  }
  if (level == "visit_id") {
    if (is.null(vid_col)) {
      stop("`visit_id_map` is required for level 'visit_id'")
    }
    pid <- as.character(df[[pid_col]])
    vid <- as.character(df[[vid_col]])
    ok <- !is.na(pid) & !is.na(vid)
    return(unique(.make_token(pid[ok], vid[ok])))
  }
  if (is.null(dcol)) {
    stop("`date_map` is required for level 'date'")
  }
  pid <- as.character(df[[pid_col]])
  dd <- as.character(df[[dcol]])
  ok <- !is.na(pid) & !is.na(dd)
  unique(.make_token(pid[ok], dd[ok]))
}

.order_compare_ge <- function(left, right) {
  left_chr <- as.character(left)
  right_chr <- as.character(right)

  left_num <- suppressWarnings(as.numeric(left_chr))
  right_num <- suppressWarnings(as.numeric(right_chr))

  left_non_na <- !is.na(left_chr)
  right_non_na <- !is.na(right_chr)

  left_numeric_ok <- sum(left_non_na) == sum(!is.na(left_num))
  right_numeric_ok <- sum(right_non_na) == sum(!is.na(right_num))

  if (left_numeric_ok && right_numeric_ok) {
    return(left_num >= right_num)
  }
  left_chr >= right_chr
}

.order_min <- function(x) {
  x_chr <- as.character(x)
  x_chr <- x_chr[!is.na(x_chr)]
  if (length(x_chr) == 0) {
    return(NA_character_)
  }
  x_num <- suppressWarnings(as.numeric(x_chr))
  if (sum(!is.na(x_num)) == length(x_chr)) {
    return(as.character(min(x_num)))
  }
  min(x_chr)
}

.find_table_for_vars <- function(vars, data_list) {
  if (length(vars) == 0) {
    stop("Predicate in `any(...)` must reference at least one column")
  }

  hit <- vapply(names(data_list), function(tbl) all(vars %in% names(data_list[[tbl]])), logical(1))
  tables <- names(data_list)[hit]
  if (length(tables) == 0) {
    stop("No table contains all referenced columns: ", paste(vars, collapse = ", "))
  }
  if (length(tables) > 1) {
    stop("Columns are ambiguous across tables for predicate: ", paste(vars, collapse = ", "))
  }
  tables[[1]]
}

.token_vector_by_level <- function(df, level, pid_col, vid_col = NULL, dcol = NULL) {
  pid <- as.character(df[[pid_col]])
  if (level == "patient_id") {
    return(pid)
  }
  if (level == "visit_id") {
    if (is.null(vid_col)) {
      stop("`visit_id_map` is required for level 'visit_id'")
    }
    return(.make_token(pid, as.character(df[[vid_col]])))
  }
  if (is.null(dcol)) {
    stop("`date_map` is required for level 'date'")
  }
  .make_token(pid, as.character(df[[dcol]]))
}

.group_bool_any <- function(values, tokens) {
  tok_u <- unique(tokens)
  tok_f <- factor(tokens, levels = tok_u)
  hit <- rowsum(as.integer(values), tok_f, reorder = FALSE)[, 1] > 0
  tok_u[hit]
}

.group_bool_all <- function(values, tokens) {
  tok_u <- unique(tokens)
  tok_f <- factor(tokens, levels = tok_u)
  miss <- rowsum(as.integer(!values), tok_f, reorder = FALSE)[, 1] == 0
  tok_u[miss]
}

.extract_agg_spec <- function(term_expr) {
  if (!is.call(term_expr)) {
    return(NULL)
  }

  cmp <- as.character(term_expr[[1]])
  cmp_ops <- c(">", ">=", "<", "<=", "==", "!=")
  if (!cmp %in% cmp_ops || length(term_expr) != 3) {
    return(NULL)
  }

  lhs <- term_expr[[2]]
  rhs <- term_expr[[3]]

  is_agg_call <- function(x) {
    is.call(x) && as.character(x[[1]]) %in% c("mean", "sum", "min", "max") && length(x) >= 2
  }

  if (is_agg_call(lhs) && length(all.vars(rhs)) == 0) {
    return(list(cmp = cmp, agg = lhs, rhs = rhs, agg_on_left = TRUE))
  }
  if (is_agg_call(rhs) && length(all.vars(lhs)) == 0) {
    return(list(cmp = cmp, agg = rhs, rhs = lhs, agg_on_left = FALSE))
  }
  NULL
}

.eval_agg_by_token <- function(x, tokens, fun_name, na_rm) {
  tok_u <- unique(tokens)
  tok_f <- factor(tokens, levels = tok_u)

  if (fun_name == "sum") {
    x2 <- x
    if (na_rm) {
      x2[is.na(x2)] <- 0
    }
    out <- rowsum(x2, tok_f, reorder = FALSE)[, 1]
    if (!na_rm) {
      has_na <- rowsum(as.integer(is.na(x)), tok_f, reorder = FALSE)[, 1] > 0
      out[has_na] <- NA_real_
    }
    names(out) <- tok_u
    return(out)
  }

  if (fun_name == "mean") {
    x_sum <- x
    is_na <- is.na(x_sum)
    x_sum[is_na] <- 0
    sums <- rowsum(x_sum, tok_f, reorder = FALSE)[, 1]
    if (na_rm) {
      cnt <- rowsum(as.integer(!is_na), tok_f, reorder = FALSE)[, 1]
    } else {
      cnt <- rowsum(rep(1L, length(x_sum)), tok_f, reorder = FALSE)[, 1]
      has_na <- rowsum(as.integer(is_na), tok_f, reorder = FALSE)[, 1] > 0
    }
    out <- sums / cnt
    out[cnt == 0] <- NA_real_
    if (!na_rm) {
      out[has_na] <- NA_real_
    }
    names(out) <- tok_u
    return(out)
  }

  split_vals <- split(x, tok_f)
  fun <- match.fun(fun_name)
  out <- vapply(split_vals, function(z) {
    suppressWarnings(fun(z, na.rm = na_rm))
  }, numeric(1))
  out
}

.cmp_values <- function(lhs, rhs, op) {
  if (op == ">") return(lhs > rhs)
  if (op == ">=") return(lhs >= rhs)
  if (op == "<") return(lhs < rhs)
  if (op == "<=") return(lhs <= rhs)
  if (op == "==") return(lhs == rhs)
  lhs != rhs
}

.eval_term_tokens_fallback <- function(term_expr, df, tokens) {
  group_idx <- split(seq_along(tokens), tokens)
  keep_tokens <- character()

  for (tok in names(group_idx)) {
    idx <- group_idx[[tok]]
    sub_df <- df[idx, , drop = FALSE]
    val <- eval(term_expr, envir = sub_df, enclos = parent.frame())

    if (is.logical(val) && length(val) == 1) {
      keep <- isTRUE(val)
    } else if (is.logical(val) && length(val) == nrow(sub_df)) {
      stop("Terminal expression must return one logical value per group; wrap row-wise conditions with any()/all().")
    } else {
      stop("Terminal expression must return a single logical value per group.")
    }

    if (keep) {
      keep_tokens <- c(keep_tokens, tok)
    }
  }

  unique(keep_tokens)
}

.eval_term_tokens <- function(term_expr,
                              data_list,
                              level,
                              patient_id_map,
                              visit_id_map,
                              date_map) {
  vars <- all.vars(term_expr)
  tbl <- .find_table_for_vars(vars, data_list)
  df <- data_list[[tbl]]

  pid_col <- patient_id_map[[tbl]]
  vid_col <- if (!is.null(visit_id_map) && tbl %in% names(visit_id_map)) visit_id_map[[tbl]] else NULL
  dcol <- if (!is.null(date_map) && tbl %in% names(date_map)) date_map[[tbl]] else NULL

  tokens_raw <- .token_vector_by_level(df, level, pid_col, vid_col, dcol)
  ok <- !is.na(tokens_raw)
  if (!any(ok)) {
    return(character())
  }
  tokens <- tokens_raw[ok]
  df_ok <- df[ok, , drop = FALSE]

  if (is.call(term_expr) && as.character(term_expr[[1]]) %in% c("any", "all") && length(term_expr) == 2) {
    predicate <- term_expr[[2]]
    values <- eval(predicate, envir = df_ok, enclos = parent.frame())
    if (!is.logical(values) || length(values) != nrow(df_ok)) {
      stop("Predicate inside any()/all() must return logical vector with nrow(table) length")
    }
    values[is.na(values)] <- FALSE
    if (as.character(term_expr[[1]]) == "any") {
      return(.group_bool_any(values, tokens))
    }
    return(.group_bool_all(values, tokens))
  }

  agg_spec <- .extract_agg_spec(term_expr)
  if (!is.null(agg_spec)) {
    agg_call <- agg_spec$agg
    fun_name <- as.character(agg_call[[1]])

    x_expr <- agg_call[[2]]
    if (is.symbol(x_expr)) {
      x <- df_ok[[as.character(x_expr)]]
      na_rm <- FALSE
      if (length(agg_call) > 2 && "na.rm" %in% names(as.list(agg_call))) {
        na_rm <- isTRUE(eval(agg_call$na.rm, envir = parent.frame()))
      }

      agg_vals <- .eval_agg_by_token(x = x, tokens = tokens, fun_name = fun_name, na_rm = na_rm)
      rhs_val <- eval(agg_spec$rhs, envir = parent.frame())

      cmp_out <- if (agg_spec$agg_on_left) {
        .cmp_values(agg_vals, rhs_val, agg_spec$cmp)
      } else {
        .cmp_values(rhs_val, agg_vals, agg_spec$cmp)
      }
      keep <- !is.na(cmp_out) & cmp_out
      return(names(agg_vals)[keep])
    }
  }

  .eval_term_tokens_fallback(term_expr = term_expr, df = df_ok, tokens = tokens)
}

.eval_key_expr_tokens <- function(expr,
                                  data_list,
                                  level,
                                  patient_id_map,
                                  visit_id_map,
                                  date_map) {
  if (is.call(expr)) {
    op <- as.character(expr[[1]])
    if (op %in% c("&", "&&")) {
      left <- .eval_key_expr_tokens(expr[[2]], data_list, level, patient_id_map, visit_id_map, date_map)
      right <- .eval_key_expr_tokens(expr[[3]], data_list, level, patient_id_map, visit_id_map, date_map)
      return(intersect(left, right))
    }
    if (op %in% c("|", "||")) {
      left <- .eval_key_expr_tokens(expr[[2]], data_list, level, patient_id_map, visit_id_map, date_map)
      right <- .eval_key_expr_tokens(expr[[3]], data_list, level, patient_id_map, visit_id_map, date_map)
      return(union(left, right))
    }
    if (op == "(") {
      return(.eval_key_expr_tokens(expr[[2]], data_list, level, patient_id_map, visit_id_map, date_map))
    }
    return(.eval_term_tokens(expr, data_list, level, patient_id_map, visit_id_map, date_map))
  }
  stop("Expression supports grouped terminal expressions combined by `&`, `|`, and parentheses")
}

.keep_by_level <- function(df, level, pid_col, selected_patients, selected_tokens, vid_col = NULL, dcol = NULL) {
  if (nrow(df) == 0) {
    return(df)
  }

  pid <- as.character(df[[pid_col]])
  keep_patient <- pid %in% selected_patients

  if (level == "patient_id") {
    return(df[which(keep_patient), , drop = FALSE])
  }

  if (level == "visit_id") {
    if (is.null(vid_col)) {
      return(df[which(keep_patient), , drop = FALSE])
    }
    token <- .make_token(pid, as.character(df[[vid_col]]))
    keep <- keep_patient & token %in% selected_tokens
    return(df[which(keep), , drop = FALSE])
  }

  if (is.null(dcol)) {
    return(df[which(keep_patient), , drop = FALSE])
  }
  token <- .make_token(pid, as.character(df[[dcol]]))
  keep <- keep_patient & token %in% selected_tokens
  df[which(keep), , drop = FALSE]
}

.apply_entry_stage <- function(data_list,
                               entry_expr,
                               entry_level,
                               patient_id_map,
                               visit_id_map,
                               date_map,
                               verbose) {
  selected_tokens <- .eval_key_expr_tokens(
    expr = entry_expr,
    data_list = data_list,
    level = entry_level,
    patient_id_map = patient_id_map,
    visit_id_map = visit_id_map,
    date_map = date_map
  )

  scope_log <- data.frame(
    step = character(),
    table = character(),
    before = integer(),
    after = integer(),
    removed = integer(),
    stringsAsFactors = FALSE
  )

  if (length(selected_tokens) == 0) {
    filtered <- lapply(data_list, function(df) df[0, , drop = FALSE])
    for (tbl in names(data_list)) {
      scope_log <- rbind(scope_log, data.frame(
        step = "entry_scope",
        table = tbl,
        before = nrow(data_list[[tbl]]),
        after = 0,
        removed = nrow(data_list[[tbl]]),
        stringsAsFactors = FALSE
      ))
    }
    return(list(data = filtered, scope_log = scope_log))
  }

  selected_patients <- if (entry_level == "patient_id") selected_tokens else unique(.extract_patient_from_token(selected_tokens))

  filtered <- data_list
  for (tbl in names(filtered)) {
    pid_col <- patient_id_map[[tbl]]
    vid_col <- if (!is.null(visit_id_map) && tbl %in% names(visit_id_map)) visit_id_map[[tbl]] else NULL
    dcol <- if (!is.null(date_map) && tbl %in% names(date_map)) date_map[[tbl]] else NULL

    before <- nrow(filtered[[tbl]])
    filtered[[tbl]] <- .keep_by_level(
      df = filtered[[tbl]],
      level = entry_level,
      pid_col = pid_col,
      selected_patients = selected_patients,
      selected_tokens = selected_tokens,
      vid_col = vid_col,
      dcol = dcol
    )
    after <- nrow(filtered[[tbl]])

    scope_log <- rbind(scope_log, data.frame(
      step = "entry_scope",
      table = tbl,
      before = before,
      after = after,
      removed = before - after,
      stringsAsFactors = FALSE
    ))
    if (verbose) {
      message(sprintf("[entry_scope] %s: %d -> %d", tbl, before, after))
    }
  }

  list(data = filtered, scope_log = scope_log)
}

.apply_anchor_stage <- function(data_list,
                                anchor_expr,
                                anchor_level,
                                anchor_window,
                                patient_id_map,
                                visit_id_map,
                                date_map,
                                verbose) {
  scope_log <- data.frame(
    step = character(),
    table = character(),
    before = integer(),
    after = integer(),
    removed = integer(),
    stringsAsFactors = FALSE
  )

  if (is.null(anchor_expr) || anchor_window == "none") {
    return(list(data = data_list, scope_log = scope_log))
  }

  effective_level <- anchor_level
  if (anchor_level == "date" && is.null(date_map)) {
    warning("`date_map` is not provided. Falling back to visit_id ordering for anchor window.")
    effective_level <- "visit_id"
  }

  if (effective_level == "visit_id" && is.null(visit_id_map)) {
    stop("`visit_id_map` is required for anchor level 'visit_id'")
  }
  if (effective_level == "date" && is.null(date_map)) {
    stop("`date_map` is required for anchor level 'date'")
  }

  selected_tokens <- .eval_key_expr_tokens(
    expr = anchor_expr,
    data_list = data_list,
    level = effective_level,
    patient_id_map = patient_id_map,
    visit_id_map = visit_id_map,
    date_map = date_map
  )

  if (length(selected_tokens) == 0) {
    filtered <- lapply(data_list, function(df) df[0, , drop = FALSE])
    for (tbl in names(data_list)) {
      scope_log <- rbind(scope_log, data.frame(
        step = "anchor_scope",
        table = tbl,
        before = nrow(data_list[[tbl]]),
        after = 0,
        removed = nrow(data_list[[tbl]]),
        stringsAsFactors = FALSE
      ))
    }
    return(list(data = filtered, scope_log = scope_log))
  }

  token_df <- .split_token(selected_tokens)
  selected_patients <- unique(token_df$patient_id)
  index_order <- stats::aggregate(token_df$order_value, by = list(patient_id = token_df$patient_id), FUN = .order_min)
  names(index_order)[2] <- "index_order"

  filtered <- data_list
  index_patients <- as.character(index_order$patient_id)
  index_values <- as.character(index_order$index_order)

  for (tbl in names(filtered)) {
    pid_col <- patient_id_map[[tbl]]
    before <- nrow(filtered[[tbl]])

    dat <- filtered[[tbl]]
    dat$.tmp_pid <- as.character(dat[[pid_col]])
    dat <- dat[dat$.tmp_pid %in% selected_patients, , drop = FALSE]
    idx <- match(dat$.tmp_pid, index_patients)
    dat$index_order <- index_values[idx]

    ocol <- if (effective_level == "date") {
      if (!is.null(date_map) && tbl %in% names(date_map)) date_map[[tbl]] else NULL
    } else {
      if (!is.null(visit_id_map) && tbl %in% names(visit_id_map)) visit_id_map[[tbl]] else NULL
    }

    if (!is.null(ocol)) {
      keep <- !is.na(dat[[ocol]]) & !is.na(dat$index_order) & .order_compare_ge(dat[[ocol]], dat$index_order)
      dat <- dat[which(keep), , drop = FALSE]
    }

    filtered[[tbl]] <- dat[, setdiff(names(dat), c(".tmp_pid", "index_order")), drop = FALSE]
    after <- nrow(filtered[[tbl]])

    scope_log <- rbind(scope_log, data.frame(
      step = "anchor_scope",
      table = tbl,
      before = before,
      after = after,
      removed = before - after,
      stringsAsFactors = FALSE
    ))
    if (verbose) {
      message(sprintf("[anchor_scope] %s: %d -> %d", tbl, before, after))
    }
  }

  list(data = filtered, scope_log = scope_log)
}

.apply_followup_filter <- function(data_list,
                                   followup_min_visits,
                                   followup_table,
                                   patient_id_map,
                                   visit_id_map,
                                   verbose) {
  log <- data.frame(
    step = character(),
    table = character(),
    before = integer(),
    after = integer(),
    removed = integer(),
    stringsAsFactors = FALSE
  )

  if (is.null(followup_min_visits)) {
    return(list(data = data_list, log = log))
  }
  if (!is.numeric(followup_min_visits) || length(followup_min_visits) != 1 || followup_min_visits < 1) {
    stop("`followup_min_visits` must be a numeric value >= 1")
  }
  if (is.null(visit_id_map)) {
    stop("`visit_id_map` is required when `followup_min_visits` is set")
  }
  if (!followup_table %in% names(patient_id_map) || !followup_table %in% names(visit_id_map)) {
    stop("`patient_id_map` and `visit_id_map` must include `followup_table`")
  }

  fdat <- data_list[[followup_table]]
  pid_col <- patient_id_map[[followup_table]]
  vid_col <- visit_id_map[[followup_table]]

  tmp <- fdat[, c(pid_col, vid_col), drop = FALSE]
  names(tmp) <- c("patient_id", "visit_id")
  tmp <- tmp[!is.na(tmp$patient_id) & !is.na(tmp$visit_id), , drop = FALSE]
  if (nrow(tmp) == 0) {
    keep_patients <- character()
  } else {
    tmp$patient_id <- as.character(tmp$patient_id)
    tmp$visit_id <- as.character(tmp$visit_id)
    pairs <- unique(tmp)
    vis_tab <- table(pairs$patient_id)
    keep_patients <- names(vis_tab)[as.integer(vis_tab) >= followup_min_visits]
  }

  for (tbl in names(data_list)) {
    if (!tbl %in% names(patient_id_map)) {
      next
    }
    col_pid <- patient_id_map[[tbl]]
    before <- nrow(data_list[[tbl]])
    keep <- as.character(data_list[[tbl]][[col_pid]]) %in% keep_patients
    data_list[[tbl]] <- data_list[[tbl]][which(keep), , drop = FALSE]
    after <- nrow(data_list[[tbl]])
    log <- rbind(log, data.frame(step = "followup", table = tbl, before = before, after = after, removed = before - after, stringsAsFactors = FALSE))
    if (verbose) {
      message(sprintf("[followup] %s: %d -> %d", tbl, before, after))
    }
  }

  list(data = data_list, log = log)
}

.full_join_all <- function(data_list, patient_id_map, visit_id_map, date_map, verbose) {
  join_base <- names(data_list)[1]
  join_order <- setdiff(names(data_list), join_base)

  joined <- data_list[[join_base]]
  log <- data.frame(
    step = integer(),
    table = character(),
    by = character(),
    before = integer(),
    after = integer(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(join_order)) {
    tbl <- join_order[i]
    rhs <- data_list[[tbl]]

    by_used <- character()
    by_names <- character()

    key_specs <- list(
      patient_id = list(base = patient_id_map[[join_base]], rhs = patient_id_map[[tbl]]),
      visit_id = list(
        base = if (!is.null(visit_id_map) && join_base %in% names(visit_id_map)) visit_id_map[[join_base]] else NULL,
        rhs = if (!is.null(visit_id_map) && tbl %in% names(visit_id_map)) visit_id_map[[tbl]] else NULL
      ),
      date = list(
        base = if (!is.null(date_map) && join_base %in% names(date_map)) date_map[[join_base]] else NULL,
        rhs = if (!is.null(date_map) && tbl %in% names(date_map)) date_map[[tbl]] else NULL
      )
    )

    for (key_name in names(key_specs)) {
      lhs_col <- key_specs[[key_name]]$base
      rhs_col <- key_specs[[key_name]]$rhs
      if (is.null(lhs_col) || is.null(rhs_col)) {
        next
      }
      if (!lhs_col %in% names(joined) || !rhs_col %in% names(rhs)) {
        next
      }
      by_names <- c(by_names, lhs_col)
      by_used <- c(by_used, rhs_col)
    }

    if (length(by_used) == 0) {
      stop(
        "No semantic shared key found for join with table '", tbl,
        "'. Ensure id maps overlap on patient_id/visit_id/date."
      )
    }

    by_used <- stats::setNames(by_used, by_names)

    before <- nrow(joined)
    joined <- dplyr::full_join(joined, rhs, by = by_used)
    after <- nrow(joined)

    by_txt <- if (is.character(by_used) && is.null(names(by_used))) {
      paste(by_used, collapse = ",")
    } else {
      paste(paste(names(by_used), unname(by_used), sep = "="), collapse = ",")
    }

    log <- rbind(log, data.frame(
      step = i,
      table = tbl,
      by = by_txt,
      before = before,
      after = after,
      stringsAsFactors = FALSE
    ))

    if (verbose) {
      message(sprintf("[join] step %d %s: %d -> %d", i, tbl, before, after))
    }
  }

  list(data = joined, log = log)
}
