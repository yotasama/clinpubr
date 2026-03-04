test_that("entry expression supports cross-table any() with &", {
  patient <- data.frame(pid = 1:3, stringsAsFactors = FALSE)
  diagnosis <- data.frame(
    pid = c(1, 2, 2, 3),
    vid = c(11, 21, 21, 31),
    icd = c("I10", "I10", "I18", "J18"),
    stringsAsFactors = FALSE
  )
  lab <- data.frame(
    pid = c(1, 1, 2, 3),
    vid = c(11, 12, 21, 31),
    Hb = c(9.5, 11.2, 10.8, 8.6),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, diagnosis = diagnosis, lab = lab),
    entry_expr = any(Hb > 10) & any(icd == "I10"),
    entry_level = "patient_id",
    patient_id_map = "pid",
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$lab$pid)), c(1, 2))
  expect_equal(sort(unique(res$diagnosis$pid)), c(1, 2))
})

test_that("entry expression supports all() grouped condition", {
  patient <- data.frame(pid = 1:3, stringsAsFactors = FALSE)
  diagnosis <- data.frame(
    pid = c(1, 1, 2, 2, 3),
    vid = c(11, 12, 21, 22, 31),
    icd = c("I10", "E11", "I10", "J18", "I10"),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, diagnosis = diagnosis),
    entry_expr = all(icd == "I10" | icd == "E11"),
    entry_level = "patient_id",
    patient_id_map = "pid",
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 3))
})

test_that("entry expression supports mean(..., na.rm=TRUE) > threshold", {
  patient <- data.frame(pid = 1:3, stringsAsFactors = FALSE)
  lab <- data.frame(
    pid = c(1, 1, 2, 2, 3, 3),
    vid = c(11, 12, 21, 22, 31, 32),
    Hb = c(11.0, NA, 9.8, 9.9, 10.5, 10.7),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, lab = lab),
    entry_expr = mean(Hb, na.rm = TRUE) > 10,
    entry_level = "patient_id",
    patient_id_map = "pid",
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 3))
})

test_that("scenario 1: target diagnosis patients and all medical records", {
  patient <- data.frame(pid = 1:3, stringsAsFactors = FALSE)
  admission <- data.frame(
    pid = c(1, 1, 2, 2, 3),
    vid = c(11, 12, 21, 22, 31),
    admit_day = c(1, 5, 2, 8, 3),
    stringsAsFactors = FALSE
  )
  diagnosis <- data.frame(
    pid = c(1, 2, 3),
    vid = c(11, 21, 31),
    dx_day = c(1, 2, 3),
    icd = c("I10", "I10", "J18"),
    stringsAsFactors = FALSE
  )
  lab <- data.frame(
    pid = c(1, 1, 2, 2, 3),
    vid = c(11, 12, 21, 22, 31),
    lab_day = c(1, 5, 2, 8, 3),
    value = c(4.5, 5.0, 5.3, 4.7, 4.9),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
    entry_expr = any(icd == "I10"),
    entry_level = "patient_id",
    patient_id_map = "pid",
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$admission$vid)), c(11, 12, 21, 22))
  expect_equal(sort(unique(res$lab$vid)), c(11, 12, 21, 22))
})

test_that("scenario 2: target diagnosis patients then index admission and after", {
  patient <- data.frame(pid = 1:3, stringsAsFactors = FALSE)
  admission <- data.frame(
    pid = c(1, 1, 2, 2, 3),
    vid = c(11, 12, 21, 22, 31),
    admit_day = c(1, 5, 2, 8, 3),
    stringsAsFactors = FALSE
  )
  diagnosis <- data.frame(
    pid = c(1, 2, 3),
    vid = c(11, 22, 31),
    dx_day = c(1, 9, 3),
    icd = c("I10", "I10", "J18"),
    stringsAsFactors = FALSE
  )
  lab <- data.frame(
    pid = c(1, 1, 2, 2, 3),
    vid = c(11, 12, 21, 22, 31),
    lab_day = c(1, 5, 2, 8, 3),
    value = c(4.5, 5.0, 5.3, 4.7, 4.9),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
    entry_expr = any(icd == "I10"),
    entry_level = "patient_id",
    anchor_expr = any(icd == "I10"),
    anchor_level = "visit_id",
    anchor_window = "from_first_anchor",
    patient_id_map = "pid",
    visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
    date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$admission$vid)), c(11, 12, 22))
  expect_equal(sort(unique(res$lab$vid)), c(11, 12, 22))
})

test_that("scenario 3: target diagnosis patients then abnormal indicator and after", {
  patient <- data.frame(pid = 1:3, stringsAsFactors = FALSE)
  admission <- data.frame(
    pid = c(1, 1, 2, 2, 2, 3),
    vid = c(11, 12, 20, 21, 22, 31),
    admit_day = c(1, 5, 1, 2, 8, 3),
    stringsAsFactors = FALSE
  )
  diagnosis <- data.frame(
    pid = c(1, 2, 3),
    vid = c(11, 21, 31),
    dx_day = c(1, 2, 3),
    icd = c("I10", "I10", "J18"),
    stringsAsFactors = FALSE
  )
  lab <- data.frame(
    pid = c(1, 1, 2, 2, 2, 3),
    vid = c(11, 12, 20, 21, 22, 31),
    lab_day = c(1, 5, 1, 2, 8, 3),
    Hb = c(9.8, 10.6, 6, 10.7, 9.1, 8.6),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
    entry_expr = any(icd == "I10"),
    entry_level = "patient_id",
    anchor_expr = any(Hb > 10),
    anchor_level = "date",
    anchor_window = "from_first_anchor",
    patient_id_map = "pid",
    visit_id_map = "vid",
    date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$admission$vid)), c(12, 21, 22))
  expect_equal(sort(unique(res$lab$vid)), c(12, 21, 22))
})

test_that("scenario 3 joined output works", {
  admission <- data.frame(
    pid = c(1, 1, 2, 2, 2, 3),
    vid = c(11, 12, 20, 21, 22, 31),
    admit_day = c(1, 5, 1, 2, 8, 3),
    stringsAsFactors = FALSE
  )
  diagnosis <- data.frame(
    pid = c(1, 2, 3),
    vid = c(11, 21, 31),
    dx_day = c(1, 2, 3),
    icd = c("I10", "I10", "J18"),
    stringsAsFactors = FALSE
  )
  lab <- data.frame(
    pid = c(1, 1, 2, 2, 2, 3),
    vid = c(11, 12, 20, 21, 22, 31),
    lab_day = c(1, 5, 1, 2, 8, 3),
    Hb = c(9.8, 10.6, 6, 10.7, 9.1, 8.6),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(admission = admission, diagnosis = diagnosis, lab = lab),
    entry_expr = any(icd == "I10"),
    entry_level = "patient_id",
    anchor_expr = any(Hb > 10),
    anchor_level = "date",
    anchor_window = "from_first_anchor",
    patient_id_map = "pid",
    visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
    date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
    output = "joined"
  )

  expect_true(is.data.frame(res))
  expect_true(all(c("pid", "vid") %in% names(res)))
  expect_equal(sort(unique(res$pid)), c(1, 2))
  expect_equal(sort(unique(res$vid)), c(12, 21, 22))
})

test_that("anchor date falls back to visit ordering when date_map is missing", {
  patient <- data.frame(pid = c(1, 2), stringsAsFactors = FALSE)
  admission <- data.frame(
    pid = c(1, 1, 2),
    vid = c(11, 12, 21),
    stringsAsFactors = FALSE
  )
  diagnosis <- data.frame(
    pid = c(1, 2),
    vid = c(12, 21),
    code = c("I11", "I10"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    res <- screen_data_list(
      data_list = list(patient = patient, admission = admission, diagnosis = diagnosis),
      entry_expr = any(code %in% c("I10", "I11")),
      entry_level = "patient_id",
      anchor_expr = any(code == "I11"),
      anchor_level = "date",
      anchor_window = "from_first_anchor",
      patient_id_map = "pid",
      visit_id_map = c(admission = "vid", diagnosis = "vid"),
      output = "list"
    ),
    "Falling back to visit_id ordering"
  )

  expect_equal(unique(res$patient$pid), 1)
  expect_equal(sort(unique(res$admission$vid)), 12)
})

test_that("followup_min_visits works", {
  admission <- data.frame(
    pid = c(1, 1, 2),
    vid = c(11, 12, 21),
    stringsAsFactors = FALSE
  )
  lab <- data.frame(
    pid = c(1, 1, 2),
    vid = c(11, 12, 21),
    Hb = c(10.1, 10.8, 9.7),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(admission = admission, lab = lab),
    entry_expr = any(Hb > 9),
    entry_level = "patient_id",
    patient_id_map = "pid",
    visit_id_map = "vid",
    followup_min_visits = 2,
    followup_table = "admission",
    output = "list"
  )

  expect_equal(unique(res$admission$pid), 1)
  expect_equal(unique(res$lab$pid), 1)
})

test_that("joined output and return_audit both work", {
  admission <- data.frame(pid = c(1, 2), vid = c(11, 21), keep_flag = c(0, 1), stringsAsFactors = FALSE)
  diagnosis <- data.frame(pid = c(1), vid = c(11), icd = c("I10"), stringsAsFactors = FALSE)

  res <- screen_data_list(
    data_list = list(admission = admission, diagnosis = diagnosis),
    entry_expr = any(icd == "I10") | any(keep_flag == 1),
    entry_level = "patient_id",
    patient_id_map = "pid",
    visit_id_map = "vid",
    output = "joined",
    return_audit = TRUE
  )

  expect_true(is.data.frame(res$data))
  expect_equal(nrow(res$data), 2)
  expect_true("icd" %in% names(res$data))
  expect_true(is.list(res$audit))
  expect_true("entry_scope" %in% names(res$audit))
})
