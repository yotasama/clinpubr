test_that("entry patient_id level keeps all records of matched patients", {
  patient <- data.frame(pid = 1:3, stringsAsFactors = FALSE)
  lab <- data.frame(
    pid = c(1, 1, 2, 3),
    vid = c(11, 12, 21, 31),
    item = c("Hb", "Hb", "Hb", "Hb"),
    value = c(11.2, 9.8, 10.5, 8.9),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, lab = lab),
    entry_filters = list(lab = ~ item == "Hb" & value > 10),
    entry_level = "patient_id",
    patient_id_map = "pid",
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$lab$pid)), c(1, 2))
})

test_that("entry visit_id level keeps matched visits and related data", {
  patient <- data.frame(pid = 1:2, stringsAsFactors = FALSE)
  admission <- data.frame(
    pid = c(1, 1, 2),
    vid = c(11, 12, 21),
    stringsAsFactors = FALSE
  )
  lab <- data.frame(
    pid = c(1, 1, 2),
    vid = c(11, 12, 21),
    item = c("Hb", "Hb", "Hb"),
    value = c(9.9, 10.8, 10.3),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, admission = admission, lab = lab),
    entry_filters = list(lab = ~ item == "Hb" & value > 10),
    entry_level = "visit_id",
    patient_id_map = "pid",
    visit_id_map = c(admission = "vid", lab = "vid"),
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$admission$vid)), c(12, 21))
  expect_equal(sort(unique(res$lab$vid)), c(12, 21))
})

test_that("entry date level keeps matched dates for date-mapped tables", {
  patient <- data.frame(pid = 1:2, stringsAsFactors = FALSE)
  lab <- data.frame(
    pid = c(1, 1, 2),
    test_day = c(1, 5, 2),
    item = c("Hb", "Hb", "Hb"),
    value = c(9.9, 10.8, 10.3),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, lab = lab),
    entry_filters = list(lab = ~ item == "Hb" & value > 10),
    entry_level = "date",
    patient_id_map = "pid",
    date_map = c(lab = "test_day"),
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$lab$test_day)), c(2, 5))
})

test_that("scenario 1: any target diagnosis then all records of matched patients", {
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
    entry_filters = list(diagnosis = ~ icd == "I10"),
    entry_level = "patient_id",
    patient_id_map = "pid",
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$admission$vid)), c(11, 12, 21, 22))
  expect_equal(sort(unique(res$lab$vid)), c(11, 12, 21, 22))
})

test_that("scenario 2: any target diagnosis then diagnosis-index admission and after", {
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
    entry_filters = list(diagnosis = ~ icd == "I10"),
    entry_level = "patient_id",
    anchor_filters = list(diagnosis = ~ icd == "I10"),
    anchor_level = "date",
    anchor_window = "from_first_anchor",
    patient_id_map = "pid",
    visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
    date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$admission$vid)), c(11, 12, 21, 22))
  expect_equal(sort(unique(res$lab$vid)), c(11, 12, 21, 22))
})

test_that("scenario 3: target diagnosis patients then abnormal indicator visit and after", {
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
    item = c("Hb", "Hb", "Hb", "Hb", "Hb"),
    value = c(9.8, 10.6, 10.7, 9.1, 8.6),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
    entry_filters = list(diagnosis = ~ icd == "I10"),
    entry_level = "patient_id",
    anchor_filters = list(lab = ~ item == "Hb" & value > 10),
    anchor_level = "date",
    anchor_window = "from_first_anchor",
    patient_id_map = "pid",
    visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
    date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
    output = "list"
  )

  expect_equal(sort(unique(res$patient$pid)), c(1, 2))
  expect_equal(sort(unique(res$admission$vid)), c(12, 21, 22))
  expect_equal(sort(unique(res$lab$vid)), c(12, 21, 22))
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
      entry_filters = list(diagnosis = ~ code %in% c("I10", "I11")),
      entry_level = "patient_id",
      anchor_filters = list(diagnosis = ~ code == "I11"),
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
    value = c(5, 6, 7),
    stringsAsFactors = FALSE
  )

  res <- screen_data_list(
    data_list = list(admission = admission, lab = lab),
    entry_filters = list(admission = ~ pid %in% c(1, 2)),
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
  admission <- data.frame(pid = c(1, 2), vid = c(11, 21), stringsAsFactors = FALSE)
  diagnosis <- data.frame(pid = c(1), vid = c(11), icd = c("I10"), stringsAsFactors = FALSE)

  res <- screen_data_list(
    data_list = list(admission = admission, diagnosis = diagnosis),
    entry_filters = list(admission = ~ pid %in% c(1, 2)),
    entry_level = "patient_id",
    patient_id_map = "pid",
    output = "joined",
    join_base = "admission",
    join_by = list(diagnosis = c(pid = "pid", vid = "vid")),
    return_audit = TRUE
  )

  expect_true(is.data.frame(res$data))
  expect_equal(nrow(res$data), 2)
  expect_true("icd" %in% names(res$data))
  expect_true(is.list(res$audit))
  expect_true("entry_filter" %in% names(res$audit))
})
