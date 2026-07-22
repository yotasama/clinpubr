test_that("extract_history handles basic negation and affirmation", {
  text <- c(
    "否认肝炎、结核、疟疾史，否认高血压、心脏病史，否认糖尿病，无手术史，无过敏史。",
    "既往有高血压病史20年，口服降压药控制良好。",
    "有阑尾炎切除手术史，否认高血压。",
    "无高血压、心脏病史，但有糖尿病病史。",
    "患者高血压病史、冠心病史，无吸烟史。",
    "高血压病史10年，吸烟史15年，每天10支，饮酒史5年。"
  )

  # Negation detection
  expect_equal(extract_history(text, "高血压", return_format = "simple"), c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(extract_history(text, "心脏病", return_format = "simple"), c(FALSE, NA, NA, FALSE, NA, NA))
  expect_equal(extract_history(text, "手术", return_format = "simple"), c(FALSE, NA, TRUE, NA, NA, NA))
  expect_equal(extract_history(text, "吸烟", return_format = "simple"), c(NA, NA, NA, NA, FALSE, TRUE))
  expect_equal(extract_history(text, "饮酒", return_format = "simple"), c(NA, NA, NA, NA, NA, TRUE))

  # Affirmation with duration
  res_detailed <- extract_history(text, "高血压", return_format = "detailed")
  expect_equal(res_detailed, c("no", "20 years", "no", "no", "yes", "10 years"))
})

test_that("extract_history handles pure mention without explicit affirmation", {
  text <- c(
    "患者高血压病史、冠心病史，无吸烟史。",
    "高血压病史10年，吸烟史15年。"
  )

  expect_equal(extract_history(text, "高血压", return_format = "simple"), c(TRUE, TRUE))
  expect_equal(extract_history(text, "冠心病", return_format = "simple"), c(TRUE, NA))
})

test_that("extract_history handles转折 (but/然而) correctly", {
  text <- "无高血压、心脏病史，但有糖尿病病史。"

  expect_equal(extract_history(text, "高血压", return_format = "simple"), FALSE)
  expect_equal(extract_history(text, "心脏病", return_format = "simple"), FALSE)
  expect_equal(extract_history(text, "糖尿病", return_format = "simple"), TRUE)
})

test_that("extract_history handles multiple keywords", {
  text <- c(
    "既往有高血压病史20年，口服降压药控制良好。",
    "无高血压、心脏病史，但有糖尿病病史。"
  )

  res <- extract_history(text, c("高血压", "糖尿病"), return_format = "simple")
  expect_equal(res$高血压, c(TRUE, FALSE))
  expect_equal(res$糖尿病, c(NA, TRUE))
})

test_that("extract_history handles named list keywords", {
  text <- c("既往有高血压病史20年。", "无吸烟史。", "否认糖尿病。")
  keywords <- list(
    hypertension = "高血压",
    smoking = "吸烟",
    diabetes = "糖尿病"
  )

  res <- extract_history(text, keywords, return_format = "simple")
  expect_equal(res$hypertension, c(TRUE, NA, NA))
  expect_equal(res$smoking, c(NA, FALSE, NA))
  expect_equal(res$diabetes, c(NA, NA, FALSE))
})

test_that("extract_history handles data.frame return format", {
  text <- "既往有高血压病史20年。"
  res <- extract_history(text, "高血压", return_format = "data.frame")

  expect_equal(res$status, TRUE)
  expect_equal(res$duration, "20 years")
})

test_that("extract_history handles duration unit conversion", {
  text <- "高血压病史10年，糖尿病史3月，肝炎史5周。"

  res_years <- extract_history(
    text, c("高血压", "糖尿病", "肝炎"),
    return_format = "data.frame", duration_unit = "years"
  )
  expect_equal(res_years$高血压_status[1], TRUE)
  expect_equal(res_years$高血压_duration[1], "10.00 years")
  expect_equal(res_years$糖尿病_status[1], TRUE)
  expect_equal(res_years$糖尿病_duration[1], "0.25 years")
  expect_equal(res_years$肝炎_status[1], TRUE)
  expect_true(!is.na(res_years$肝炎_duration[1]))

  res_days <- extract_history(
    text, c("高血压", "糖尿病"),
    return_format = "data.frame", duration_unit = "days"
  )
  expect_equal(res_days$高血压_status[1], TRUE)
  expect_equal(res_days$高血压_duration[1], "3652.5 days")
  expect_equal(res_days$糖尿病_status[1], TRUE)
  expect_equal(res_days$糖尿病_duration[1], "91.3 days")
})

test_that("extract_history handles NA and empty input", {
  text <- c("有高血压史。", NA, "否认高血压。", "")
  res <- extract_history(text, "高血压")
  expect_equal(res, c(TRUE, NA, FALSE, NA))

  expect_equal(extract_history(character(0), "高血压"), logical(0))
})

test_that("extract_history handles extract_duration = FALSE", {
  text <- "既往有高血压病史20年。"
  res <- extract_history(text, "高血压", extract_duration = FALSE, return_format = "data.frame")
  expect_equal(res$status, TRUE)
  expect_true(is.na(res$duration))
})

test_that("extract_history handles keywords with regex special chars", {
  text <- "患有C++相关疾病。"
  expect_equal(extract_history(text, "C++"), TRUE)
})

test_that("extract_history handles multiple keywords with return_format = data.frame", {
  text <- "高血压病史10年，吸烟史15年。"
  res <- extract_history(
    text, c("高血压", "吸烟"),
    return_format = "data.frame"
  )

  expect_equal(names(res), c("高血压_status", "高血压_duration", "吸烟_status", "吸烟_duration"))
  expect_equal(res$高血压_status, TRUE)
  expect_equal(res$高血压_duration, "10 years")
  expect_equal(res$吸烟_status, TRUE)
  expect_equal(res$吸烟_duration, "15 years")
})

test_that("extract_history handles no match case", {
  text <- c("患者无任何异常。", "身体健康。")
  res <- extract_history(text, "高血压")
  expect_true(all(is.na(res)))
})

test_that("extract_history prefers real duration over year-like numbers", {
  text <- "心脏病“病史10年，2015年开始使用恩格列净"
  expect_equal(extract_history(text, "心脏病", return_format = "detailed"), "10 years")

  text2 <- "高血压病史5年，2010年确诊。"
  expect_equal(extract_history(text2, "高血压", return_format = "detailed"), "5 years")
})