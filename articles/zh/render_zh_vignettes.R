#!/usr/bin/env Rscript
# render_zh_vignettes.R
# 批量渲染 clinpubr 中文 vignettes 为 HTML
# 使用方法: Rscript render_zh_vignettes.R

library(rmarkdown)

# 定义路径
vignette_dir <- "vignettes/zh"
output_dir <- "vignettes/zh"

# 获取所有 Rmd 文件
rmd_files <- list.files(vignette_dir, pattern = "\\.Rmd$", full.names = TRUE)

cat("=== clinpubr 中文 Vignettes 渲染脚本 ===\n\n")
cat("找到", length(rmd_files), "个 Rmd 文件:\n")
for (f in rmd_files) {
  cat("  -", basename(f), "\n")
}
cat("\n开始渲染...\n\n")

# 逐个渲染
success_count <- 0
fail_count <- 0

for (rmd_file in rmd_files) {
  rmd_name <- basename(rmd_file)
  html_name <- sub("\\.Rmd$", ".html", rmd_name)
  html_path <- file.path(output_dir, html_name)

  cat("渲染:", rmd_name, "... ")

  result <- tryCatch({
    rmarkdown::render(
      input = rmd_file,
      output_file = html_name,
      output_dir = output_dir,
      quiet = TRUE,
      encoding = "UTF-8"
    )
    cat("成功 ✓\n")
    cat("  输出:", html_path, "\n")
    success_count <<- success_count + 1
    TRUE
  }, error = function(e) {
    cat("失败 ✗\n")
    cat("  错误:", conditionMessage(e), "\n")
    fail_count <<- fail_count + 1
    FALSE
  })

  cat("\n")
}

# 汇总
cat("=== 渲染完成 ===\n")
cat("成功:", success_count, "个\n")
cat("失败:", fail_count, "个\n")
cat("输出目录:", normalizePath(output_dir), "\n")
