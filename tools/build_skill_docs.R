#!/usr/bin/env Rscript
# build_skill_docs.R
# 从 clinpubr 包的 .Rd 文档重建 Markdown 格式的函数说明
# 每个导出函数生成一个独立的 .md 文件，作为技能使用的最终参考
#
# 用法:
#   Rscript build_skill_docs.R [输出目录] [包根目录]
#   默认输出目录: tools/skill_docs/（相对于包根目录）
#   默认包根目录: 脚本所在目录的上级

## ---- 参数解析 ----

cmd_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", cmd_args, value = TRUE)

if (length(script_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  script_dir <- dirname(script_path)
} else {
  script_dir <- getwd()
}

pkg_root <- "./"

trail_args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(trail_args) >= 1) {
  trail_args[1]
} else {
  file.path(pkg_root, "tools", "skill_docs")
}

pkg_root <- if (length(trail_args) >= 2) {
  normalizePath(trail_args[2], mustWork = FALSE)
} else {
  pkg_root
}

man_dir <- file.path(pkg_root, "man")
namespace_file <- file.path(pkg_root, "NAMESPACE")

cat("==== clinpubr .Rd → Markdown 文档生成器 ====\n")
cat("包根目录:", pkg_root, "\n")
cat("man 目录:", man_dir, "\n")
cat("输出目录:", output_dir, "\n\n")

if (!dir.exists(man_dir)) {
  stop("man 目录不存在: ", man_dir)
}

## ---- 读取导出函数列表 ----

ns_lines <- readLines(namespace_file)
export_fns <- sub("^export\\((.+)\\)$", "\\1", grep("^export\\(", ns_lines, value = TRUE))
export_fns <- trimws(export_fns)
cat("导出函数数量:", length(export_fns), "\n")

## ---- .Rd 解析辅助函数 ----

# 去掉 tag 开头的反斜杠
normalize_tag <- function(tag) {
  if (is.null(tag)) return("")
  sub("^\\\\", "", tag)
}

# 递归提取 Rd 元素中的所有文本，对部分 tag 做 markdown 转换
rd_to_md <- function(el) {
  if (is.character(el)) {
    return(paste(el, collapse = ""))
  }
  if (is.list(el)) {
    tag <- normalize_tag(attr(el, "Rd_tag"))
    # 递归处理子元素
    parts <- character(length(el))
    for (i in seq_along(el)) {
      parts[i] <- Recall(el[[i]])
    }
    content <- paste(parts, collapse = "")

    # 对部分内联 tag 做 markdown 包装
    if (tag == "code") {
      return(paste0("`", content, "`"))
    } else if (tag == "emph") {
      return(paste0("*", content, "*"))
    } else if (tag == "bold") {
      return(paste0("**", content, "**"))
    } else if (tag == "link" || tag == "linkS4class") {
      return(content)
    } else if (tag == "eqn") {
      return(paste0("$", content, "$"))
    } else if (tag == "deqn") {
      return(paste0("\n$$", content, "$$\n"))
    } else if (tag == "dots") {
      return("...")
    } else if (tag == "tab") {
      return("\t")
    } else if (tag == "cr") {
      return("\n")
    } else {
      return(content)
    }
  }
  return("")
}

# 提取某个 tag 的第一个匹配的文本
get_section <- function(rd, tag) {
  for (i in seq_along(rd)) {
    el <- rd[[i]]
    if (normalize_tag(attr(el, "Rd_tag")) == tag) {
      return(trimws(rd_to_md(el)))
    }
  }
  return("")
}

# 提取 \arguments 中的参数列表
get_arguments <- function(rd) {
  args_list <- list()
  for (i in seq_along(rd)) {
    el <- rd[[i]]
    if (normalize_tag(attr(el, "Rd_tag")) == "arguments") {
      for (j in seq_along(el)) {
        item <- el[[j]]
        if (normalize_tag(attr(item, "Rd_tag")) == "item") {
          arg_name <- trimws(rd_to_md(item[[1]]))
          arg_desc <- trimws(rd_to_md(item[[2]]))
          args_list[[length(args_list) + 1]] <- list(name = arg_name, desc = arg_desc)
        }
      }
    }
  }
  return(args_list)
}

# 提取 \seealso 的文本
get_seealso <- function(rd) {
  return(get_section(rd, "seealso"))
}

# 提取 \note 的文本
get_note <- function(rd) {
  return(get_section(rd, "note"))
}

# 提取 \references 的文本
get_references <- function(rd) {
  return(get_section(rd, "references"))
}

# 提取所有 \alias
get_aliases <- function(rd) {
  aliases <- character()
  for (i in seq_along(rd)) {
    el <- rd[[i]]
    if (normalize_tag(attr(el, "Rd_tag")) == "alias") {
      aliases <- c(aliases, trimws(rd_to_md(el)))
    }
  }
  return(aliases)
}

## ---- 转换为 Markdown 文档 ----

rd_to_markdown <- function(rd, fn_name) {
  title <- get_section(rd, "title")
  description <- get_section(rd, "description")
  usage <- get_section(rd, "usage")
  arguments <- get_arguments(rd)
  value <- get_section(rd, "value")
  details <- get_section(rd, "details")
  examples <- get_section(rd, "examples")
  seealso <- get_seealso(rd)
  note <- get_note(rd)
  references <- get_references(rd)
  aliases <- get_aliases(rd)

  lines <- c()
  lines <- c(lines, paste0("# `", fn_name, "()`"))
  lines <- c(lines, "")

  if (nchar(title) > 0) {
    lines <- c(lines, paste0("> **", title, "**"))
    lines <- c(lines, "")
  }

  if (length(aliases) > 1 || (length(aliases) == 1 && aliases[1] != fn_name)) {
    other_aliases <- setdiff(aliases, fn_name)
    if (length(other_aliases) > 0) {
      lines <- c(lines, paste0("**别名**: ", paste(paste0("`", other_aliases, "()`"), collapse = ", ")))
      lines <- c(lines, "")
    }
  }

  if (nchar(description) > 0) {
    lines <- c(lines, "## Description")
    lines <- c(lines, "")
    lines <- c(lines, description)
    lines <- c(lines, "")
  }

  if (nchar(usage) > 0) {
    lines <- c(lines, "## Usage")
    lines <- c(lines, "")
    lines <- c(lines, "```r")
    lines <- c(lines, usage)
    lines <- c(lines, "```")
    lines <- c(lines, "")
  }

  if (length(arguments) > 0) {
    lines <- c(lines, "## Arguments")
    lines <- c(lines, "")
    lines <- c(lines, "| 参数 | 说明 |")
    lines <- c(lines, "|------|------|")
    for (arg in arguments) {
      arg_name <- gsub("\n", " ", arg$name)
      arg_desc <- gsub("\n", " ", arg$desc)
      arg_desc <- gsub("\\|", "\\\\|", arg_desc)
      lines <- c(lines, paste0("| `", arg_name, "` | ", arg_desc, " |"))
    }
    lines <- c(lines, "")
  }

  if (nchar(value) > 0) {
    lines <- c(lines, "## Value")
    lines <- c(lines, "")
    lines <- c(lines, value)
    lines <- c(lines, "")
  }

  if (nchar(details) > 0) {
    lines <- c(lines, "## Details")
    lines <- c(lines, "")
    lines <- c(lines, details)
    lines <- c(lines, "")
  }

  if (nchar(note) > 0) {
    lines <- c(lines, "## Note")
    lines <- c(lines, "")
    lines <- c(lines, note)
    lines <- c(lines, "")
  }

  if (nchar(references) > 0) {
    lines <- c(lines, "## References")
    lines <- c(lines, "")
    lines <- c(lines, references)
    lines <- c(lines, "")
  }

  if (nchar(seealso) > 0) {
    lines <- c(lines, "## See Also")
    lines <- c(lines, "")
    lines <- c(lines, seealso)
    lines <- c(lines, "")
  }

  if (nchar(examples) > 0) {
    lines <- c(lines, "## Examples")
    lines <- c(lines, "")
    lines <- c(lines, "```r")
    lines <- c(lines, examples)
    lines <- c(lines, "```")
    lines <- c(lines, "")
  }

  return(paste(lines, collapse = "\n"))
}

## ---- 主流程 ----

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 清理旧文件
old_files <- list.files(output_dir, pattern = "\\.md$", full.names = TRUE)
if (length(old_files) > 0) {
  file.remove(old_files)
  cat("已清理旧文件:", length(old_files), "个\n")
}

rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
cat("找到 .Rd 文件:", length(rd_files), "个\n\n")

generated <- 0
skipped <- 0
failed <- 0
index_data <- list()
alias_map <- list()  # 记录 alias → 主文件名 的映射

for (rd_file in rd_files) {
  fn_name <- sub("\\.Rd$", "", basename(rd_file))

  tryCatch({
    rd <- tools::parse_Rd(rd_file)

    # 获取所有 alias
    aliases <- get_aliases(rd)
    if (length(aliases) == 0) {
      aliases <- fn_name
    }

    # 检查是否有任一 alias 在导出列表中
    exported_aliases <- intersect(aliases, export_fns)
    if (length(exported_aliases) == 0) {
      skipped <- skipped + 1
      next
    }

    # 主函数名：优先用文件名，如果文件名不在导出列表则用第一个导出的 alias
    primary_name <- if (fn_name %in% export_fns) fn_name else exported_aliases[1]

    md_content <- rd_to_markdown(rd, primary_name)
    out_file <- file.path(output_dir, paste0(primary_name, ".md"))
    writeLines(md_content, out_file)
    generated <- generated + 1

    # 为其他导出的 alias 创建符号链接文件（内容指向主文件）
    other_exported <- setdiff(exported_aliases, primary_name)
    for (alias in other_exported) {
      alias_file <- file.path(output_dir, paste0(alias, ".md"))
      alias_content <- c(
        paste0("# `", alias, "()`"),
        "",
        paste0("> 这是 [`", primary_name, "()`](", primary_name, ".md) 的别名，完整文档请查看主函数。"),
        "",
        md_content
      )
      writeLines(paste(alias_content, collapse = "\n"), alias_file)
      generated <- generated + 1
    }

    # 收集索引信息
    title <- get_section(rd, "title")
    description <- get_section(rd, "description")
    summary_text <- if (nchar(description) > 0) description else title
    summary_text <- gsub("\n", " ", summary_text)
    summary_text <- trimws(summary_text)

    for (alias in exported_aliases) {
      index_data[[alias]] <- list(
        file = paste0(primary_name, ".md"),
        title = title,
        description = summary_text
      )
    }

    cat("  生成:", primary_name, ".md")
    if (length(other_exported) > 0) {
      cat(" (+", length(other_exported), " 别名)", sep = "")
    }
    cat("\n")

  }, error = function(e) {
    failed <<- failed + 1
    cat("  失败:", fn_name, "—", conditionMessage(e), "\n")
  })
}

cat("\n==== 完成 ====\n")
cat("生成:", generated, "个 md 文件\n")
cat("跳过:", skipped, "个（非导出函数）\n")
cat("失败:", failed, "个\n")
cat("输出目录:", output_dir, "\n")

## ---- 生成临时索引文件（供后续构建精简索引使用）----

index_file <- file.path(output_dir, "_function_index_raw.md")
index_lines <- c(
  "# clinpubr 函数索引（原始数据）",
  "",
  paste0("> 自动生成，共 ", length(index_data), " 个导出函数"),
  "> 此文件包含完整 description，供构建精简索引时参考",
  ""
)

for (fn in sort(names(index_data))) {
  info <- index_data[[fn]]
  index_lines <- c(index_lines, paste0(
    "- **`", fn, "`** → [", info$file, "](", info$file, ")",
    "  \n  ", info$description
  ))
}

writeLines(index_lines, index_file)
cat("原始索引:", index_file, "\n")
