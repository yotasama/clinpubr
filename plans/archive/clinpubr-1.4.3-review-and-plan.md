# clinpubr 优化计划（1.4.4 单次发布）

> **本文是后续优化的唯一参考源。**所有问题描述、证据、修法、测试与验收标准都写在本文内，不依赖任何外部报告或对话。执行时以本文条目编号（P0-x / P1-x）为准；每完成一项，把该项状态改为 `已完成` 并注明 PR/commit。

---

## 0. 基线与约束

### 0.1 代码基线

| 项目 | 值 |
|---|---|
| `DESCRIPTION` 版本 | 1.4.3 |
| 主开发线 | Gitee `origin/develop`，HEAD = `7bf91cd`（"fix check error"） |
| GitHub | `github` / `upstream` 均为 `yotasama/clinpubr`，停在 1.4.0 |
| 导出函数 | 72 个（`NAMESPACE` 中 72 条 `export()`） |
| 测试 | 31 个 `test-*.R`，无 `helper-*.R` |
| 大文件行数 | `data_screen.R` 1087 / `merge.R` 1140 / `regressions.R` 875 / `misc.R` 626 |
| 依赖 | Imports 17 个（Rcpp、broom、car、data.table、DescTools、dplyr、fBasics、forestploter、ggplot2、Hmisc、rlang、rms、stringi、stringr、survival、survminer、tidyr）；Suggests 14 个（caret、dcurves、dtplyr、geepack、knitr、pROC、ResourceSelection、rmarkdown、rstatix、tableone、testthat、timeROC、vdiffr、withr）；`mice` 未出现在 DESCRIPTION |
| CI | `.github/workflows/{R-CMD-check,test-coverage}.yaml` 仅触发 `push: [main, master]` + `pull_request`；`rhub.yaml` 仅手动 |

**执行前必做**：`git fetch origin && git merge --ff-only origin/develop`。本地工作区可能落后一个 commit（`25e3729`），该 commit 之前 `create_formula()` 生成的是未限定名的 `Surv(...)` / `rcs(...)`，在只加载 clinpubr 的会话里 cox 模型会报 "没有 Surv 这个函数"。`7bf91cd` 已用 `survival::Surv` / `rms::rcs` 修复，不要回退这一改动。

### 0.2 三条不变量（任何改动都不得破坏）

1. **正确失败优先于静默错误**：宁可在入口处 `stop()` 并给出可操作的修复提示，也不允许产生"看起来正常但语义错误"的数据。
2. **静默路径必须可审计**：凡是跳过、剔除、降级、重标，必须 `warning()` 且写入返回对象的 `audit` / `diagnostics` / `scope_log`。
3. **统计输出可追溯**：读者能从输出表格区分"预设主分析"与"探索性尝试"。

### 0.3 版本目标

| 版本 | 主题 | 完成标准 |
|---|---|---|
| **1.4.4** | 正确性止血 + 统计语义 | 本文 P0 与 P1 全部完成；每条修复有"先失败、后通过"的测试；`R CMD check --as-cran` 0 error 0 warning；CI 覆盖 `develop` |

不分期：原 1.4.5 的统计语义项并入 1.4.4；原 1.5.0 的结构项（P2）移出本计划（见第 3 节）。1.4.4 定为一个"语义修正版"，NEWS 集中写迁移。

### 0.4 必要性评估结论

逐条核对代码后，只保留"会产生错误数据、错误统计量或必然崩溃"的改动；纯结构、纯命名、纯方法学口味项移除或降级为文档说明。

| 条目 | 判定 | 说明 |
|---|---|---|
| P0-1 | 保留，收窄 | 只去掉 `unit_view()` 的 `mapped` 列；`on_unmapped = "assume_target"` 按要求保留，作为旧行为的迁移开关 |
| P0-2 | 保留 | 净删约 500 行，行为不变 |
| P0-3 / P0-4 / P0-6 / P0-7 | 保留 | 均为静默错误或必然崩溃 |
| P0-5 | 保留，收窄 | `on_missing_anchor` 只留 `error` / `skip`，去掉与 `skip` 语义重叠的 `warn` |
| P0-8 | 保留（CI + 删草稿） | "GitHub/Gitee 二选一"只是发布决策，不占代码工作量 |
| P1-1 | 保留，收窄 | 三项真缺陷保留；`p.adjust` 跨族统一与 `best.var.trans` 改名 **只改文档**（见 D2/D4） |
| P1-2 … P1-6 | 保留 | 均为真实错误；P1-2 的 `test = "LRT"` 一项实测 `anova()` 接受，改为只补文档 |
| P2-1 … P2-5 | 移出 | 非正确性缺陷；API 分层与参数改名破坏下游，拆文件放大 diff 且不可审 |

---

## 1. P0：正确性缺陷（1.4.4）

### P0-1 `unit_standardize()` 静默重标单位

**状态**：已完成（1.4.4 工作区）

#### 症状

对同一 subject 只换算部分行，却把整组行的单位都改写成目标单位。结果是**数值保持原量级、单位变成目标单位**，后续所有按单位解释的计算（参考区间、剂量、单位换算）全部错，且没有 warning。

#### 复现

```r
df <- data.frame(subject = c("b", "b", "b"), value = c(10, 20, 30), unit = c("mL", "dL", "dL"))
unit_standardize(df, "subject", "value", "unit", list(list(subject = "b")))
#> 现状：unit 全为 "dL"，value 仍为 10/20/30 —— 10 mL 被标成了 10 dL
```

#### 根因

`R/unit_standardize.R:236-320`（`.unit_standardize_optimized`）先按行换算 `subject_values`，循环结束后执行 `units[subject_idx] <- target_unit`，把该 subject 的**所有**行（含未命中、含 `NA` 单位）统一改写：

```r
values[subject_idx] <- subject_values
units[subject_idx]  <- target_unit   # 第 313 行
```

同时 `R/unit_standardize.R:31-32` 的文档声称"`units2change` 缺省 = 除目标外全部"，实现里缺省时一行都不换算——文档与实现互相矛盾。

现有测试 `tests/testthat/test-unit_standardize.R:19-26` 把该错误固化成了期望值（`b` 的 `mL` 行数值不变、单位变 `dL`），必须替换而不是更新快照。

#### 设计决策

| 方案 | 结论 |
|---|---|
| 加参数"把所有未列出的单位都改成目标单位"，默认不动 | **不采纳**。缺系数时"全部改"只能默认系数 = 1，等于把当前静默 bug 改名保留；且默认"不动"会让结果继续存在非标准单位 |
| 拆成 `unit_relabel()` + `unit_convert()` 两个函数 | **不采纳**。真实数据往往两者混合，用户要调两次；且多一个导出函数 |
| **逐行写回 + 未映射默认报错 + `on_unmapped` 三值参数** | **采纳**。默认路径保证"要么每个 subject 只剩一个单位，要么明确失败"；`assume_target` 保留给需要旧行为的用户 |

#### 新 API

```r
unit_standardize(df, subject_col, value_col, unit_col, change_rules,
                 extract_numbers = FALSE,
                 on_unmapped = c("error", "warn", "assume_target"),
                 verbose = FALSE)
```

行级语义（四类互斥）：

| 行的单位 | 输出单位 | 输出数值 |
|---|---|---|
| `== target_unit` | `target_unit` | 不变 |
| ∈ `units2change`（有系数） | `target_unit` | `value * coeff` |
| ∈ `units2change`（系数为 `NA`，即 `units2remove`） | `NA` | `NA` |
| 其他（未映射） | 由 `on_unmapped` 决定 | 由 `on_unmapped` 决定 |

`on_unmapped`：

- `"error"`（默认）：扫完全部 subject 后**一次性** `stop()`，而不是逐行中断：

```
未映射的单位无法标准化：
  subject "alt"  unit "U/L"   23 行
  subject "alt"  unit ""       4 行
  subject "cr"   unit NA       8 行
请为这些单位在 change_rules 中提供换算系数，或标记为 "r" 删除；
若这些数值本身已是目标单位、只需统一标签，请设置 on_unmapped = "assume_target"。
```

- `"warn"`：同样的表降级为 warning，这些行保持原值与原单位（不做任何重标）。
- `"assume_target"`：把这些行标成 `target_unit`、数值不动（等价旧行为），并 warning 汇总被重标的 subject / unit / 行数。

附加规则：

1. `units2change` 非空但对应 `coeffs` 为空 → `stop()`（不再默认按 1 处理）。
2. 文档中"`units2change` 缺省 = 除目标外全部"改为"缺省 = 不换算"。
3. `label = "r"` 的行输出单位改为 `NA`（现在写 `target_unit`，等于给已删除的值贴单位）。
4. 同一 subject 只有一个单位时整组跳过（保持现状：不改标签、不报错）——它不存在"多单位冲突"，不需要标准化。
5. `assume_target` 是旧"只改标签"行为的唯一入口，必须 warning；默认路径不得退化成它。

#### 改动清单

| 文件 | 位置 | 动作 |
|---|---|---|
| `R/unit_standardize.R` | 90-123 | 增加 `on_unmapped` 参数并 `match.arg` |
| `R/unit_standardize.R` | 236-320 | 逐行写回；收集违规项；集中报错/warning；删除 `rule_lookup`、`target_units` 死代码（247-258） |
| `R/unit_standardize.R` | 204-234 | `coeffs` 缺失时 `stop()` |
| `R/unit_standardize.R` | 107-109、136-138 | 删除重复的"非数值列"warning（现在同一句打两遍） |
| `R/unit_standardize.R` | 157-162 | "全部 label 为 NA"时与其它路径一致：默认按未映射处理（走 `on_unmapped`），不再仅 warning 后原样返回 |
| `R/unit_standardize.R` | 31-32、58-89、335-354 | 修正文档与示例：示例 2 需补齐全部 `label` |

（`unit_view()` 的 `mapped` 列不在本次范围：默认报错后未映射项已经无法静默通过，再加一列只是重复信息。）

#### 测试

替换 `tests/testthat/test-unit_standardize.R:19-26`，新增：

```r
test_that("unmapped units fail by default", {
  df <- data.frame(subject = c("b", "b", "b"), value = c(10, 20, 30), unit = c("mL", "dL", "dL"))
  expect_error(unit_standardize(df, "subject", "value", "unit", list(list(subject = "b"))), "mL")
})

test_that("only converted rows are relabelled", {
  # 期望：dL 行换算并保持 dL；mL 行在 on_unmapped = "warn" 下仍是 mL/10
})

test_that("assume_target reproduces legacy relabel with a warning", {
  # 期望：单位全为 dL，数值不变，且必须有 warning
})

test_that("on_unmapped = 'warn' keeps original unit and value", {
  # 期望：mL 行仍是 mL/10，且 warning
})

test_that("removed units get NA unit and NA value", {
  # units2remove 的行：unit 为 NA、value 为 NA
})

test_that("rows not matched by rules are untouched", {
  # 属性测试：未命中行 unit/value 逐位相等；
  # 输出单位集合 ⊆ 输入单位 ∪ {target_unit, NA}
})
```

#### 迁移

NEWS 的 1.4.4 段落需写明：

```
* `unit_standardize()` 现在逐行写回单位，不再把未换算行的单位改写为目标单位。
  未在 `change_rules` 中列出的单位默认报错；如需保留旧的"只改标签"行为，
  请设置 `on_unmapped = "assume_target"`；如需保持原值与原单位并继续运行，
  请设置 `on_unmapped = "warn"`。
```

#### 验收

- 默认调用时，任何未映射单位都会失败，且错误信息包含全部违规 `subject/unit/行数`。
- `on_unmapped = "warn"` 时未映射行保持原单位与原值，且有 warning。
- `assume_target` 下有 warning，输出与旧版逐位一致。
- 属性测试通过：输出单位集合只可能是"输入单位 ∪ `{target_unit, NA}`"。

---

### P0-2 `merge.R` 内部函数重复定义

**状态**：已完成（1.4.4 工作区）

#### 事实

`R/merge.R` 中 7 个函数各出现两次，逐字相同，后一份覆盖前一份：

`.coerce_join_value`（132 / 637）、`.build_merge_group_key`（165 / 670）、`.na_like_df`（182 / 687）、`.build_clipped_ranges`（196 / 701）、`.merge_by_range_impl`（273 / 778）、`.merge_by_range_finalize`（347 / 852）、`merge_by_range`（512 / 1017）。

约 500 行重复。危险在于：**修改第一份不会生效**，且代码审查时容易误以为已经改过。

#### 改动

删除 637-1140 行（第二份完整拷贝，含第二个 `merge_by_range`），保留 1-636 行。删除后文件以第 635 行的 `merge_by_range` 结尾。

#### 测试与验收

- `tests/testthat/test-merge.R` 全部原样通过（行为不变）。
- `git diff --stat` 显示约 −500 行。
- 全局检查每个内部函数只出现一次：`grep -n "^\\.coerce_join_value <- function" R/merge.R` 只有 1 条。

> 先做这一项，再做 P0-3，可以让接下来的行为修改只改一处实现。

---

### P0-3 `merge_by_substring()` 空 key 与行数膨胀

**状态**：已完成（1.4.4 工作区）

#### 症状

1. `key_df` 中存在空字符串 key 时，`(?:)` 模式匹配**一切**，输入行被成倍复制且无提示。
2. 一个搜索值命中多个分组时，同一行出现多次，行数增加且无提示。
3. 输出行序被 `data.table::merge` 按 `search_col` 重排，输入行序静默丢失（现有测试已把排序结果写进断言）。

#### 复现

```r
df  <- data.frame(name = c("AB", "B,C", "A..", "ACD"), value = 1:4)
key <- data.frame(key = c("A", "B", "C", "ACD", "AB"),
                  category = c("cat1", "cat2", "cat3", "cat4", "cat1"))
merge_by_substring(df, key, "name", "key", "category")
#> 现状：4 行 → 8 行（设计允许），但按 name 排序，输入顺序丢失

key2 <- rbind(key, data.frame(key = "", category = "cat5"))
merge_by_substring(df, key2, "name", "key", "category")
#> 现状：4 行 → 12 行，所有行都命中 cat5，无任何提示
```

注意：`merge_by_substring` 的语义就是"一个名字可以属于多个分类"，所以**不能**禁止一对多。真正需要拦截的是"空 key 匹配一切"和"用户不知道行数变了"。

#### 改动

| 位置 | 动作 |
|---|---|
| `R/merge.R:63-76` | 提取 `keys_all <- key_df[[key_col]]`；若存在非 `NA` 但 `!nzchar(trimws())` 的 key → `stop("key_df 中存在空字符串 key，会匹配所有行：...")`；若全部为 `NA` → `stop()` |
| `R/merge.R:117-129` | `match_results` 后统计每个 search 值命中的组数：`dup_vals <- match_results[, .N, by = search_col][N > 1]`；若非空则 `warning()` 列出前 10 个值与总行数变化 |
| `R/merge.R:125-128` | `data.table::merge.data.table(..., sort = FALSE)` 保持输入行序 |
| roxygen | 明确写出"输出行数可能大于输入行数；一个搜索值命中多个分组时会产生多行；输出顺序与 `data` 一致" |

#### 测试

```r
test_that("empty string key is rejected",   { expect_error(merge_by_substring(df, key_empty, ...), "空字符串") })
test_that("multi-group match warns",        { expect_warning(merge_by_substring(df, key_overlap, ...), "命中") })
test_that("row order follows input",        { expect_equal(res$name, c("AB", "B,C", "A..", "ACD")[c(...)]) })
test_that("all-NA key is rejected",         { expect_error(...) })
```

需同步更新 `tests/testthat/test-merge.R:18`（当前断言按 `search_col` 排序）。

#### 验收

- 空 key、全 `NA` key 均报错。
- 命中多组时 warning 给出行数变化与示例值。
- 输出前 N 行的相对顺序与输入一致（多行匹配者相邻）。

---

### P0-4 `predictor_effect_plot()` 崩溃、参数校验与 PH 取值

**状态**：已完成（1.4.4 工作区）

#### 症状

1. `add_hist = FALSE` 或 `method = "categorical"` 且 `return_details = TRUE` 时，必然报 `找不到对象'df_hist_data'`。
2. `method` 传入非法值时，报"更换参数长度为零"这种不可读错误，且静默走到分类绘图分支。
3. `method = "linear"` 遇到因子，`median()` 返回 `NA`，后续参考值静默缺失。
4. PH p 值用位置索引 `table[1, 3]`，图上只标 "P-proportional"，没有说明它检验的是哪个变量。
5. `warning("...", e)` 把 condition 对象当格式化参数，输出难以阅读。

#### 复现

```r
d <- mtcars; d$am <- factor(d$am); d$vs <- factor(d$vs)
predictor_effect_plot(d, x = "wt", y = "vs", method = "linear", add_hist = FALSE, return_details = TRUE)
#> Error: 找不到对象'df_hist_data'
predictor_effect_plot(d, x = "am", y = "vs", method = "categorical", return_details = TRUE)
#> Error: 找不到对象'df_hist_data'
predictor_effect_plot(d, x = "wt", y = "vs", method = "bogus")
#> Error: 更换参数长度为零
```

#### 根因

- `R/predictor_effect_plot.R:251-255` 只在 `add_hist && method %in% c("rcs","linear")` 内给 `df_hist_data` 赋值；`R/predictor_effect_plot.R:403` 无条件读取。
- `R/predictor_effect_plot.R:143` 用 `phassump$table[1, 3]`。实测 `cox.zph()` 的行序为"各变量 + GLOBAL"，而 `create_formula()` 把 predictor 放在公式首位，所以第 1 行确实就是**该预测变量**的 PH 检验——这不是"取错行"，但代码与图注都没有说明这一点，且一旦公式结构变化就会静默取错。
- `R/predictor_effect_plot.R:145,152` 的 `warning(..., e)`。

#### 改动

```r
# 1) 入口
method <- match.arg(method, c("auto", "rcs", "linear", "categorical"))

# 2) 初始化
df_hist_plot <- NULL
df_hist_data <- NULL          # 与 df_hist_plot 同层

# 3) factor + linear
if (method == "linear" && is.factor(indf[[x]])) {
  stop(sprintf("`x = \"%s\"` 是因子，不能使用 method = \"linear\"；请用 \"categorical\"。", x))
}

# 4) PH：按行名取，两个值都返回
ph_table <- as.data.frame(phassump$table)
ph_row <- rownames(ph_table)[!rownames(ph_table) %in% "GLOBAL"][1]
pvalue_ph        <- ph_table[ph_row, "p"]          # 该预测变量
pvalue_ph_global <- ph_table["GLOBAL", "p"]        # 整体模型
```

图注改为 `P-proportional (变量名)`；`return_details = TRUE` 时额外返回 `pvalue_ph_global` 与 `ph_table`。

`warning("...", e)` 统一改为 `warning("...: ", conditionMessage(e))`。

#### 测试

| 用例 | 断言 |
|---|---|
| `add_hist = FALSE` + `return_details = TRUE` | 不报错；`n.plot` 为 `NULL` |
| `method = "categorical"` + `return_details = TRUE` | 不报错 |
| `method = "bogus"` | 报 `match.arg` 的标准错误 |
| `method = "linear"` + factor | 报包含变量名的错误 |
| cox 模型 | `pvalue_ph` 等于 `cox.zph()$table` 中该变量行的 p；`pvalue_ph_global` 等于 `GLOBAL` 行 |

#### 迁移

`return_details = TRUE` 的返回列表新增字段属于加法变更；图注文字变化写入 NEWS。

---

### P0-5 `screen_data_list()` 的 anchor 跳过与 join 行膨胀

**状态**：已完成（1.4.4 工作区）

#### 症状 1：缺映射时静默保留全部记录

`R/data_screen.R:857-866`：某张表没有配置 `date_map` / `visit_id_map` 对应列时，`ocol` 为 `NULL`，整段 anchor 过滤被跳过。该表保留这些患者的**全部**随访记录，不报错、不 warning，而 `scope_log` 仍然写入一条看起来"已处理"的 `anchor_scope` 记录（before/after 相同）。外部表现是：入组窗口限制对这张表完全失效。

#### 症状 2：join 用 key 交集，但唯一性只查单表自身

`R/data_screen.R:316-339` 的唯一性检查使用"该表自身已映射的 key 元组"；而实际 join 使用两表 key 的**交集**（`R/data_screen.R:1057-1058`）。例如 A 在 `(patient_id, visit_id)` 上唯一、B 在 `(patient_id, date)` 上唯一，交集只有 `patient_id`，`full_join(by = "patient_id")` 会静默产生 m×n 行。

#### 改动

1. 新增参数：

```r
screen_data_list(..., anchor_window = c("from_first_anchor", "none"),
                 on_missing_anchor = c("error", "skip"), ...)
```

- `"error"`（默认）：`stop()`，列出"表名 / 缺失的语义列（date 或 visit_id）/ 受影响患者数 / 受影响行数"。
- `"skip"`：不报错，但必须在 `scope_log` 中写入 `status = "skipped"` 与 `reason`（同时 `warning()` 一句，避免用户完全无感）。

不提供 `"warn"`：它与 `"skip"` 的唯一区别是控制台是否出声，而 `skip` 已经强制写入审计日志，多一个值只是多一条分支。

2. `scope_log` 增加两列：`status`（`applied` / `skipped` / `empty`）、`reason`（字符）。属于审计结构的加法变更，NEWS 说明。

3. join 前对**实际 `by_used`** 做双侧重复检测：

```r
dup_x <- anyDuplicated(joined[, by_used, drop = FALSE]) > 0
dup_y <- anyDuplicated(rhs[, by_used, drop = FALSE]) > 0
if (dup_x && dup_y) {
  stop(sprintf(
    "表 `%s` 与已合并结果在 join key (%s) 上同时存在重复，会产生 m×n 行膨胀。请补充更细的 key 或先去重。",
    tbl_name, paste(by_used, collapse = ", ")
  ))
}
```

`by_used` 用两表 key 的交集，且交集为空时直接 `stop()`。

#### 测试

```r
test_that("missing anchor mapping errors by default", {...})
test_that("skip records the reason in audit", {
  audit <- screen_data_list(..., on_missing_anchor = "skip", return_audit = TRUE)
  expect_true(any(audit$scope_log$status == "skipped"))
})
test_that("many-to-many join fails", {
  # A: (patient, visit) 唯一；B: (patient, date) 唯一
  expect_error(screen_data_list(..., output = "joined"), "行膨胀")
})
```

#### 验收

- 缺 date/visit 映射时默认报错，错误信息能定位到表与列。
- 显式 `skip` 时审计里能看到跳过原因。
- 只在 `patient_id` 上多对多的两表 join 必须失败。

---

### P0-6 `classif_model_compare()` 正类与边界条件

**状态**：已完成（1.4.4 工作区）

#### 症状

`R/classif_model_compare.R:137,151,153,206` 硬编码 `direction = "<"` 与 `positive = levels(target)[2]`。`factor()` 默认按字母序排列水平，若结局是 `"case"` / `"control"`，`levels = c("case","control")`，正类会被取成 `control`，AUC 与敏感度/特异度**整体反向**。

实测（n=200，真实 AUC≈0.77）：

```r
pROC::roc(y, pred, direction = ">")  # 0.773  ← 正类 = case 时应有的值
pROC::roc(y, pred, direction = "<")  # 0.227  ← 现状
```

#### 改动

1. 新增参数 `positive_class = NULL`：
   - 指定时必须 ∈ `levels(target)`，否则 `stop()`。
   - 未指定时取 `levels(target)[2]` 并 **warning** 说明"按因子水平顺序推断正类；建议显式指定 `positive_class`"。
2. 方向只由 `positive_class` 推导，所有下游统一使用：

```r
direction <- if (identical(positive_class, levels(target)[1])) ">" else "<"
```

   影响点：`pROC::roc`、`pROC::ci.auc`、`caret::confusionMatrix(positive = ...)`、Brier/HL 的目标重编码（`as.numeric(target == positive_class)`）、DCA。
3. 预测列含 `NA` → 在范围校验之前 `stop()`，信息包含模型名与前 5 个行号。
4. 校准分箱：`n_tiles <- min(max(round(nrow(data) / 10), 1), 20)`；当 `nrow(data) < 20` 时 `warning("样本量不足，已跳过校准曲线")` 并把校准相关列置 `NA`（不引入留一法）。
5. PR 曲线与 PRAUC 共用一份清理后的 `(recall, precision)`：

```r
.pr_points <- function(roc_res) {
  p <- pROC::coords(roc_res, "all", ret = c("precision", "recall"))
  p <- as.data.frame(p)
  p[is.finite(p$precision) & is.finite(p$recall), , drop = FALSE]
}
```

   删掉 `R/classif_model_compare.R:230` 单独把 `NaN` 置 1 的分支。

#### 测试

| 用例 | 断言 |
|---|---|
| 水平 `c("case","control")` + `positive_class = "case"` | AUC 与手工 `pROC` 一致（≈0.77），敏感度对应 case |
| 同上 + `positive_class = "control"` | AUC = 1 − 前值 |
| 未指定 `positive_class` | 有 warning，且使用的是 `levels[2]` |
| 预测列含 `NA` | 报错含模型名与行号 |
| 常量预测 | 报错 |
| `n = 10` | 不崩，校准列为 `NA` + warning |

#### 迁移

NEWS 写明：默认正类仍是 `levels(target)[2]`（与旧版一致），但会 warning；`case/control` 结局建议显式传 `positive_class`。

---

### P0-7 `extract_history()` 空关键词与转义错误

**状态**：已完成（1.4.4 工作区）

#### 症状 1：空关键词匹配一切

`R/extract_history.R:147-149`：

```r
build_keyword_pattern <- function(keyword) {
  paste0("(?:", paste(escape_keyword(keyword), collapse = "|"), ")")
}
```

`keyword = ""` → `"(?:)"`，匹配任意文本；实测 `stri_detect_regex(c("任意文本","abc",""), "(?:)")` 全为 `TRUE`。结果是"历史提取"把整列都判为阳性，且无任何提示。

#### 症状 2：转义顺序错误，含 `.` 的关键词匹配不到自己

`R/extract_history.R:204-211` 用 `stri_replace_all_fixed(..., vectorize_all = FALSE)` 顺序替换：先把 `.` 换成 `\.`，随后又把所有反斜杠翻倍成 `\\`，最终 `"a.b"` 变成 `"a\\.b"`，在正则层面表示字面量 `a\` + 任意字符 + `b`——**匹配自己返回 FALSE**（实测）。

#### 改动

```r
# 入口：keyword 校验
if (length(keyword) == 0) stop("`keyword` 不能为空向量。")
if (any(!nzchar(trimws(keyword)))) {
  stop("`keyword` 含空字符串，会匹配所有文本，请删除空关键词。")
}

# 转义：一次正则替换（已验证可正确匹配自身）
escape_keyword <- function(keyword) {
  stringi::stri_replace_all_regex(keyword, "([\\[\\]{}()\\-+*?.\\\\^$|])", "\\\\$1")
}
```

验证结果（6 组关键词全部能匹配自身）：`a.b` → `a\\.b`，`x(y)` → `x\\(y\\)`，`C++` → `C\\+\\+`，`0.5-1` → `0\\.5\\-1`，`A|B` → `A\\|B`，`a\b` → `a\\\\b`。

#### 否定判断的最小修正

`R/extract_history.R:187-194`：前向窗口的排除集是 `[^，,。；但\n\r]`，后向窗口是 `[^，,。；\n\r]`（少了"但"），两个方向不一致。改为共用同一个边界字符类，并把 `negation_window` 写进文档：

```r
boundary <- "[^，,。；但\\n\\r]"
neg_words <- "否认|没有|未见|否定|未|无"
neg_pre  <- sprintf("(?:%s)%s{0,%d}%s", neg_words, boundary, window, keyword_pattern)
neg_post <- sprintf("%s%s{0,%d}(?:%s)", keyword_pattern, boundary, window, neg_words)
```

测试样例集（需写入回归测试）：

| 文本 | 关键词 | 期望 |
|---|---|---|
| `"未使用降压药但高血压病史"` | `高血压` | `TRUE`（"但"分句，不否定） |
| `"否认高血压"` | `高血压` | `FALSE` |
| `"无明显诱因"` | `诱因` | 见下方说明 |
| `"a.b 阳性"` | `a.b` | `TRUE`（转义修复） |
| `""` | 任意 | 报错（空关键词） |

`"无明显诱因"` 属于真实假阴性场景，但需要短语级词表（"无明显诱因""未见异常"），不属于本次止血范围；在文档中列入已知限制，后续版本再评估。

#### 测试

- 空关键词 → 报错，且信息说明原因。
- 含 `.` `(` `+` `|` 的关键词能匹配自身。
- 否定词跨句（逗号/句号/分号/但）不被误判。

---

### P0-8 发布与 CI 基线

**状态**：已完成（1.4.4 工作区）

#### 事实

- `.github/workflows/R-CMD-check.yaml:3-6`、`test-coverage.yaml:3-6` 只触发 `[main, master]`，开发线 `develop` 完全没有 CI。
- 没有"安装全部 Suggests"的 job，`skip_if_not_installed()` 会掩盖主路径问题。
- 仓库根目录有 28.9 KB 的 `my_test.r` 开发草稿，未被 `Collate` 引用。
- GitHub / upstream 停在 1.4.0，Gitee 已是 1.4.3。

#### 改动

```yaml
on:
  push:
    branches: [main, master, develop]
  pull_request:
```

- 新增一个 job：`R CMD check` 前显式安装全部 `Suggests`（`dependencies: '"hard"'` + `extra-packages` 或 `pak::pak(c("caret","dcurves","dtplyr","geepack","pROC","ResourceSelection","rstatix","tableone","timeROC","vdiffr"))`），确保可选依赖路径被真实执行。
- 删除 `my_test.r`。
- 确认 `develop` 为唯一开发线（Gitee 或 GitHub 二选一，另一个只做镜像），README 标注镜像关系。

#### 验收

- 向 `develop` push 后 CI 自动运行。
- 全 Suggests job 通过；若失败，视为真实缺陷修复而不是跳过。

---

## 2. P1：统计语义（1.4.4）

### P1-1 `regression_scan()`：协变量状态泄漏与趋势检验

**状态**：已完成（1.4.4 工作区）

| 问题 | 位置 | 改动 |
|---|---|---|
| `covars` 在 predictor 循环内被永久改写，导致不同 predictor 使用不同协变量集 | `R/regressions.R:657` | 改为循环内局部变量：`covars_i <- remove_conflict(covars, ...)`，并把第 692-694 行的 `covars` 全部替换为 `covars_i` |
| 名义（无序）因子也输出 "P for trend" | `R/regressions.R:184-189`、`R/regressions.R:312`、`R/regressions.R:343-355` | 仅当 `is.ordered()` 时输出 trend 行；trend 计算用 `as.numeric` 前必须确认是有序因子 |
| `suppressWarnings()` 包裹整个拟合，完全分离/收敛失败被吞 | `R/regressions.R:840-873` | 去掉包裹；`fit_model()` 返回对象增加 `attr(, "diagnostics")`；`regression_scan` 把诊断写入 `diagnostics` 列 |
| `p.adjust` 在每个变换族内分别执行 | `R/regressions.R:710-716` | **只改文档**：在 `@details` 与 vignette 写明"族内校正（original / logarithm / categorized / rcs 各自一族），跨族不可直接比较"（见 D4） |
| `best.var.trans` 由未校正 p 最小值选出 | `R/regressions.R:717-721` | **只改文档**：保留列名，写明"仅供探索，不可用于推断"（见 D2；改名会破坏下游列名且不修正任何数值） |
| 不可达的 GEE 分支 | `R/regressions.R:792`、`829-831`、`855-862` | `cluster` 非空必然 `rms = TRUE`，删除 GEE 分支；`cluster` 文档统一写成"稳健方差（`rms::robcov`）" |
| `sum(!is.na(x) > 0)` 写法绕 | `R/regressions.R:719` | 结果与 `sum(!is.na(x))` 等价（逻辑值比较后求和仍是计数），**只做可读性改写**：`sum(!is.na(unlist(p_table[i, ]))) > 0` |

**验收**：同一数据重复运行结果逐位一致；构造完全分离数据时结果表出现诊断信息而非静默 NA；3 水平名义因子不再出现 trend 行；文档写清族内校正与 `best.var.trans` 的探索性质。

---

### P1-2 `interactions`：对称重复与静默失败

**状态**：已完成（1.4.4 工作区）

| 问题 | 位置 | 改动 |
|---|---|---|
| `age×sex` 与 `sex×age` 各占一行，且都进入 `p.adjust` | `R/interactions.R:58-60`、`R/interactions.R:98` | 增加 `seen` 集合，key 用 `paste(sort(c(predictor, group_var)), collapse = "\r")`，重复对跳过；`p.adjust` 只对去重后的行执行 |
| 交互检验失败返回 `NA` 但无 warning | `R/interactions.R:71-73`、`81-83` | `error = function(e) { warning("... predictor/group ...: ", conditionMessage(e)); NA }` |
| 线性模型也传 `test = "LRT"` | `R/interactions.R:426`、`430` | **只改文档**：实测 `anova()` 对 `lm` 接受 `"LRT"` 并给出 Chisq p（`"bogus"` 才会报错），不是缺陷；文档说明"线性模型用似然比检验，小样本偏宽松" |

**验收**：`predictors = c("age","sex")`、`group_vars = c("age","sex")` 时只产生一对；失败时有 warning 且信息含变量名。

---

### P1-3 `calc_cindex()` / `time_roc_plot()`：不要静默翻转指标

**状态**：已完成（1.4.4 工作区）

| 问题 | 位置 | 改动 |
|---|---|---|
| C-index < 0.5 自动改成 `1 - c` | `R/time_roc.R:20-23` | 删除翻转，改为 `warning("C-index < 0.5，可能方向相反，请确认 marker 与事件的关系")`，返回原始值 |
| `total_tp == 0` 时除零 | `R/time_roc.R:103-108` | 跳过该时间点并 `warning()`，不生成 `NaN/Inf` 曲线 |

**验收**：反向 marker 返回真实 C-index 且有 warning；`times` 含无事件时间点时曲线只包含有效时间点。

---

### P1-4 `tidy.rms()`：注册而不是删除

**状态**：已完成（1.4.4 工作区）

#### 事实（重要）

`R/misc.R:598-626` 定义了 `tidy.rms()` 但未注册。它**不是死代码**：`broom::tidy()` 在 clinpubr 命名空间内被调用时，S3 调度会沿环境链找到它。实测把它替换成 `stop()` 后，`regression_scan()` 的 4 个 RCS 列全部变为 `NA`（错误被 `tryCatch` 吞掉）。

另外，`broom::tidy()` 自带的 `tidy.coxph` 对 `rms::cph` 对象会走 `summary.rms()`，在没有 `datadist` 时直接报错：

```
Error in summary.rms(x, ...) : adjustment values not defined here or with datadist for age
```

所以 `tidy.rms()` 是 RCS 结果目前唯一的正确计算路径。

#### 改动

1. 在 `R/misc.R:598` 前加 roxygen 标签并导出为 S3 方法：

```r
#' @importFrom broom tidy
#' @exportS3Method broom::tidy
tidy.rms <- function(x, conf.int = FALSE, exponentiate = FALSE, ...) { ... }
```

2. `R/regressions.R:798` 不再依赖隐式调度：

```r
full_res <- if (inherits(model, "rms")) {
  tidy.rms(model, conf.int = TRUE, exponentiate = analysis_type %in% c("logistic", "cox"))
} else {
  broom::tidy(model, conf.int = TRUE, exponentiate = analysis_type %in% c("logistic", "cox"))
}
```

3. 增加测试：`broom::tidy(cph_model)` 返回含 `term/estimate/std.error/statistic/p.value` 的 data.frame；`regression_scan()` 在 `survival::cancer` 上 RCS 列非 `NA`。

**验收**：从用户代码直接 `broom::tidy(cph_model)` 可用；RCS 列在研究数据上不再是 `NA`；不再依赖调用环境。

---

### P1-5 `subgroup_forest()`：类型判断与失败隔离

**状态**：已完成（1.4.4 工作区）

| 问题 | 位置 | 改动 |
|---|---|---|
| 用 `is.factor` 判断类型，`character` 被当成数值 | `R/subgroup_forest.R:50` | `x_type <- if (is.numeric(data[[x]])) "number" else "factor"`；`character` 先 `factor()` 并 `message()` 告知 |
| 协变量列按位置重命名，列顺序变化即错位 | `R/subgroup_forest.R:82-88` | 改为按名字：`idx <- match(covars, names(indf)); names(indf)[idx] <- placeholder`；`match` 出现 `NA` 时 `stop()` |
| 单个子组拟合失败会中断整张森林图 | `R/subgroup_forest.R:187-190` | 包 `tryCatch`，失败子组输出 `NA` 行并 `warning()`，其余子组继续 |
| 子组缺水平时行数依赖"空水平未被丢弃" | `R/subgroup_forest.R:183`、`202` | 显式 `factor(subset_data[[x]], levels = levels(indf[[x]]))`，并在拟合后断言行数与 `length(levels(indf[[x]])) - 1` 一致 |

**验收**：`x` 为 `character` 时不报"非数值"类错误；子组缺某一水平时 `Level` 与 `estimate` 行数一致；单个子组失败不影响其它子组。

---

### P1-6 `predictor_effect_plot()` 与 `regression_basic_results()` 的位置索引

**状态**：已完成（1.4.4 工作区）

`R/predictor_effect_plot.R:159-160` 的 `anova_fit[1, "P"]` / `[2, "P"]` 与 `R/regressions.R:815` 的 `ps[1]` / `ps[2]` 都依赖 `anova()` 的行序。改为按行名或按"是否包含 Nonlinear"匹配：

```r
p_overall   <- anova_fit[1, "P"]                              # 第一行是变量整体
p_nonlinear <- anova_fit[grepl("Nonlinear", rownames(anova_fit)), "P"]
```

**验收**：RCS 模型下 `pvalue_nonlin` 与 `anova(model)` 的 Nonlinear 行逐位一致。

---

## 3. 已移出范围（不在 1.4.4）

原 P2 无一条会修正错误数据或错误统计量，且大多带破坏性（停止导出、参数改名）或纯搬代码。1.4.4 不做，留待将来单独立项。

| 原条目 | 内容 | 移出理由 |
|---|---|---|
| P2-1 | 72 个导出拆 stable / experimental / internal | 停止导出会打断下游脚本，收益只是文档整洁 |
| P2-2 | `save_table`/`save_output`、`return_details`/`return_audit` 统一 | 纯改名 + deprecated 层，代码净增不净减 |
| P2-3 | 全量 `import()` 改按需 `importFrom` | 大 diff、零行为收益；只保留一条：确认 `src/*.o`、`src/*.dll` 未入库（`.gitignore` 检查） |
| P2-4 | 拆 `data_screen.R` / `merge.R` / `regressions.R` / `misc.R` | 纯搬代码；`merge.R` 的主要收益已由 P0-2 拿到 |
| P2-5 | `_pkgdown.yml`、vignette 方法学章节、README 措辞 | 发布包装，不属于正确性修复；README 措辞可随手改，不列为任务 |

---

## 4. 明确不做的事

| 事项 | 理由 |
|---|---|
| 新增绘图类型、新增"一键显著结果" | 在 P0 清零前不扩展功能面 |
| 禁止 `merge_by_substring()` 的一对多 | 一个名字属于多个分类是该函数的设计意图；应提示而非禁止 |
| 删除 `tidy.rms()` | 它是 RCS 结果的唯一正确来源，删除会让 RCS 列静默变 `NA` |
| 把 `survival` / `rms` / `Hmisc` 移入 `Suggests` | 主流程没有降级路径，移走等于要求用户手动装包才能用核心功能 |
| 为小样本校准引入留一法 | 属于新增算法，不在止血范围；先给 warning + `NA` |
| 否定词"距离权重"模型 | 先修边界与转义；短语级否定词表另行评估 |
| `p.adjust` 跨变换族统一校正（D4） | 族内校正是可辩护口径；改动会改变全部 p 值，只补文档 |
| `best.var.trans` 改名（D2） | 不修正任何数值，只破坏下游列名；只补文档 |
| 1.6.0 方法学扩展（多重插补、竞争风险、时变协变量、外部验证） | 与本计划的正确性目标无关，另行立项 |

---

## 5. 决策记录

| 编号 | 决策 | 结论 | 状态 |
|---|---|---|---|
| D1 | `unit_standardize()` 的 `on_unmapped` 取值 | `error`（默认）/ `warn` / `assume_target`（旧行为，强制 warning） | **已定** |
| D2 | `best.var.trans` 的去留 | 保留列名，只补"仅供探索，不可用于推断"文档 | **已定** |
| D3 | 唯一主仓库 | GitHub / Gitee 二选一，另一个做只读镜像 | **待定**（发布决策，不阻塞代码） |
| D4 | `p.adjust` 的作用范围 | 维持族内校正，只补文档 | **已定** |
| D5 | `regression_scan` 输出排序 | 保持按 p 值升序，文档说明不构成推断依据 | **已定** |
| D6 | 正类推断默认行为 | 用 `levels[2]` + warning，允许 `positive_class` 覆盖 | **已定** |

---

## 6. 执行顺序

```
P0-8 (CI + 快进 develop)
  └─ P0-1 (unit_standardize)
       └─ P0-2 (merge 去重)
            └─ P0-3 (merge_by_substring)
P0-4 (predictor_effect_plot)      ← 与上面无依赖，可并行
P0-5 (screen_data_list)           ← 与上面无依赖，可并行
P0-6 (classif_model_compare)      ← 与上面无依赖，可并行
P0-7 (extract_history)            ← 与上面无依赖，可并行
P1-4 (tidy.rms 注册)              ← 先做，因为 P1-1 的诊断输出依赖它
P1-1 (regression_scan)
P1-2 / P1-3 / P1-5 / P1-6        ← 互不依赖，可并行
  ↓ 1.4.4 单次发布
```

理由：

- P0-8 先做，否则后续所有改动没有 `develop` 上的 CI 兜底。
- P0-2 先于 P0-3，让行为修改只改一份实现。
- P1-4 先于 P1-1，`tidy.rms` 注册后 RCS 结果才稳定，诊断输出才有意义。
- P1 不再单独等一个版本：所有行为变更集中在同一条 NEWS 段落，用户只需迁移一次。

---

## 7. 发布检查单

1.4.4 发布前逐项确认：

- [ ] 本文 P0 与 P1 条目状态全部为 `已完成`。
- [ ] 每条修复都有"先失败、后通过"的测试；没有 `skip_if_not_installed()` 掩盖核心路径。
- [ ] 新增/修改的导出函数有 `@returns`、可运行示例、入口校验。
- [ ] 静默路径全部改为"报错 / warning + audit"。
- [ ] 快照变更经人工确认，未把错误行为重新固化（尤其 `tests/testthat/_snaps/regressions.md` 的 RCS 列）。
- [ ] `R CMD check --as-cran`：0 error / 0 warning；`develop` CI 通过。
- [ ] 逐个 `test_file()` 单独运行通过（防止测试顺序依赖）。
- [ ] NEWS 用一个 1.4.4 段落记录全部行为变更与迁移方式（`unit_standardize`、`merge_by_substring` 顺序、`screen_data_list`、`calc_cindex`、`interactions` 行数、`regression_scan` 诊断列、`classif_model_compare` 正类 warning）。
- [ ] 文档与实现一致（默认值、"未指定时"的语义、族内校正口径）。

**单文件测试命令**（用于检查顺序依赖）：

```powershell
Get-ChildItem tests/testthat/test-*.R | ForEach-Object {
  & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "suppressMessages(pkgload::load_all('.', quiet=TRUE)); testthat::test_file('$($_.FullName)')"
}
```

---

## 附录 A：问题位置总表

| 编号 | 文件 | 行号 | 一句话 |
|---|---|---|---|
| P0-1 | `R/unit_standardize.R` | 313 | 整组改写单位 |
| P0-1 | `R/unit_standardize.R` | 247-258 | `rule_lookup` / `target_units` 死代码 |
| P0-2 | `R/merge.R` | 637-1140 | 7 个函数重复定义 |
| P0-3 | `R/merge.R` | 88 | 空 key → `(?:)` |
| P0-3 | `R/merge.R` | 125-128 | `merge` 默认排序，丢失输入行序 |
| P0-4 | `R/predictor_effect_plot.R` | 143 | PH 用位置索引 |
| P0-4 | `R/predictor_effect_plot.R` | 251-255 / 403 | `df_hist_data` 未初始化即引用 |
| P0-4 | `R/predictor_effect_plot.R` | 145 / 152 | `warning(..., e)` |
| P0-4 | `R/predictor_effect_plot.R` | 56-62 | `method` 无 `match.arg` |
| P0-5 | `R/data_screen.R` | 857-866 | 缺映射时静默跳过 anchor |
| P0-5 | `R/data_screen.R` | 316-339 / 1057-1058 | 唯一性检查用单表 key，join 用交集 |
| P0-6 | `R/classif_model_compare.R` | 137 / 151 / 153 / 206 | 方向与正类硬编码 |
| P0-6 | `R/classif_model_compare.R` | 115 | NA 预测通过校验 |
| P0-6 | `R/classif_model_compare.R` | 243 | `n_tiles` 无下限 |
| P0-6 | `R/classif_model_compare.R` | 140-143 / 230 | PRAUC 与图口径不一致 |
| P0-7 | `R/extract_history.R` | 147-149 | 空关键词 → `(?:)` |
| P0-7 | `R/extract_history.R` | 204-211 | 顺序转义破坏含 `.` 的关键词 |
| P0-7 | `R/extract_history.R` | 187-194 | 否定窗口前后边界不一致 |
| P1-1 | `R/regressions.R` | 657 | `covars` 状态泄漏 |
| P1-1 | `R/regressions.R` | 184-189 / 312 / 343-355 | 名义因子输出 trend |
| P1-1 | `R/regressions.R` | 840-873 | 整体 `suppressWarnings` |
| P1-1 | `R/regressions.R` | 717-721 | 最小 p 选变换（仅补文档，不改代码） |
| P1-2 | `R/interactions.R` | 58-60 / 98 | 对称重复 + 重复校正 |
| P1-2 | `R/interactions.R` | 71-73 / 81-83 | 失败静默返回 NA |
| P1-2 | `R/interactions.R` | 426 / 430 | `test = "LRT"`（实测可接受，仅补文档） |
| P1-3 | `R/time_roc.R` | 20-23 | C-index 自动翻转 |
| P1-3 | `R/time_roc.R` | 103-108 | `total_tp = 0` 除零 |
| P1-4 | `R/misc.R` | 598-626 | `tidy.rms` 未注册 |
| P1-5 | `R/subgroup_forest.R` | 50 / 82-88 / 187-190 | 类型判断、按位置改名、无失败隔离 |
| P1-6 | `R/predictor_effect_plot.R` | 159-160 | `anova` 位置索引 |

## 附录 B：最小复现脚本

```r
# P0-1
df <- data.frame(subject = c("b","b","b"), value = c(10,20,30), unit = c("mL","dL","dL"))
unit_standardize(df, "subject", "value", "unit", list(list(subject = "b")))

# P0-3
df  <- data.frame(name = c("AB","B,C","A..","ACD"), value = 1:4)
key <- data.frame(key = c("A","B","C","ACD","AB",""), category = c(1,2,3,4,1,5))
merge_by_substring(df, key, "name", "key", "category")

# P0-4
d <- mtcars; d$am <- factor(d$am); d$vs <- factor(d$vs)
predictor_effect_plot(d, x = "wt", y = "vs", method = "linear", add_hist = FALSE, return_details = TRUE)
predictor_effect_plot(d, x = "wt", y = "vs", method = "bogus")

# P0-6
set.seed(1)
y <- factor(c("case","control")[sample(1:2, 200, TRUE)], levels = c("case","control"))
pred <- ifelse(y == "case", rnorm(200, 1), rnorm(200, 0))
d <- data.frame(y = y, m1 = pred)
classif_model_compare(d, target_var = "y", model_names = "m1", positive_class = "case")

# P0-7
stri_detect_regex("任意文本", "(?:)")                       # TRUE，说明空关键词匹配一切
stri_detect_regex("a.b", escape_keyword("a.b"))              # 现状 FALSE，修复后应为 TRUE
```

## 附录 C：变更影响面

| 变更 | 破坏性 | 需要同步修改 |
|---|---|---|
| `unit_standardize` 默认报错 | **是** | 文档示例 2、`test-unit_standardize.R`、NEWS、下游脚本 |
| `merge_by_substring` 保序 | **是**（输出顺序） | `test-merge.R:18`、NEWS |
| `screen_data_list` 默认报错 / `scope_log` 加列 | **是** | `test-data_screen.R`、NEWS |
| `classif_model_compare` 默认 warning | 否 | NEWS |
| `calc_cindex` 不再翻转 | **是**（返回值变化） | `test-time_roc.R`、NEWS |
| `interactions` 去重 | **是**（行数变化） | `test-interactions.R`、快照 |
| `regression_scan` 新增 `diagnostics` 列（列名不变） | 否（加法） | 快照、NEWS |
| `tidy.rms` 注册 | 否 | NEWS |
