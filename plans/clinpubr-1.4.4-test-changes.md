# clinpubr 1.4.4 既有测试改动说明

依据 `plans/clinpubr-1.4.3-review-and-plan.md` 实施 P0-1 ~ P0-8、P1-1 ~ P1-6 时，有 11 个测试文件与 4 组快照需要改动。改动分三类：

1. **必要改动**：修复本身改变了默认行为或输出结构，旧断言必然失败。
2. **收窄断言**：旧断言依赖"静默错误"这一缺陷（例如依赖单位被静默重标、依赖 C-index 被翻转、依赖失败被吞掉），属于把 bug 写成契约。
3. **顺带修复的基线失败**：测试在只加载 `testthat` 的会话里本来就跑不起来（`Surv` / `with_tempdir` 找不到），与本次语义变更无关。

所有断言目标仍是原函数的统计含义；没有为了"让测试变绿"而放宽数值容差。

---

## 1. 逐文件改动与原因

### 1.1 `tests/testthat/test-unit_standardize.R`（P0-1）

| 改动 | 原因 |
|---|---|
| `Test 2` 由只给 `list(subject = "b")` 直接调用，改为「先断言默认报错，再用 `on_unmapped = "assume_target"` 得到旧结果」 | 新默认 `on_unmapped = "error"`。旧用例正是被修的 bug：`b` 组有 `mL` 未映射，旧代码把 3 行全标成 `dL`、数值不变。保留旧结果的唯一合法途径是显式选择 `assume_target` |
| `Test 5` 由 `Errors on target_unit not in units2change` 改名为 `Only converted rows are relabelled`，并给每个 subject 写明 `units2change` + `coeffs` | 旧用例靠"只给 `target_unit`"触发 `stop`；新语义下只给 `target_unit` 等于"该组无换算规则"，触发的是未映射错误，错误原因已不同。改名后断言的是真正要守住的性质：只有实际换算的行才改单位 |
| 新增 `unmapped units fail by default` / `on_unmapped = "warn" keeps original unit and value` | 固定"默认正确失败"与"warn 时保持原单位+原值"两条新契约 |
| 新增 `removed units get NA unit and NA value` | 旧实现在 `units2remove` 上会保留原值却删除单位；现已统一为值、单位都为 `NA` |
| 新增 `rows not matched by rules are untouched and output units stay closed` | 属性测试：输出单位集合必须是输入单位集合的子集（+目标单位 + `NA`），防止再出现"凭空产生单位" |
| 新增 `assume_target reproduces legacy relabel with a warning` | 迁移路径必须可用：老用户仍能得到 1.4.3 的结果，但会被 warning 告知 |
| 新增 `units2change without coeffs is rejected`、`on_unmapped rejects unknown values` | 规则自相矛盾（有 `units2change` 无 `coeffs`）与非法参数不能在运行时静默降级 |
| 数据框 `change_rules` 用例的 `label` 由按位置写 `c("t", NA, 1e-3, NA, NA, "r")` 改为按「subject + unit」条件赋值 | 新规则要求"出现的每个单位都要有 label"，按位置赋值依赖 `unit_view()` 的当前行序，行序一变就静默错位；按行内容赋值同时满足新校验 |

### 1.2 `tests/testthat/test-merge.R`（P0-2、P0-3）

| 改动 | 原因 |
|---|---|
| 6 处既有用例加 `expect_warning(..., "命中多个分组")` | `merge_by_substring()` 命中多个分组时输出行数会从 n 变 m，现要求 warning。这些用例本身就有多组命中，原来"没有 warning"的断言等于把静默膨胀写成契约 |
| 基本用例的行序断言由 `c("A..", "AB", "AB", "ACD", ...)` 改为输入顺序 `c("AB", "AB", "B,C", "B,C", "A..", "ACD", ...)`，并补 `value` 断言 | `merge.data.table(..., sort = FALSE)` 明确保持输入行序；原顺序是 merge 的排序副作用，未在文档中承诺 |
| 新增 `rejects empty string keys`、`rejects all-NA keys` | 空字符串 key 会匹配所有行、全 `NA` key 会静默丢匹配，两者都属于"看起来正常的错数据" |

### 1.3 `tests/testthat/test-data_screen.R`（P0-5）

| 改动 | 原因 |
|---|---|
| `scenario 2` / `scenario 3` / `joined output` / `fallback` 四处补 `on_missing_anchor = "skip"` 并包 `expect_warning("已跳过 anchor 窗口")` | 这四处场景本来就有表（`patient`，或缺少 `visit_id` 的表）没有日期/就诊映射，旧行为是静默跳过 anchor 窗口。新默认改为报错，必须显式选 `skip` 才能复现原意 |
| 新增 `missing anchor mapping errors by default and skip records the reason` | 同时固定三件事：默认 `stop` 且错误列出表名；`skip` 时 warning；审计表 `anchor_scope` 有 `status`（applied/skipped/empty）与 `reason` |
| 新增 `many-to-many join on the shared key fails` | 原实现用共享 `pid` 直接 join，两张表在该键上都不唯一时会导致行数 m×n 膨胀；现要求 `stop("...行膨胀...")` |

### 1.4 `tests/testthat/test-predictor_effect_plot.R`（P0-4、P1-6）

| 改动 | 原因 |
|---|---|
| 新增 `return_details works without a histogram and for categorical predictors` | 旧代码在 `add_hist = FALSE` 与分类分支上引用未定义的 `df_hist_data`，一调用就报"对象不存在"；这是回归测试 |
| 新增 `invalid method and factor with method = 'linear' are rejected` | `method` 现在 `match.arg` 校验；因子 + 线性方法会给出不可解释的连续效应曲线，改为带变量名的 `stop` |
| 新增 `PH p-values are taken from the predictor row and the GLOBAL row` | P1-6：旧实现按位置索引取 PH p 值，协变量数量一变就取错行；改为按行名取 predictor 行，并单独暴露 `pvalue_ph_global` |
| 仅测试内部：新增用例里 `data$am <- as.factor(data$am)` | `mtcars$am` 是数值型，不转因子会走 `linear` 分支而不是 `categorical` 分支，测试会测错路径 |

### 1.5 `tests/testthat/test-classif_model_compare.R`（P0-6）

| 改动 | 原因 |
|---|---|
| 3 处既有调用补 `positive_class = "1"` | 未指定正类时新行为是 `levels(target)[2]` + warning。既有用例的 `target` 水平顺序不由测试控制，AUC/Brier 等断言依赖正类，必须显式指定 |
| 新增 `positive_class controls the direction of every metric` | 用 `pROC` 作为外部参考：`positive_class = "case"` 的 AUC 应等于参考值，`"control"` 应等于 `1 - AUC`；默认路径必须 warning |
| 新增 `invalid positive_class and NA predictions are rejected` | 非法正类、预测列含 `NA` 都属于入口错误，旧实现会把 `NA` 带进范围校验后产生难以定位的错误 |
| 新增 `small samples skip the calibration curve instead of crashing` | `caret::calibration` 在 `n < 20` 时切分失败；现改为 warning + `calibration_plot = NULL`，指标表仍返回 |

### 1.6 `tests/testthat/test-extract_history.R`（P0-7）

| 改动 | 原因 |
|---|---|
| 新增 `empty keywords are rejected instead of matching everything` | 空关键词会被拼成空 pattern，`grepl("")` 对任意文本都为 `TRUE`，等于"全部患者都有该病史" |
| 新增 `keywords with regex special characters match themselves` | 旧转义是逐个 `gsub` 且顺序有误，`a.b` / `x(y)` / `C++` / `A|B` 会被当正则解释；改为单次 `stri_replace_all_regex` 后应"原样自匹配" |
| 新增 `negation stops at clause boundaries in both directions` | 修 `但` 前后分句的否定作用域：`未使用降压药但高血压病史` 应判阳性；`否认高血压`、`高血压未控制` 仍判阴性 |
| 修复被误删的 `test_that("extract_history prefers real duration over year-like numbers", {` 行 | 实施期间编辑误伤导致该用例的 `test_that` 头缺失（基线已是坏文件）；恢复后断言内容未改 |

### 1.7 `tests/testthat/test-regressions.R`（P1-1、P1-4、P1-6）

| 改动 | 原因 |
|---|---|
| `regression_scan returns expected structure` 的列名集合补 `diagnostics` | P1-1 新增诊断列，属于输出结构变化 |
| 新增 `keeps covariates that conflict with one predictor for the others` | 直接回归"协变量状态泄漏"：`predictors = c("sex", "age")`、`covars = "sex"` 时，`age` 的 HR 必须等于把 `sex` 作为协变量的模型，而不是未校正模型 |
| 新增 `only reports P for trend for ordered factors` | 名义因子不应有 "P for trend"；同时确认有序因子仍有该行。注意实现保留了一处刻意的宽松：由数值派生的分组列（`x.quartile` 等）仍按有序变量出 trend，以避免既有四分位趋势结果消失 |
| 新增 `records fit diagnostics instead of failing silently` | 完全分离数据下 `glm` 的 warning 必须落到 `diagnostics` 列，而不是被 `suppressWarnings()` 吞掉 |
| 新增 `regression_fit takes RCS p-values from the anova rows by name` | P1-6：`p_overall` / `p_nonlinear` 改为按 `anova()` 行名匹配（`grepl("Nonlinear", ...)`），不再按位置 |

### 1.8 `tests/testthat/test-interactions.R`（P1-2）

| 改动 | 原因 |
|---|---|
| 文件头补 `library(withr)` | 基线失败：该文件用了 `with_tempdir()` 却从未加载 `withr`（Suggests 包不会被自动 attach），在任何干净会话中都会报"找不到函数"。与本次语义变更无关 |
| 新增 `interaction_scan lists each predictor/group pair once` | P1-2：`predictor × group` 与 `group × predictor` 是同一个交互项，旧实现会输出两行且 p 值相同；现在按排序后的 (predictor, group) 去重 |

### 1.9 `tests/testthat/test-time_roc.R`（P1-3）

| 改动 | 原因 |
|---|---|
| 文件头补 `library(survival)` | 基线失败：`timeROC` / `pec` 在公式里解析 `Surv`，只加载 `testthat` 时找不到。与环境配置有关，非语义变更 |
| `calc_cindex returns a valid C-index` 重写为 `returns the raw C-index without flipping` | P1-3 的核心：旧实现把 `< 0.5` 的 C-index 静默翻成 `1 - c`，旧断言 `>= 0.5` 正是把这个错误写成契约。新断言：与 `Hmisc::rcorr.cens` 的原始值一致；反向 marker 仍 `< 0.5` 且给出 warning |
| 反向 marker 数据改为 4 行确定性小样本 `rev_marker = c(4, 3, 2, 1)` | 旧的 `100 - maker` 与 `rcorr.cens` 的方向语义不保证为负相关，断言时对方向做了额外假设 |
| 新增 `time_roc_plot skips time points without cases or controls` | `total_fp == 0` 或 `total_tp == 0` 的时间点会除以零产生 `NaN` 点；现在跳过并 warning，且图数据中不允许出现 `NaN` |

### 1.10 `tests/testthat/test-subgroup_forest.R`（P1-5）

| 改动 | 原因 |
|---|---|
| 新增 `accepts a character predictor` | P1-5：`x` 为 character 时旧判断把它当数值处理；现在 `as.factor` + `message("... converted to a factor")` |
| 新增 `isolates a failing subgroup instead of aborting` | 单个子组拟合失败（单行子组的 `coxph` 报错）不应让整张森林图失败；现在 warning + 该子组返回 `NA` 行，图仍可生成 |

### 1.11 `tests/testthat/test-rcs_plot.R`（P0-4、P1-6）

| 改动 | 原因 |
|---|---|
| 两处 `expect_named(details, ...)` 名单补 `ph_table`、`pvalue_ph`、`pvalue_ph_global` | `return_details` 新增的三项是本次 PH 取值的修正结果，测试需同步输出契约 |

---

## 2. 快照改动

| 快照 | 变化 | 原因 |
|---|---|---|
| `_snaps/regressions.md` | 表格新增 `diagnostics` 列（部分单元格含真实拟合 warning 文本） | P1-1 不再吞掉拟合 warning，改为逐行记录；快照接受前已人工核对其余数值未变 |
| `_snaps/rcs_plot.md` | `details` 列表新增 `ph_table` / `pvalue_ph` / `pvalue_ph_global` | 同上，新增返回项；其中非 Cox 模型时三项为 `NULL`/`NA` |
| `_snaps/classif_model_compare/pr-plot.svg` | PR 曲线去掉 `NaN` 点 | `.pr_points()` 统一了 PRAUC 与绘图的取点逻辑，不再把非有限点当成 `1` |
| `_snaps/predictor_effect_plot/predictor-effect-cox-rcs-age.svg`、`_snaps/rcs_plot/cox-model-rcs-plot.svg`、`_snaps/rcs_plot/cox-model-rcs-plot-y-0-5-to-2-5.svg` | 图注由 `P-proportional` 改成 `P-proportional (age)` | PH 检验现在按行名取 predictor 行，并在标签里写明被检验的变量；这是唯一视觉差异 |

`_snaps/classif_model_compare.md` 仅有行尾格式变化，断言内容未变。

---

## 3. 刻意没有改的测试

- `regression_basic_results` 的四分位/中位数分组仍输出 "P for trend"：实现用 `.is_ordinal_var()` 白名单保留该行为，因此既有的四分位趋势断言不需要改。若改成"只有 `is.ordered()` 才出 trend"，1.4.3 的四分位趋势结果会整体消失，属于超出计划的语义变更。
- `time_roc_plot` 的既有图形用例未改：跳过无效时间点只在时间点确实无效时触发，默认数据不触发。
- `merge_by_range` 的测试未改：行膨胀校验只加在 `merge_by_substring` 与多表 join 上。

---

## 4. 验证方式

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "devtools::test()"
```

R CMD check 在无外网环境需关掉联网项：

```powershell
$env:_R_CHECK_CRAN_INCOMING_="false"; $env:_R_CHECK_CRAN_INCOMING_REMOTE_="false"; $env:_R_CHECK_FUTURE_FILE_TIMESTAMPS_="false"
& "C:\Program Files\R\R-4.5.1\bin\R.exe" CMD check --as-cran --no-manual clinpubr_1.4.4.tar.gz
```

目标：`0 error | 0 warning | 0 note`（本地无 LaTeX 时用 `--no-manual`）。
