# 对《clinpubr 1.4.3 之后的改进路线》的逐条核验与修改方案

**核验对象**：报告基于 Gitee `develop` / commit `7bf91cd`（DESCRIPTION 1.4.3）。
**核验方式**：直接读源码 + 在本机 R 4.5.1 上跑最小复现；凡是报告给出的断言，都尽量用一次实际调用确认。
**重要前提**：本地工作区的 `develop` 停在 `25e3729`，比 Gitee 的 `7bf91cd` 少一个 commit（该 commit 把 `create_formula()` 的 `Surv` / `rcs` 改成命名空间限定写法）。因此本地直接跑测试会看到 5 个 `predictor_effect_plot` 报错，**这不是报告指出的问题，而是本地落后一版**；本方案的所有结论以 `7bf91cd` 为准。

---

## 0. 总体结论

1. **报告整体可信**：可核验的 30 条断言中，24 条完全成立，4 条部分成立（定性偏重或机制描述不准），2 条不成立（`PH 取 GLOBAL` 被当成"取错行"、`tidy.rms` 被当成死代码）。
2. **它的分层方向正确**：先把"静默出错"清零，再做统计语义，最后做结构治理。1.4.4 / 1.4.5 / 1.5.0 / 1.6.0 的顺序我认可。
3. **但它有 5 条"修法"会改坏现有正确行为**（详见 §4.4），其中两条后果严重：
   - 删除 `tidy.rms()`：它是 RCS 结果目前唯一的计算来源，删掉后 `regression_scan()` 的 4 个 RCS 列会变成全 `NA`（已被 `tryCatch` 静默吞掉）。
   - 禁止 `allow.cartesian`：该参数是"一个检验名命中多个分类"这一设计意图的必需项，禁掉会让多标签匹配直接报错。
4. **它漏掉了 5 个同类小问题**（§4.5），都不影响版本主线，可顺手修。
5. **关于你问的 3.1 方案**：我建议"逐行写回 + 未映射单位默认报错 + `on_unmapped` 三值参数"，不建议"加一个开关把未列出的单位全部改掉"。理由见 §2：**缺系数时任何"全部改"都只能默认系数 = 1，等于把现在这个静默 bug 换个名字留下来。**你的诉求（一轮之后不应再有非标准单位）通过"默认报错"即可保证，不需要"全改"开关来保证。

---

## 1. 逐条核验（P0 组）

| 编号 | 报告断言 | 核验 | 实测/证据 | 紧迫性 | 结论 |
|---|---|---|---|---|---|
| 3.1 | 只换算 `units2change` 命中的行，却把该 subject 所有行的单位改写 | **成立** | `R/unit_standardize.R:312-313`：先按行改 `subject_values`，最后 `units[subject_idx] <- target_unit` 覆盖整组；`tests/testthat/test-unit_standardize.R:19-26` 把 `10 mL → 10 dL`（数值不变、单位变）固化为期望 | **高**（静默产生 10 倍数量级错误） | 必改 |
| 3.1 | `units2change` 未指定 = "除目标外全部"（文档语义） | **成立（文档与实现不一致）** | 文档 `R/unit_standardize.R:31-32` 这么写；实现中 `units2change` 为空时一行都不换算，只改标签 | 中 | 改文档，不要改实现（见 §2） |
| 3.1 | `rule_lookup` / `target_units` 是死代码 | **成立** | `R/unit_standardize.R:247-258` 构造后从未读取 | 低 | 顺手删 |
| 3.2 | 7 个内部函数各定义两次，约 500 行重复 | **成立** | `.coerce_join_value` / `.build_merge_group_key` / `.na_like_df` / `.build_clipped_ranges` / `.merge_by_range_impl` / `.merge_by_range_finalize` / `merge_by_range` 分别在 132-636 与 637-1140 行各一份，逐字相同；后一份覆盖前一份 | **中**（改第一份等于没改，是维护陷阱） | 必改 |
| 3.2 | 空 key 生成 `(?:)`，匹配一切 | **成立，且可复现** | `R/merge.R:88`：`key = ""` → `escapeRegex("")` = `""` → `"(?:)"`；实测 4 行输入 → 12 行输出，无任何提示 | 高（临床数据行被复制） | 必改 |
| 3.2 | 互为子串时产生"笛卡尔重复" | **部分成立** | 不是 m×n 笛卡尔扩张（`match_results` 每个 search 值每组只有一行），而是"一个检验名命中多个分类 → 同一行出现多次"；实测 `key = c("A","AB")`、name = `"ABCD"` → 2 行，无提示 | 中 | 改为"行数膨胀警告"，不要禁 `allow.cartesian` |
| 3.2 | 需要"明确并固定 join 后的行顺序" | **部分成立** | 现状已经确定：`data.table::merge` 默认 `sort = TRUE`，输出按 `search_col` 排序；`tests/testthat/test-merge.R:18` 已把排序结果写进断言。**真实问题是"静默改变输入行序"，不是"顺序不确定"** | 低-中 | 改为保序（`sort = FALSE`）+ 文档 |
| 3.3 | PH 检验取 `table[1, 3]` 而不是 `GLOBAL` 行 | **事实成立，"错报"定性不成立** | `R/predictor_effect_plot.R:143`。`create_formula()` 把 predictor 放在公式首位，所以第 1 行就是**该预测变量自己的 PH 检验**（实测 `cox.zph(cph)` 行名为 `age`/`sex`/`GLOBAL`）；对"单变量效应图"而言，看自己的 PH 比看 GLOBAL 更贴切 | 中 | 两个值都返回，按行名取；改标签 |
| 3.3 | `add_hist = FALSE` 或 `method = "categorical"` + `return_details = TRUE` 必崩 | **成立，已复现** | `df_hist_data` 只在 `R/predictor_effect_plot.R:254` 赋值，`R/predictor_effect_plot.R:403` 无条件引用；实测两种组合均报 `找不到对象'df_hist_data'` | **高**（100% 可复现崩溃） | 必改 |
| 3.3 | `method` 无 `match.arg()`，非法值静默降级 | **成立** | 实测 `method = "bogus"` 报"更换参数长度为零"这种不可读错误 | 中 | 必改 |
| 3.3 | `warning("...", e)` 写法错误 | **成立** | `R/predictor_effect_plot.R:145,152` | 低 | 顺手改 |
| 3.4 | 缺 date/visit 映射时静默跳过 anchor 过滤 | **成立** | `R/data_screen.R:857-866`：`ocol` 为 `NULL` 时整段跳过，只保留"患者属于入组集"的筛选，仍写 `anchor_scope` 日志（日志看起来像处理过） | **高**（入组窗口失效但无人知晓） | 必改 |
| 3.4 | join 前只检查单表 key 完整性，未检查交集 key | **成立** | 检测用该表自身 key 元组（`R/data_screen.R:316-339`），join 用两表 key 的交集（`R/data_screen.R:1057-1058`）；A 在 (patient,visit) 唯一、B 在 (patient,date) 唯一时，`by = "patient_id"` 会静默 m×n | **高**（队列行数被放大） | 必改 |
| 3.5 | 硬编码 `direction = "<"` 与 `positive = levels(target)[2]` | **成立，且 case/control 会反向** | `R/classif_model_compare.R:137,151,153,206`；`factor()` 默认字母序 → `levels = c("case","control")` → 正类取成 `control` | **高**（AUC、敏感度、特异度全部反着报） | 必改 |
| 3.5 | 预测列含 `NA` 直接崩 | **成立** | `R/classif_model_compare.R:115` 用 `na.rm = TRUE` 放过 NA，到 `ResourceSelection::hoslem.test` 才报错，错误信息与"哪个模型/哪一行"无关 | 中 | 必改 |
| 3.5 | 小样本校准分箱为 0 | **部分成立** | `n_tiles <- min(round(nrow(data)/10), 20)`（`R/classif_model_compare.R:243`）：`n ≤ 5` 时 `n_tiles = 0` → `cut()` 报"间隔数目无效"；`n = 10` 时只有 1 个箱（不崩但校准无意义） | 中低 | 改为明确 warning + `NA`，不做留一法 |
| 3.5 | PR 图与 PRAUC 的 NaN 处理不一致 | **成立** | `R/classif_model_compare.R:140-143` 剔除无效点算积分；`R/classif_model_compare.R:230` 把 precision 的 NaN 置 1 用于绘图 | 低-中 | 统一口径 |
| 3.6 | 空关键词生成 `(?:)` 匹配一切 | **成立** | `R/extract_history.R:147-149`；实测 `""` / `c("a","")` 均匹配全部文本 | **高**（历史提取全命中，结果全错） | 必改 |
| 3.6 | 关键词转义不完整 | **成立，且比报告说的更严重** | `R/extract_history.R:204-209` 用 `vectorize_all = FALSE` 顺序替换：先把 `.` 换成 `\.`，再把所有 `\` 翻倍成 `\\`，导致含 `.` 的关键词**匹配不到自己**（实测 `"a.b"` 返回 FALSE） | **高**（静默漏检） | 必改 |
| 3.6 | 否定判断用单字 `未`/`无` + 固定 20 字符窗口 | **成立** | `R/extract_history.R:178-194`；且后向窗口不排除"但"，前向窗口排除，两向不对称 | 中 | 修边界，不做大改 |

---

## 2. 3.1 专项：`unit_standardize()` 应该怎么改

### 2.1 先确认"重要性"从哪来

现在的行为不是"不改未列出的单位"，而是**"数值按规则改、单位整组改"**：

```r
values[subject_idx] <- subject_values   # 只改了命中行
units[subject_idx]  <- target_unit      # 但整组 3 行都改
```

所以 `subject = "b", units = mL/dL/dL` 且只写 `list(subject = "b")` 时，`10 mL` 会变成 `10 dL`——**数值是原始量级，标签是目标单位**，后续任何按单位做解释的分析都错，且没有任何提示。这一条比报告里其他 P0 更危险：它不报错、不 warning、结果看起来很整齐。

### 2.2 你的方案 vs 我的建议

| 方案 | 做法 | 优点 | 问题 |
|---|---|---|---|
| A. 你的原话 | 加参数"明确修改所有单位"，默认不动未列出的 + 报错/报警提示用该参数 | 简单，改动小 | "修改所有单位"在**缺系数时无法安全执行**：只能默认系数 = 1，等于把当前静默 bug 变成显式 bug；而且默认"不动"会让结果继续存在非标准单位，正是你想避免的困扰 |
| B. 建议采纳 | 逐行写回 + 未映射单位**默认报错**（一次性列出全部 subject/unit/行数）+ `on_unmapped = c("error","warn","assume_target")` | 默认路径保证"要么每个 subject 只剩一个单位，要么明确失败"；`assume_target` 精确表达"数值已是目标单位、只需改标签"这一真实场景 | 默认行为是破坏性变更（需要 NEWS + 迁移说明） |
| C. 拆函数 | `unit_relabel()`（只改标签） + `unit_convert()`（只换算，必须有系数） | 语义最干净 | 多一个导出函数；真实数据往往两者混合，用户要调两次；不符合"使用简单" |

**推荐 B。** 它与你的思路只差一点：把"全部改"这个开关命名为 `assume_target`（假设数值已在目标单位），而不是 `convert_all`（暗示存在安全的自动换算）。这样开关的名字本身就在说明风险。

### 2.3 推荐 API 与语义

```r
unit_standardize(df, subject_col, value_col, unit_col, change_rules,
                 extract_numbers = FALSE,
                 on_unmapped = c("error", "warn", "assume_target"),
                 verbose = FALSE)
```

按行处理，三类行互斥：

| 行的单位 | 数值 | 输出单位 | 输出数值 |
|---|---|---|---|
| `== target_unit` | 原样 | `target_unit` | 原样 |
| ∈ `units2change`（有系数） | `value * coeff` | `target_unit` | 换算后 |
| ∈ `units2remove` | 任意 | `NA` | `NA` |
| 其他（未映射） | — | `on_unmapped` 决定 | — |

`on_unmapped` 行为：

- `"error"`（默认）：先扫完整份数据，再抛出**一条**包含全部违规项的错，而不是逐行中断：

```
未映射的单位无法标准化：
  subject "alt"  unit "U/L"   23 行
  subject "alt"  unit ""      4 行
  subject "cr"   unit NA      8 行
请为这些单位在 change_rules 中提供换算系数，或标记为 "r" 删除；
若这些数值本身已经是目标单位、只需要统一标签，请设置 on_unmapped = "assume_target"。
```

- `"warn"`：同样的表，降级为 warning，行保持原值与原单位（不伪装成已标准化）。
- `"assume_target"`：把这些行标成 `target_unit`、数值不动（等于现在的行为），并 warning 汇总被重标的 subject / 单位 / 行数。

配套改动：

1. `units2change` 里有单位但 `coeffs` 为空 → 直接 `stop()`（现在默认按 1 处理，同样是隐式假设）。
2. 文档中"`units2change` 缺省 = 除目标外全部"改成"缺省 = 不换算"（缺系数无法换算，这句话本身就不可能成立）。
3. 删除 `rule_lookup` / `target_units` 死代码；删除 `R/unit_standardize.R:107-109` 与 `136-138` 的重复 warning（同一句会打两遍）。
4. `label = "r"` 的行输出单位改为 `NA`（现在是 `target_unit`，等于给"已删除的值"贴了一个单位）。
5. `unit_view()` 增加一列 `mapped`（或 `status`）标记"有规则/无规则"，让上表在报错前就能被看到——这是让"一轮搞定"最省事的一步，比加开关更有效。

### 2.4 需要同步修改的测试与文档

- `tests/testthat/test-unit_standardize.R:19-26`（`mL → dL`）必须**替换**，不能更新快照了事。
- `tests/testthat/test-unit_standardize.R:49-66` 标题写"Errors on target_unit not in units2change"，实际断言是成功，改名。
- 文档示例 2（`R/unit_standardize.R:58-89`）只标了 3 个单位，其余 `label` 为 `NA`；新默认下会报错，示例需要补齐标签。
- 新增测试：未映射报错（含全部违规项）、`warn` 保留原值、`assume_target` 复现旧行为但有 warning、未命中行数值与单位不变、输出单位集合 ⊆ 输入 ∪ `{target, NA}`。

---

## 3. 逐条核验（P1 / P2 组）

| 编号 | 报告断言 | 核验 | 证据 | 紧迫性 | 结论 |
|---|---|---|---|---|---|
| 4.1 | 按最小 p 自动选变换 | **成立** | `R/regressions.R:717-721`，取的是未校正 `*pval` 列；`R/regressions.R:709` 还按 p 值排序 | 中 | 建议**保留列但改名/降级**，不必删（见 §4.4） |
| 4.1 | `covars` 在循环内被改写（状态泄漏） | **成立** | `R/regressions.R:657`：`covars <- remove_conflict(...)` 在 `for (i in seq_along(predictors))` 内部改写函数作用域的 `covars`，之后所有 predictor 都用被削过的协变量集 | **中高**（同一张表里不同行的模型设定不同，结果不可比） | 1.4.5 必改 |
| 4.1 | `suppressWarnings()` 包裹整个拟合 | **成立** | `R/regressions.R:840-873` | 中 | 改 |
| 4.1 | `p.adjust` 按变换族分别执行 | **成立** | `R/regressions.R:710-716` | 中低 | 方法论选择，需你拍板 |
| 4.1 | 名义因子也输出 "P for trend" | **成立** | `R/regressions.R:184-189`（`as.factor` 丢掉 ordered）、`R/regressions.R:312`、`R/regressions.R:348`（按 level 编号当连续变量） | **中高**（把无序变量当趋势检验，p 值不可解释） | 1.4.5 必改 |
| 4.1 | cluster / GEE 分支不可达 | **成立** | `R/regressions.R:792` 中 `cluster` 非空即 `rms = TRUE`，故 `R/regressions.R:829-831,855-862` 的 GEE 分支永远进不去；cluster 实际总走 `rms::robcov`（`R/regressions.R:849-850`） | 低-中 | 删分支 + 改文档 |
| 4.2 | `age×sex` / `sex×age` 对称重复 | **成立** | `R/interactions.R:58-60` 只排除自身；重复行还各自进入 `p.adjust`（`R/interactions.R:98`），等于同一检验校正两次 | 中 | 必改 |
| 4.2 | 交互检验统一传 `"LRT"` | **成立** | `R/interactions.R:426,430`；对 `lm` 会走 χ² 而非 F（不报错，但检验不同） | 低-中 | 改 |
| 4.2 | 失败静默吞掉 | **成立（且比报告更糟）** | `R/interactions.R:71-73,81-83`：`error = function(e) NA`，连 warning 都没有 | 中 | 必改 |
| 4.3 | 类型判断用 `is.factor` 而非 `is.numeric` | **成立** | `R/subgroup_forest.R:50`：`character` 会被判成 `"number"` | 中低 | 改 |
| 4.3 | 子组缺水平未显式对齐 | **部分成立** | `R/subgroup_forest.R:183,202` 用全量 `levels()`，行数靠"因子保留空水平"碰巧对齐；子组拟合无 `tryCatch`（`R/subgroup_forest.R:187-190`），一个子组失败会中断整张森林图 | 低 | 加强制 + 测试 |
| 4.4 | C-index < 0.5 自动翻转 | **成立** | `R/time_roc.R:20-23` | 中（静默改指标，和路线原则冲突） | 必改 |
| 4.4 | `total_tp == 0` 无保护 | **成立** | `R/time_roc.R:103-108` 直接除 | 低-中 | 改 |
| 4.6 | `tidy.rms` 是"定义了但未注册的死代码"，建议删除 | **不成立** | `R/misc.R:598-626` 未注册，但 `broom::tidy()` 在 clinpubr 命名空间内调用时会通过 S3 环境链找到它——实测把它替换成 `stop()` 后，`regression_scan()` 的 RCS 列全部变 `NA` | 高（删了会出事） | **注册，不要删** |
| 5.1 | 导出 70+、内部工具被导出 | **成立** | `NAMESPACE` 共 72 条 `export()`；`break_at` / `emp_colors` / `formula_add_covs` / `unmake_names` / `replace_elements` 均在列 | 低-中 | 1.5.0 |
| 5.1 | 参数命名/返回类型不统一 | **成立** | `save_table`（5 个导出）vs `save_output`（2 个）；`return_details`（2 个）vs `return_audit`（1 个） | 低 | 1.5.0 |
| 5.2 | "补齐剩余全量 `import()`" | **不成立（方向相反）** | 现状 5 条全量 `import()`（Rcpp/ggplot2/rlang/stats/stringr）、22 条 `importFrom`；应继续收敛到 `importFrom` | — | 不采纳 |
| 5.2 | 可选依赖应移入 `Suggests` | **部分成立** | `survival` / `rms` / `Hmisc` 在 Imports，但它们是主流程核心（不存在降级路径），不该移；`pROC` 已在 Suggests；`mice` 在 DESCRIPTION 里根本不存在 | — | 只保留"可选依赖降级"部分 |
| 5.4 | CI 不覆盖 `develop`；无"全 Suggests" job | **成立** | `.github/workflows/R-CMD-check.yaml:3-6` 与 `test-coverage.yaml:3-6` 均为 `[main, master]` | 中（低成本、高收益） | 1.4.4 就做 |
| 5.4 | GitHub 落后 Gitee | **成立** | `origin`(Gitee)=1.4.3、`github`/`upstream`=1.4.0；本地又比 Gitee 少一个 commit | 中 | 先定主仓库 |
| 5.4 | `my_test.r` 游离在根目录 | **成立** | 28.9 KB 开发草稿，未被 Collate 引用 | 低 | 删 |
| 5.3 | 四个大文件行数 | **成立** | 1087 / 1140 / 875 / 626 | 低-中 | 1.5.0 |

---

## 4. 修改方案

### 4.1 1.4.4：数据正确性（建议 8 项，按依赖排序）

| # | 项 | 动作 | 验收（先失败后通过） |
|---|---|---|---|
| 1 | `unit_standardize` | 逐行写回 + `on_unmapped` 三值 + 系数必须显式 + 删死代码/重复 warning | §2.4 的 5 条新测试 + 替换旧测试；属性测试：输出单位 ⊆ 输入 ∪ `{target, NA}` |
| 2 | `merge.R` 去重 | 删除 637-1140 的第二份复制（保留哪一份不影响行为，建议保留靠前的一份并删后一份，避免行号漂移） | `merge_by_*` 全部测试不变；`git diff --stat` 应约 -500 行 |
| 3 | `merge_by_substring` | 空/全 `NA` key → error；一个 search 值命中 >1 组 → warning 并给出行数变化；`merge(sort = FALSE)` 保序 | 三个负例 + 行序测试；更新 `test-merge.R:18` 的排序断言 |
| 4 | `predictor_effect_plot` | `df_hist_data <- NULL` 初始化；`method <- match.arg()`；`method="linear"` + factor → error；PH 同时返回"该变量"与 `GLOBAL`（按行名取）；`warning(paste0(...))` | 三种 `return_details` 组合不崩；PH 数值与 `cox.zph()` 对应行逐位一致 |
| 5 | `screen_data_list` | `on_missing_anchor = c("error","warn","skip")` 默认 error；skip 时写 `scope_log` 原因；join 前对**实际 `by_used` 交集**做 `duplicated()` 检测 | 缺映射报错；表只在 `patient_id` 上多对多时 join 失败 |
| 6 | `classif_model_compare` | 新增 `positive_class`，AUC/Brier/HL/DCA/confusionMatrix 的方向全部由它推导；预测列 NA → 报错带模型名与行号；`n < 20` → warning + 校准 `NA`；PRAUC 与图共用一份清理后的 P/R | case/control + `positive_class="case"` 与手工 `pROC` 一致；NA/常量/`n = 10` 三种输入各有明确信息 |
| 7 | `extract_history` | 空关键词 → error；转义改成一次正则替换；否定词前后窗口用同一套标点边界 | 空关键词、含 `.` 关键词、跨句否定三组测试 |
| 8 | CI + 仓库 | 两个 workflow 的 `push.branches` 加 `develop`；加一个安装全部 Suggests 的 job；删 `my_test.r`；把本地 `develop` 快进到 `origin/develop` | CI 在 develop 上跑绿；单文件测试可独立运行 |

> 为什么第 8 项放 1.4.4：它是零风险的，而且后面所有修复都需要 CI 在 `develop` 上兜底。

### 4.2 1.4.5：统计可信度

1. `regression_scan`
   - `covars` 改为循环内局部变量（`covars_i`），保证每个 predictor 用同一协变量集；
   - 只对 `is.ordered()` 输出 `P for trend`，名义因子只给 overall p；
   - 去掉整体 `suppressWarnings()`，把完全分离/收敛失败/ties 写入结果表 `diagnostics` 列；
   - `p.adjust` 是否跨变换族统一，建议**是**（一族一个校正会让"选最小 p"更容易显著）；
   - `best.var.trans`：**保留但改名 `exploratory.best.trans`** 并在文档中写明"由未校正 p 最小值选出，仅供探索"；直接删列会破坏 README 表格和现有用户脚本，收益不足（见 §4.4）；
   - 删除不可达 GEE 分支，`cluster` 文档统一写成"稳健方差（`rms::robcov`）"。
2. `interactions`：无序对去重；`lm` 用 F、`glm`/`coxph` 用 LRT；失败返回 `NA` + warning（带 predictor/group 名）。
3. `time_roc` / `calc_cindex`：删除自动翻转，改为 warning + 返回原值；`total_tp == 0` 跳过该时间点并 warning。
4. `tidy.rms`：**注册** `S3method(tidy, rms)`（`importFrom(broom, tidy)` + `@export`），并让 `regression_fit()` 显式调用 `tidy.rms()`，不再依赖隐式调度。
5. `subgroup_forest`：`is.numeric()` + `character → factor`（带 message）；子组拟合包 `tryCatch`，失败子组输出 `NA` 并 warning。

### 4.3 1.5.0：结构与发布

- API 三分（stable / experimental / internal），内部工具停止导出；统一 `save_table`/`return_details` 命名。
- 拆 `merge.R`（去重后只剩 600 行左右）、`data_screen.R`、`regressions.R`、`misc.R`。
- 定唯一主仓库（GitHub 或 Gitee 二选一，另一个做只读镜像），CI 覆盖 `develop`，加 revdep / CRAN checklist，加 `_pkgdown.yml`。

### 4.4 不采纳 / 需要改写的报告建议

| 报告建议 | 处置 | 理由 |
|---|---|---|
| 删除 `tidy.rms` | **反向：注册它** | 实测它是 RCS 结果的唯一来源；删掉后 `regression_scan()` 的 RCS 4 列静默变 NA |
| 禁止 `allow.cartesian` | 改为"行数膨胀 warning" | 一个检验名命中多个分类是函数的设计意图，禁掉会让正常用法报错 |
| 补齐全量 `import()` | 反向：继续收敛到 `importFrom` | 全量 import 会污染命名空间（目前已出现 `filter`/`lag` 与 dplyr 冲突的迹象） |
| `survival`/`rms`/`Hmisc` 移到 `Suggests` | 保持 `Imports` | 主流程无降级路径，移走等于让主流程依赖用户手动装包 |
| PH 必须取 `GLOBAL` | 两个都给，图上标清楚 | 单变量效应图看自身 PH 更合理；GLOBAL 用于整体模型诊断 |
| `n < 20` 用留一法校准 | 改为 warning + 校准 `NA` | 留一法校准是新增算法，不属于 1.4.4 的止血范围 |
| 1.6.0 方法学扩展 | 保留为方向 | 不在本次评审范围 |

### 4.5 报告漏掉的同类小问题（可顺手处理）

1. `unit_standardize` 非数值列会打两遍同样的 warning（`R/unit_standardize.R:107-109` 与 `136-138`）。
2. `.parse_df_rules()` 在"全部 label 为 NA"时只 warning 并原样返回（`R/unit_standardize.R:157-162`），与其他"无规则就报错"的路径不一致。
3. `regression_scan()` 第 719 行 `sum(!is.na(unlist(p_table[i, ])) > 0)` 运算符优先级写错（结果碰巧可用，但意图是 `sum(!is.na(...)) > 0`）。
4. `pvalue_all` / `pvalue_nonlin` 同样用位置索引 `anova_fit[1, "P"]` / `[2, "P"]`（`R/predictor_effect_plot.R:159-160`），与 PH 那条是同一类脆弱写法。
5. `screen_data_list()` 会把未映射的同名列重命名为 `列名.表名`（`R/data_screen.R:982-990`），join 结果的 schema 会静默变化，应至少在文档中声明。

---

## 5. 建议的 1.4.4 执行顺序

1. 本地 `develop` 快进到 `origin/develop`（`7bf91cd`），并让 CI 覆盖 `develop` —— 否则后面所有验证都基于旧代码。
2. `unit_standardize()`（依赖最少、收益最大、要改测试最多，先做掉）。
3. `merge.R` 去重（纯删除，先做可以让接下来的 merge 行为修改只改一处）。
4. `merge_by_substring()` 空 key / 行数膨胀 / 保序。
5. `predictor_effect_plot()` 崩溃与 `method` 校验。
6. `screen_data_list()` anchor 与 join key。
7. `classif_model_compare()` `positive_class`。
8. `extract_history()` 空关键词与转义。
9. 全量回归：`R CMD check --as-cran` + 单文件 `test_file()` 逐个跑（确认没有测试顺序依赖）。

---

## 6. 需要你拍板的 3 个决策

1. **`unit_standardize()` 的默认值**：默认 `"error"`（正确但破坏兼容），还是默认 `"warn"`（兼容但可能让混合单位继续存在）？我建议前者，并在 NEWS 写明。
2. **`regression_scan()` 的 `best.var.trans`**：改名降级（我建议）还是直接删除（报告建议）？
3. **主仓库**：GitHub 还是 Gitee？这决定 CI 与 README 的写法，越早定越好。
