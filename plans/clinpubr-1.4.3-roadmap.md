# clinpubr 1.4.3 之后的改进路线

**评估对象**：Gitee `develop`，commit `7bf91cd`，`DESCRIPTION` 版本 1.4.3  
**文档范围**：只覆盖 1.4.3 之后的改进路线，不重复完整缺陷清单  
**核心判断**：1.4.3 已完成第一轮质量修复；剩余问题集中在“静默错误”和“统计选择偏差”。后续应先把这两类问题清零，再扩展方法学能力。

---

## 一、路线原则

1. **先正确失败，再增加功能**：宁可显式报错，也不允许静默产生错误数据或不完整筛选。
2. **所有静默行为必须可审计**：跳过、降级、剔除数据都必须 warning + 写入 audit 日志。
3. **每个版本可独立发布、可回滚**：不把结构性重构和统计语义变更混在同一个版本。
4. **停止扩展“自动显著性筛选”**：从“挑最小 p 值/最优变换”转向“预设主分析 + 敏感性分析并列”。
5. **API 分层**：稳定 API、实验 API、内部工具三类，内部工具不再导出。

**明确不做**：在 P0 清零前新增绘图类型、新增自动选模型/选变换功能、继续堆叠“一键显著结果”。

---

## 二、版本路线总览

| 版本 | 主题 | 关键结果 |
|---|---|---|
| **1.4.4** | 数据完整性止血 | 消除静默改数据、静默跳过、重复定义、必然崩溃 |
| **1.4.5** | 统计可信度 | 移除 p-hacking、统一诊断输出、明确模型语义 |
| **1.5.0** | 结构与发布治理 | API 分层、拆分巨型函数、CI 覆盖真实开发线 |
| **1.6.0** | 方法学能力 | 多重插补、竞争风险、时变协变量、外部验证 |

---

## 三、1.4.4：数据完整性止血（P0）

### 3.1 `unit_standardize()`：停止静默重标

**现状**：只换算 `units2change` 命中的行，却把该 subject 所有行的单位统一改写；未标注单位被“数值不变、单位被改”。现有测试还把 `10 mL → 10 dL` 固化为期望。

**动作**
- 改为逐行映射：只对被换算的行写回 `target_unit`，未命中行保持原单位（包括 `NA`）。
- 实现 `units2change` 缺省语义（除目标单位外全部转换），或明确修改文档为“缺省不转换，仅重标”，二选一。
- 删除 `rule_lookup`、`target_units` 等死代码。
- 保留 1.4.3 已加入的 NA subject/value 行 warning 与剔除逻辑。

**验收**
- 新增测试：未标注单位必须保持 `NA`；未被换算的行数值与单位都不变。
- 替换现有固化错误行为的测试（`subject = "b"` 的 `mL, dL, dL`）。
- 属性测试：任意输入下，输出单位集合只能是“输入单位 ∪ {target_unit}”，且数值只允许在命中行发生变化。

### 3.2 `merge.R`：去重与冲突显式化

**现状**：7 个内部函数各定义两次，后定义覆盖前定义，约 500 行重复代码；空 key 生成 `(?:)`，互为子串时产生笛卡尔重复且无警告。

**动作**
- 删除全部重复定义，只保留一份实现。
- 空 key / 全 `NA` key：显式报错。
- key 互为子串或一个 search 值匹配多个组：报错或 warning，并禁止 `allow.cartesian` 静默扩张。
- 明确并固定 join 后的行顺序；在文档中写明。

**验收**
- 行数减少约 500 行，`merge_by_*` 行为不变。
- 新增空 key、子串冲突、一对多三组负例测试。
- 新增行序稳定性测试。

### 3.3 `predictor_effect_plot()`：修复错报与对象不存在

**现状**：PH 检验取 `table[1, 3]`（第一个变量）而非 `GLOBAL` 行；`df_hist_data` 只在 `add_hist && method %in% c("rcs","linear")` 时定义，`return_details = TRUE` 却无条件引用。

**动作**
- PH p 值固定取 `GLOBAL` 行，并在结果中同时保留各变量行。
- `df_hist_data` 在函数入口初始化为 `NULL`；`return_details` 显式处理“无直方图数据”的情况。
- `method` 增加 `match.arg()`；`method = "linear"` 遇到 factor 时明确报错。
- `warning("...", e)` 改为 `paste0()` 或 `conditionMessage(e)`。

**验收**
- `add_hist = FALSE`、`method = "categorical"`、`return_details = TRUE` 三种组合均不报 object not found。
- PH 结果与 `cox.zph()` 的 `GLOBAL` 行逐位一致。

### 3.4 `screen_data_list()`：不允许静默保留全量数据

**现状**：表缺少 date/visit 映射时直接跳过 anchor 过滤，保留该患者全部记录，不报错不警告。

**动作**
- 缺映射时默认 `error`；提供 `on_missing_anchor = c("error", "warn", "skip")`，默认 `"error"`。
- 跳过时写入 `audit$anchor_scope`，包含表名、原因、受影响患者数。
- join 前增加真实一对多检测：对每个共享 key 子集检查 `duplicated()`，命中则报错，不再只检查单表完整 key。

**验收**
- 负例：缺 date 映射时必须报错；显式 `skip` 时 audit 中能看到跳过原因。
- 负例：两张表只在 `patient_id` 上一对多时 join 必须失败。

### 3.5 `classif_model_compare()`：正类与边界条件

**现状**：`direction = "<"` 与 `positive = levels(target)[2]` 硬编码，`"case"/"control"` 时正类会取反；预测列含 `NA` 直接崩；小样本校准分箱为 0。

**动作**
- 增加 `positive_class` 参数；未指定时按显式规则推断并 warning，不再依赖字母序。
- 统一 AUC、Brier、HL、DCA 的方向来源，只从 `positive_class` 推导。
- 预测列含 `NA` 时显式报错并给出行号/模型名。
- `n_tiles` 下限保护：`n < 20` 时使用留一法或直接给出“样本不足，无法校准”的明确错误。
- PR 图与 PRAUC 指标统一 NaN 处理策略。

**验收**
- 目标为 `"case"/"control"` 且 `positive_class = "case"` 时，AUC 与手工 `pROC` 结果一致。
- 常量预测、含 NA 预测、n = 10 三种输入均有明确错误信息。

### 3.6 `extract_history()`：新功能的边界加固

**现状**：空关键词生成 `(?:)` 匹配一切；关键词转义不完整；单字 `未`/`无` 在 20 字符窗口内判断否定，中文病历假阴性风险高。

**动作**
- 空关键词向量直接报错或返回全 `NA`，禁止 `(?:)`。
- 关键词转义改用统一转义函数（或固定匹配），覆盖 `[ ] { } ( ) - + * ? . \ ^ $ |`。
- 否定判断改为“句内否定词表 + 距离权重”，`negation_window` 可配置；补充“无明显诱因”“未使用降压药但高血压病史”等负例。
- 明确 duration 只从关键词后同一句内提取。

**验收**
- 空关键词、含元字符关键词、否定词跨句三类测试。
- 假阴性/假阳性样例集纳入回归测试。

### 3.7 1.4.4 完成定义（DoD）

- 所有 P0 项均有“先失败、后通过”的回归测试。
- `R CMD check --as-cran` 在 `develop` 上 0 error / 0 warning。
- NEWS 为每项写“行为变更”，破坏性变更给出迁移说明。
- 现有快照更新后，人工确认没有把错误行为再次固化。

---

## 四、1.4.5：统计可信度（P1）

### 4.1 `regression_scan()`：移除 p-hacking 与状态泄漏

**动作**
- 删除“按最小 p 值自动选择变量变换”的逻辑；改为输出所有候选变换的并列结果，并标记 `exploratory`。
- 主分析必须由用户预设；敏感性分析单独成表。
- `p.adjust` 跨所有候选变换族统一执行，不再族内分别校正。
- `covars` 在循环内保持不可变，每个 predictor 使用同一协变量集。
- 仅对有序因子输出 “P for trend”；名义变量只给 overall p。
- 移除包裹整个模型拟合的 `suppressWarnings()`；完全分离、收敛失败、Cox ties 写入结果表的 `diagnostics` 列。
- 明确 `cluster` 语义：稳健方差（`rms::robcov`）与 GEE 二选一，删除不可达分支或补齐实现。

**验收**
- 同一数据重复运行，结果表逐位一致（无随机/顺序依赖）。
- 构造完全分离数据时返回明确诊断信息，而不是静默系数。
- 名义 3 水平 factor 不再出现 trend 行。

### 4.2 `interactions`

- 去除 `age×sex` / `sex×age` 对称重复，按变量对唯一化。
- 按模型类选择检验：`lm` 用 F 检验，`glm`/`coxph` 用 LRT，不再统一传 `"LRT"`。
- 交互检验失败时返回 `NA` 并 warning，不静默吞掉；绘图失败时保留错误信息。

### 4.3 `subgroup_forest`

- 类型判断改为 `is.numeric(x)`；`character` 先显式转 factor。
- 子组内缺失 factor 水平时，用模型矩阵对齐水平，保证 `Level` 与 `estimate` 行数一致。
- 列重命名改为按名字操作，不再依赖 `select` 的列位置。

### 4.4 `time_roc` / `calc_cindex`

- 删除 C-index < 0.5 自动翻转；改为 warning + 返回原始值，方向异常由用户确认。
- `total_tp == 0` 时显式跳过该时间点并 warning。

### 4.5 统一模型诊断层

新增内部函数（例如 `model_diagnostics()`），统一输出：
- VIF、PH 检验（含 `GLOBAL`）、校准曲线、Brier、DCA；
- 每个回归/模型比较函数的返回对象都带 `diagnostics`，绘图函数只消费该对象。

**验收**
- 同一模型在 `regression_fit()`、`predictor_effect_plot()`、`classif_model_compare()` 中得到的 PH/校准数值一致。

### 4.6 `tidy.rms`

- 注册 `S3method(tidy, rms)` 并导出，或删除自定义实现、显式依赖 broom。
- 不允许“定义了但未注册”的死代码继续存在。

---

## 五、1.5.0：结构与发布治理

### 5.1 API 分层

- 将当前 70+ 导出收敛为三类：
  - **stable**：数据清洗、基线表、回归、模型比较等主流程（约 30–35 个）；
  - **experimental**：`extract_history`、`group_by_range` 等新功能，标注 lifecycle；
  - **internal**：`break_at`、`emp_colors`、`formula_add_covs`、`unmake_names`、`replace_elements` 等不再导出。
- 统一参数命名（`save_table` / `save_output`、`return_details` / `return_audit`）与返回类型（data.frame vs data.table）。

### 5.2 命名空间与依赖

- 保持 1.4.3 的 `importFrom` 方向，补齐剩余全量 `import()`。
- 所有可选依赖进入 `Suggests`，主流程按需 `requireNamespace()` 并提供降级路径。
- 为 Rcpp 增加“无编译环境”说明；评估是否保留 C++ 加速（收益 vs 构建成本）。

### 5.3 拆分巨型文件

| 文件 | 现状 | 拆分方向 |
|---|---|---|
| `data_screen.R` | 1077 行 | 表达式解析 / key 构建 / anchor / join / audit |
| `merge.R` | 1140 行 | 区间连接核心 / 子串连接 / 公共工具 |
| `regressions.R` | 875 行 | 拟合 / scan / forest / diagnostics |
| `misc.R` | 600+ 行 | 格式化 / 公式 / 宽表 / S3 方法 |

目标：单函数可独立测试，核心函数控制在 100 行以内。

### 5.4 发布流程

- **同步 GitHub 与 Gitee**：当前 GitHub `develop/main` 停在 1.4.0，外部用户与 CI 看不到 1.4.3。明确主仓库并在 README 标注镜像关系。
- CI 增加 `push: branches: [main, master, develop]`；PR 保留全分支触发。
- 增加一个“安装全部 Suggests”的 CI job，避免 `skip_if_not_installed` 掩盖核心路径。
- 删除 `my_test.r`；`Collate` 改为 roxygen 自动生成；增加 `_pkgdown.yml`。
- 增加 revdep 检查与 CRAN 提交前 checklist。

### 5.5 测试策略

- **数值断言优先**：基线表、回归系数、AUC/Brier、C-index 与 `tableone`/`survival`/`pROC` 手工结果比对。
- **属性测试**：单位标准化、区间连接、缺失值筛选满足不变量。
- 快照只用于图形外观，不用于统计结果；统计结果快照必须配数值断言。
- 每个 P0/P1 修复必须先有失败测试。

### 5.6 文档

- 为 stable API 增加 lifecycle badge 与迁移说明。
- vignette 增加“方法学边界”章节：哪些是预设分析、哪些是探索性、哪些结果不应直接写进论文。
- README 弱化 “significant result screening”，改为 “pre-specified clinical analysis + transparent reporting”。

---

## 六、1.6.0：方法学能力

按“临床研究最常被审稿人质疑”的顺序补齐：

1. **缺失数据**：多重插补（`mice`）+ 插补后汇总；保留完整病例分析作为敏感性分析。
2. **生存分析**：竞争风险（Fine-Gray）、时变协变量、左截断。
3. **模型验证**：bootstrap 内部校正 + 外部验证；校准与 DCA 进入标准输出。
4. **报告规范**：TRIPOD / STROBE 条目映射，生成可直接放入论文的表格说明。
5. **可复现性**：所有含随机性的函数统一 `seed` 语义，并写入返回对象的 `call`/`seed`。

---

## 七、跨版本完成定义（DoD）

每个版本发布前逐项确认：

- [ ] 新增/修改的导出函数有 `@returns`、可运行示例、输入校验。
- [ ] 静默路径全部改为“报错 / warning + audit”。
- [ ] 修复类 PR 包含先失败的回归测试。
- [ ] `R CMD check --as-cran` 0 error / 0 warning；`develop` 分支 CI 通过。
- [ ] NEWS 记录行为变更与迁移方式。
- [ ] 快照变更经人工确认，未固化错误行为。
- [ ] 文档与实现一致（尤其是默认值与“未指定时”的语义）。

---

## 八、风险与取舍

| 风险 | 应对 |
|---|---|
| 破坏性变更影响现有用户 | 1.4.4/1.4.5 只改错误行为；API 改名放到 1.5.0，并提供 deprecated 过渡 |
| Rcpp 增加构建成本 | 保留但提供纯 R fallback；或在 1.5.0 评估后移除 |
| 方法学扩展导致范围失控 | 1.6.0 每项独立开关，默认不启用 |
| Gitee/GitHub 双仓库继续分叉 | 1.5.0 前必须确定唯一主仓库并自动化镜像 |
| 继续以“显著结果”为卖点 | 文档与默认输出改为“预设分析 + 敏感性分析”，不再默认输出最优 p 值 |

---

## 九、建议的首个迭代 Backlog

按依赖顺序执行，前 6 项属于 1.4.4：

1. `unit_standardize()` 逐行单位映射 + 替换错误测试。
2. `merge.R` 删除 7 组重复定义。
3. `merge_by_substring()` 空 key/子串冲突显式报错。
4. `predictor_effect_plot()` PH 取 `GLOBAL` + `df_hist_data` 初始化。
5. `screen_data_list()` anchor 缺映射默认报错 + audit。
6. `classif_model_compare()` 增加 `positive_class` 并统一方向。
7. `extract_history()` 空关键词与否定词边界测试。
8. `regression_scan()` 移除最小 p 选变换、固定 `covars`。
9. 注册或删除 `tidy.rms`。
10. CI 增加 `develop` 触发与“全 Suggests”job。

---

## 附：路线成功标准

1.4.3 之后的改进是否成功，不看新增了多少函数，而看三件事：

- 是否还存在“不报错但结果错”的路径；
- 统计输出是否能被审稿人追溯到预设分析与敏感性分析；
- 外部用户拿到的代码是否就是最新开发线。
