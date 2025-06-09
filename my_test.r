set.seed(1)
load_all()
library(knitr)
library(dtplyr)
library(clinpubr)
# logistic model with time not assigned
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2
p=rcs_plot(cancer, x = "age", y = "dead", covars = "ph.karno", save_plot = TRUE, add_hist = F, trans = "log10")

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  # coord_trans(x = "log10", y = "reverse") +
  scale_y_continuous(expand = expansion(c(0.1, 0.3))) +
  theme_minimal()

library(ggplot2)
library(grid)  # 基础包无需额外安装

p <- ggplot(mtcars, aes(mpg, wt)) + 
  geom_point()

p + 
  # coord_cartesian(clip = "off")+ 
  scale_x_continuous(trans = "log10") + 
  annotation_custom(
    grob = grid::textGrob("固定文本", 
                          x = unit(0.2, "npc"),  # 水平1/5位置
                          y = unit(0.8, "npc"),  # 垂直1/5位置（NPC坐标0=底部,1=顶部）
                          hjust = 0.5, vjust = 0.5)
  ) + 
  scale_y_reverse()