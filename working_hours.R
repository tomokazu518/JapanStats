# 毎月勤労統計の長期時系列表を使って，就業形態別に総実労働時間数の推移をグラフにする

library(tidyverse)

# ggplot2のフォント設定
# "IPAexGothic"フォントは https://moji.or.jp/ipafont/ から入手可能

theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 12)
)

# 毎月勤労統計調査		長期時系列表	実数・指数累積データ	
# 表番号1 実数・指数累積データ　実数

maikin <-
  read.csv("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032189776&fileKind=1",
                   fileEncoding = "shift-jis") %>% 
  filter(`年` >= 1993 & 
           `月` == "CY" & 
           substr(`産業分類`, 1, 2) == "TL" &
           `規模` == "T")

w_status <- c("就業形態計", "一般労働者", "パートタイム労働者")

maikin %>% 
  ggplot(aes(x=`年`, y=`総実労働時間` , color = as.factor(`就業形態`))) +
  geom_line() +
  geom_point() +
  scale_color_hue(name = "就業形態", labels = w_status)

ggsave("output/working_hours.pdf", device = cairo_pdf)
