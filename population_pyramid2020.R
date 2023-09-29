# 国勢調査を用いて2020年の人口ピラミッドの作成

library(tidyverse)
library(estatapi)

# e-statのappIDが必要
# 利用申請(無料)をすればだれでも入手できる

# appID = "入手したappIDをここに設定（行頭の#を外す）"

# ggplot2のフォント設定
# "IPAexGothic"フォントは https://moji.or.jp/ipafont/ から入手可能
theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 18)
)


# e-statからデータ取得 statID 0003445133
# 	国勢調査 令和２年国勢調査 人口等基本集計
#       （主な内容：男女・年齢・配偶関係，世帯の構成，
#           住居の状態，母子・父子世帯，国籍など）
#   表番号2-1-1
#   男女，年齢（各歳），国籍総数か日本人別人口
#       －全国，都道府県，21大都市，特別区，人口50万以上の市

estat_census2020 <- estat_getStatsData(
  appId = appID,
  statsDataId = "0003445133",
  cdCat01 = "0", # 総人口
  cdCat02 = c("1", "2"), # 男・女
  cdArea = "00000" # 全国
)

# 人口ピラミッドを描くためのデータの整理
pop2020 <- estat_census2020 %>%
  filter(
    (cat03_code != "000" &
      as.numeric(cat03_code) < 101) | # 年齢  総数と100歳を超える年齢を除く
      cat03_code == "R6" # 年齢  100歳以上は残す
  ) %>%
  mutate(
    age = case_when( # 年齢の作成
      cat03_code == "R6" ~ 100, # 100歳以上は100とする
      TRUE ~ as.numeric(cat03_code) - 1
    ), # 100再未満の年齢
    value = case_when(
      `男女` == "男" ~ -value / 1000, # 人口は1000人単位に
      `男女` == "女" ~ value / 1000
    ) # 男性の人口はマイナス
  ) %>% # (人口ピラミッドを描くため)
  select(age, `男女`, value)

# 人口ピラミッドの描画
pop2020 %>%
  ggplot(aes(x = age, y = value, fill = `男女`)) + # x軸に年齢，y軸に人口をとり性別で塗り分け(縦横は入れ替える)
  geom_bar(stat = "identity", color = "black") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(
    breaks = seq(-1000, 1000, 500),
    labels = abs(seq(-1000, 1000, 500))
  ) +
  scale_fill_hue(name = "", labels = c("F" = "女", "M" = "男")) +
  labs(y = "人口(単位：1,000人)", x = "年齢") +
  coord_flip() # グラフの回転 (縦軸と横軸の入替)

ggsave("output/population_pyramid2020.pdf", device = cairo_pdf)

