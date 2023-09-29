# Covid-19の累積感染者数のコロプレス図を作成

library(tidyverse)
library(sf)

# ggplot2のフォント設定
# "IPAexGothic"フォントは https://moji.or.jp/ipafont/ から入手可能
theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 18)
)

# 作成したgeojsonファイルの読込み
# 以下のように奥村先生のホームページからダウンロードしても良い
# japan_prefecture = read_sf("https://okumuralab.org/~okumura/stat/data/japan.geojson")

japan_prefectures <- read_sf("output/japan_prefectures.geojson")

# Covid-19　都道府県別累積死亡者数データのダウンロード (厚労省のオープンデータ)

covid_deaths <- read.csv("https://covid19.mhlw.go.jp/public/opendata/deaths_cumulative_daily.csv")

# 最新の日付のデータを抽出
# Covid-19のオープンデータの更新は2023/5/9で終了 (5/9は都道府県によって欠損しているので5/8のデータを利用)

covid_latest <- covid_deaths[nrow(covid)-1,] %>% 
  select(  -ALL) %>% 
  pivot_longer(cols = -Date )

# 地理情報とマージ

covid19 <- cbind(japan_prefecture, covid_latest)

# コロプレス図作成

covid19 %>%
  ggplot(aes(fill = value)) +
  geom_sf() +
  scale_fill_continuous("死亡者数(累積)", low = "white", high = "red")
