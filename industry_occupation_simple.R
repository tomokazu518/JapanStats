# 産業構造・職業構成の変化（国勢調査から）

# 国勢調査は2005年以前と2010年以降で産業・職業の分類が異なる
# ここでは，新旧分類を統合せず別々にグラフを作成する

library(tidyverse)
library(estatapi)
library(RColorBrewer)

# e-statのappIDが必要
# 利用申請(無料)をすればだれでも入手できる

# appID = "入手したappIDをここに設定（行頭の#を外す）"

# ggplot2のフォント設定
# "IPAexGothic"フォントは https://moji.or.jp/ipafont/ から入手可能
theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 18)
)

###############################
###       産業構造          ###
###############################

##### 国勢調査・時系列データ・人口の労働力状態，就業者の産業・職業　表番号4　

# 産業（大分類），男女別15歳以上就業者数
# －全国（平成7年～平成27年）※平成19年11月改訂後
industry_latest <-
  estat_getStatsData(appId = appID,
                     statsDataId = "0003410395",
                     cdTab = 105,     # 表章項目 == "割合"
                     cdCat01From = 120, # 産業分類
                     cdCat01To   = 330, # 　総数，小計，再掲などを除く
                     cdCat02     = 100  # 男女_時系列 == "総数"
  ) %>%
  mutate(year = as.numeric(time_code) / 1000000,
         industry = `産業大分類2015`) %>%
  select(year, industry, value)

#【参考】産業（旧大分類），男女別15歳以上就業者数及び産業別割合
# －全国（大正9年～平成12年）※平成14年3月改訂前
industry_old <- 
  estat_getStatsData(appId = appID,
                     statsDataId = "0003410396",
                     cdTab = 1260,      # 表章項目 == "産業別割合"
                     cdCat01From = 120, # 産業分類
                     cdCat01Tob  = 330, # 　総数を除く
                     cdCat02     = 100  # 男女_時系列 == "総数"
  ) %>% 
  filter(cat01_code != 150 & cat01_code != 190) %>%  # 小計(第n次産業)を除く
  mutate(year = as.numeric(time_code) / 1000000,
         industry = `産業大分類（平成14年3月改訂前）`) %>%
  select(year, industry, value)

#### グラフ

clr <- c(brewer.pal(11, "Paired"), brewer.pal(8, "YlOrBr"), "#FFFFFF")

## 1995年以降（新産業分類）

industry_latest %>%
  ggplot(aes(x = year, y = value, fill = industry)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(name = "産業", values = clr) +
  labs(x = "年", y = "")

ggsave("output/industry_latest.pdf", device = cairo_pdf)

## 2000年以前（旧産業分類）

industry_old %>%
  ggplot(aes(x = year, y = value, fill = industry)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(name = "産業", values = clr) +
  labs(x = "年", y = "")

ggsave("output/industry_old.pdf", device = cairo_pdf)


###############################
###       職業構造          ###
###############################

###### 国勢調査・時系列データ・人口の労働力状態，就業者の産業・職業　表番号7

# 職業（大分類），男女別15歳以上就業者数－全国（平成7年～平成27年）
occupation_latest <-
  estat_getStatsData(appId = appID,
                     statsDataId = "0003410408",
                     cdTab = 1280,      # 表章項目 == "割合"
                     cdCat01From = 110, # 職業分類
                     cdCat01To   = 220, # 
                     cdCat02     = 100  # 男女_時系列 == "総数")
  ) %>% 
  mutate(year = as.numeric(time_code) / 1000000,
         occupation = `職業大分類2015`) %>%
  select(year, occupation, value)

# 【参考】職業（旧大分類），男女別15歳以上就業者数及び産業別割合
# －全国（昭和25年～平成17年）※平成21年12月改訂前
occupation_old <- 
  estat_getStatsData(appId = appID, 
                     statsDataId = "0003410409",
                     cdTab = 1280,      # 表章項目 == "割合"
                     cdCat01From = 110, # 職業分類
                     cdCat01To   = 200, # 
                     cdCat02     = 100  # 男女_時系列 == "総数")#%>%)%>%
  ) %>%
  mutate(year = as.numeric(time_code) / 1000000,
         occupation = `職業大分類（旧大分類H21改定前）`) %>%
  select(year, occupation, value)


#### グラフ

clr <- c(brewer.pal(7, "Accent"), brewer.pal(5, "YlOrRd"), "#FFFFFF")

## 1995年以降（新職業分類）

occupation_latest %>%
  ggplot(aes(x = year, y = value, fill = occupation)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(name = "職業", values = clr) +
  labs(x = "年", y = "") 

ggsave("output/occupation_latest.pdf", device = cairo_pdf)

## 1990年以前(旧職業分類)

occupation_old %>%
  ggplot(aes(x = year, y = value, fill = occupation)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(name = "職業", values = clr) +
  labs(x = "年", y = "") 

ggsave("output/occupation_old.pdf", device = cairo_pdf)



