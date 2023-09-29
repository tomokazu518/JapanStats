# 産業構造・職業構成の変化（国勢調査から）

# 国勢調査は2005年以前と2010年以降で産業・職業の分類が異なる
# ここでは，新旧分類を統合してグラフを作成する

library(tidyverse)
library(estatapi)
library(RColorBrewer)

# e-statのappIDが必要
# 利用申請(無料)をすればだれでも入手できる

# appID = "入手したappIDをここに設定（行頭の#を外す）"

# ggplot2のフォント設定
# "IPAexGothic"フォントは https://moji.or.jp/ipafont/ から入手可能
# 文字化けしなければとくに指定しなくても良い

theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 18)
)


###############################
###       産業構造          ###
###############################

# e-stat 国勢調査　時系列統計
# 人口の労働力状態，就業者の産業・職業　表番号4　

#【参考】産業（旧大分類），男女別15歳以上就業者数及び産業別割合
# －全国（大正9年～平成12年）※平成14年3月改訂前
industry_before1995 <- 
  estat_getStatsData(appId = appID,
                     statsDataId = "0003410396") %>%
  filter(
    `表章項目` == "産業別割合" &
      `男女_時系列` == "総数" &
      cat01_code >= 120 & cat01_code <= 270 & # 総数を除く
      cat01_code != 150 & cat01_code != 190 & # 小計(第n次産業)を除く
      as.numeric(time_code) / 1000000 < 1995 # 1995年は除外(新分類を利用)
  ) %>%
  mutate(year = as.numeric(time_code) / 1000000,
         industry = `産業大分類（平成14年3月改訂前）`) %>%
  select(year, industry, value)

# 産業（大分類），男女別15歳以上就業者数
# －全国（平成7年～平成27年）※平成19年11月改訂後
industry_since1995 <-
  estat_getStatsData(appId = appID,
                     statsDataId = "0003410395") %>%
  filter(`表章項目` == "割合" &
           `男女_時系列` == "総数" &
           cat01_code >= 120 & cat01_code <= 330) %>% # 総数，小計(第n次産業)を除く
  mutate(year = as.numeric(time_code) / 1000000,
         industry = `産業大分類2015`) %>%
  select(year, industry, value)



# 新旧産業分類の統合

industries <-
  c(
    "農林漁業",
    "鉱業",
    "建設業",
    "製造業",
    "電気・ガス・熱供給・水道業",
    "情報通信業",
    "運輸業(1990年以前は通信業含む)",
    "卸売・小売(1990年以前は飲食店含む)",
    "金融・保険業",
    "不動産業(1995年以降は物品賃貸業含む)",
    "サービス業(1995年以前)",
    "学術研究，専門・技術サービス業",
    "宿泊業，飲食サービス業",
    "生活関連サービス業，娯楽業",
    "教育，学習支援業",
    "医療，福祉",
    "複合サービス事業",
    "その他のサービス業",
    "公務",
    "分類不能"
  )



## 1990年以前（旧産業分類）

industry_1 <- industry_before1995 %>%
  pivot_wider(names_from = industry) %>%
  mutate(
    `農林漁業` = `A農業` + `B林業` + `C漁業`,
    `鉱業` = `D鉱業`,
    `建設業` = `E建設業`,
    `製造業` = `F製造業`,
    `電気・ガス・熱供給・水道業` = `G電気・ガス・熱供給・水道業`,
    `運輸業(1990年以前は通信業含む)` = `H運輸・通信業`,
    `卸売・小売(1990年以前は飲食店含む)` = `I卸売・小売業，飲食店`,
    `金融・保険業` = `J金融・保険業`,
    `不動産業(1995年以降は物品賃貸業含む)` = `K不動産業`,
    `サービス業(1995年以前)` = `Lサービス業`,
    `公務` = `M公務(他に分類されないもの)`,
    `分類不能` = `N分類不能の産業`
  ) %>%
  select(year, `農林漁業`:`分類不能`) %>%
  pivot_longer(-year, names_to = "industry") %>%
  mutate(industry = factor(industry, levels = industries))


## 1995年以降（新産業分類）

industry_2  <- industry_since1995  %>%
  pivot_wider(names_from = industry) %>%
  mutate(
    `農林漁業` = `Ａ農業，林業` + `Ｂ漁業`,
    `鉱業` = `Ｃ鉱業，採石業，砂利採取業`,
    `建設業` = `Ｄ建設業`,
    `製造業` = `Ｅ製造業`,
    `電気・ガス・熱供給・水道業` = `Ｆ電気・ガス・熱供給・水道業`,
    `情報通信業` = `Ｇ情報通信業`,
    `運輸業(1990年以前は通信業含む)` = `Ｈ運輸業，郵便業`,
    `卸売・小売(1990年以前は飲食店含む)` = `Ｉ卸売業，小売業`,
    `金融・保険業` = `Ｊ金融業，保険業`,
    `不動産業(1995年以降は物品賃貸業含む)` = `Ｋ不動産業，物品賃貸業`,
    `学術研究，専門・技術サービス業` = `Ｌ学術研究，専門・技術サービス業`,
    `宿泊業，飲食サービス業` = `Ｍ宿泊業，飲食サービス業`,
    `生活関連サービス業，娯楽業` = `Ｎ生活関連サービス業，娯楽業`,
    `教育，学習支援業` = `Ｏ教育，学習支援業`,
    `医療，福祉` = `Ｐ医療，福祉`,
    `複合サービス事業` = `Ｑ複合サービス事業`,
    `その他のサービス業` = `Ｒサービス業（他に分類されないもの）`,
    `公務` = `Ｓ公務（他に分類されるものを除く）`,
    `分類不能` = `Ｔ分類不能の産業`
  ) %>%
  select(year, `農林漁業`:`分類不能`) %>%
  pivot_longer(-year, names_to = "industry") %>%
  mutate(industry = factor(industry, levels = industries))

#### Plot

industry <- rbind(industry_1, industry_2)

clr <- c(brewer.pal(11, "Paired"), brewer.pal(8, "YlOrBr"), "#FFFFFF")

industry %>%
  ggplot(aes(x = year, y = value, fill = industry)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(name = "産業", values = clr) +
  labs(x = "年", y = "")


###############################
###       職業構造          ###
###############################

# e-stat 国勢調査　時系列統計
# 人口の労働力状態，就業者の産業・職業　表番号7

# 【参考】職業（旧大分類），男女別15歳以上就業者数及び産業別割合－全国（昭和25年～平成17年）※平成21年12月改訂前
occupation_before1995 <-
  estat_getStatsData(appId = appID,
                     statsDataId = "0003410409") %>% 
  filter(
    `表章項目` == "職業別割合" &
      `男女_時系列` == "総数" &
      cat01_code >= 110 & cat01_code <= 200 &
      as.numeric(time_code) / 1000000 < 1995
  ) %>%
  mutate(year = as.numeric(time_code) / 1000000,
         occupation = `職業大分類（旧大分類H21改定前）`) %>%
  select(year, occupation, value)

# 職業（大分類），男女別15歳以上就業者数－全国（平成7年～平成27年）
occupation_since1995 <- 
  estat_getStatsData(appId = appID,
                     statsDataId = "0003410408") %>% 
  filter(`表章項目` == "職業別割合" &
         `男女_時系列` == "総数" &
         cat01_code >= 110 & cat01_code <= 220 
  ) %>%
  mutate(year = as.numeric(time_code) / 1000000,
         occupation = `職業大分類2015`) %>%
  select(year, occupation, value)



# 新旧統合分類を作成

occupations <- 
  c(
    "専門的・技術的職業",
    "管理的職業",
    "事務職",
    "販売職",
    "サービス職",
    "保安職",
    "農林漁業作業",
    "運輸・通信(1990年以前)",
    "輸送・機械運転",
    "生産工程(1990年以前は労務作業を含む)",
    "建設・採掘",
    "運搬・清掃・包装等",
    "分類不能"
    )

## 1990年以前

occupation_1 <- occupation_before1995 %>%
  pivot_wider(names_from = occupation) %>%
  mutate(
    `専門的・技術的職業` = `A専門的・技術的職業従事者`,
    `管理的職業` = `B管理的職業従事者`,
    `事務職` = `C事務従事者`,
    `販売職` = `D販売従事者`,
    `サービス職` = `Eサービス職業従事者`,
    `保安職` = `F保安職業従事者`,
    `農林漁業作業` = `G農林漁業作業者`,
    `運輸・通信(1990年以前)` = `H運輸・通信従事者`,
    `生産工程(1990年以前は労務作業を含む)` = `I生産工程・労務作業者`,
    `分類不能` = `J分類不能の職業`
  ) %>% 
  select(year, `専門的・技術的職業`:`分類不能`) %>%
  pivot_longer(-year, names_to = "occupation") %>%
  mutate(occupation = factor(occupation, levels = occupations))

## 1995年以降

occupation_2 <- occupation_since1995 %>%
  pivot_wider(names_from = occupation) %>%
  mutate(
    `専門的・技術的職業` = `Ｂ専門的・技術的職業従事者`,
    `管理的職業` = `Ａ管理的職業従事者`,
    `事務職` = `Ｃ事務従事者`,
    `販売職` = `Ｄ販売従事者`,
    `サービス職` = `Ｅサービス職業従事者`,
    `保安職` = `F保安職業従事者`,
    `農林漁業作業` = `Ｇ農林漁業従事者`,
    `輸送・機械運転` = `Ｉ輸送・機械運転従事者`,
    `建設・採掘` = `Ｊ建設・採掘従事者`,
    `運搬・清掃・包装等` = `Ｋ運搬・清掃・包装等従事者`,
    `生産工程(1990年以前は労務作業を含む)` = `Ｈ生産工程従事者`,
    `分類不能` = `Ｌ分類不能の職業`
  ) %>% 
  select(year, `専門的・技術的職業`:`分類不能`) %>%
  pivot_longer(-year, names_to = "occupation") %>%
  mutate(occupation = factor(occupation, levels = occupations))


# Plot

occupation <- rbind(occupation_1, occupation_2)

clr <- c(brewer.pal(7, "Accent"), brewer.pal(5, "YlOrRd"), "#FFFFFF")

occupation %>%
  ggplot(aes(x = year, y = value, fill = occupation)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(name = "職業", values = clr) +
  labs(x = "年", y = "") +
  theme_bw(base_family = "IPAexGothic") +
  theme(text = element_text(size = 14))
