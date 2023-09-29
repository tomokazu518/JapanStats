# 労働力調査を用いて労働力人口の推移，年齢階級別労働力率・失業率の推移，
# 労働供給のM字カーブのグラフを作成する。

library(tidyverse)
library(estatapi)
library(readxl)
library(RColorBrewer)
library(patchwork)

# e-statのappIDが必要
# 利用申請(無料)をすればだれでも入手できる

# appID = "入手したappIDをここに設定（行頭の#を外す）"

# ggplot2のフォント設定
# "IPAexGothic"フォントは https://moji.or.jp/ipafont/ から入手可能

theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 18)
)

####### データ取得

# 総務省人口推計　1968年以降の各年10月1日総人口

# 人口推計　2019年　参考表2　男女別人口－総人口(各年10月1日現在)
download.file(
  "https://www.stat.go.jp/data/jinsui/2019np/zuhyou/05k01-5.xlsx",
  destfile = "files/pop_estimate.xlsx",
  method = "curl"
)

# 人口推計　令和3年10月確定値　（参考表）全国人口の推移
download.file(
  "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032179523&fileKind=4",
  destfile = "files/pop_latest.xlsx",
  method = "curl"
)

pop_1 <- read_excel("files/pop_estimate.xlsx", range = "E24:E60", col_names = F)
pop_2 <- read_excel("files/pop_estimate.xlsx", range = "L6:L20", col_names = F)
pop_3 <- read_excel("files/pop_latest.xlsx", range = "D19:D20", col_names = F)
population <- rbind(pop_1, pop_2, pop_3 / 1000) / 10
year <- seq(1968, 2021, 1)

population <- cbind(year, population) %>%
  mutate(category = "総人口") %>%
  rename(value = ...1)

# 労働力調査　基本集計　全都道府県　年次　表番号1-1-5

labor_force <- estat_getStatsData(
  appId = appID,
  statsDataId = "0002060047",
  cdCat01 = "000", # 全産業
  cdCat02 = c("1", "2", "0"), # 男・女
  cdCat03 = c("00", "01", "08"), # 15歳以上人口，労働力人口，完全失業者
  cdArea = "00000", # 全国
  cdTimeFrom = 1968
) %>%
  mutate(year = as.numeric(time_code) / 1000000) %>%
  select(year, `性別`, `就業状態`, `年齢階級`, `value`)



####### 総人口，15歳以上人口，生産年齢人口，労働力人口の長期的な推移

# データ整理
population_laborforce <- labor_force %>%
  mutate(
    category = case_when(
      `就業状態` == "15歳以上人口" & `年齢階級` == "15歳以上" ~ "15歳以上人口",
      `就業状態` == "15歳以上人口" & `年齢階級` == "15～64歳" ~ "生産年齢人口",
      `就業状態` == "労働力人口" & `年齢階級` == "15～64歳" ~ "労働力人口",
      TRUE ~ ""
    ),
    category = factor(category,
      levels = c(
        "総人口", "15歳以上人口",
        "生産年齢人口", "労働力人口"
      )
    )
  ) %>%
  filter(`性別` == "総数" & category != "") %>%
  select(year, value, category)

population_laborforce <- rbind(population_laborforce, population)

# グラフ作成
population_laborforce %>%
  ggplot(aes(x = year, y = value, color = category, shape = category)) +
  geom_line() +
  geom_point(size = 2) +
  scale_color_discrete(name = "") +
  scale_shape_discrete(name = "") +
  labs(x = "年", y = "人口(万人)")

ggsave("output/population_laborforce.pdf", device = cairo_pdf)


####### 年齢階級別労働力率

# データ整理
lf_by_age <- labor_force %>%
  pivot_wider(names_from = `年齢階級`) %>%
  mutate(`25～54歳` = `25～34歳` + `35～44歳` + `45～54歳`) %>% # 25~54歳までをまとめる
  pivot_longer(-c("year", "性別", "就業状態"), names_to = "年齢階級") %>%
  pivot_wider(names_from = `就業状態`) %>%
  mutate(
    participation_rate = `労働力人口` / `15歳以上人口`,
    unemployment_rate = `完全失業者` / `労働力人口`
  )


# グラフ作成
age_groups <- c("15～24歳", "25～54歳", "55～59歳", "60～64歳", "65歳以上")

g1 <- lf_by_age %>%
  filter(`性別` == "男" & `年齢階級` %in% age_groups) %>%
  ggplot(aes(
    x = year, y = participation_rate,
    color = `年齢階級`, shape = `年齢階級`
  )) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  labs(title = "男", x = "年", y = "労働力率") +
  theme(legend.position = "none")

g2 <- lf_by_age %>%
  filter(`性別` == "女" & `年齢階級` %in% age_groups) %>%
  ggplot(aes(
    x = year, y = participation_rate,
    color = `年齢階級`, shape = `年齢階級`
  )) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  labs(title = "女", x = "年", y = "")

plot(g1 + g2)

ggsave("output/labor_participation_by_age.pdf", device = cairo_pdf)

####### 年齢階級別失業率の推移

# グラフ作成
age_groups <- c("15～24歳", "25～34歳", "35～44歳", "45～54歳")

lf_by_age %>%
  filter(`性別` == "総数" & `年齢階級` %in% age_groups) %>%
  ggplot(aes(
    x = year, y = unemployment_rate,
    color = `年齢階級`, shape = `年齢階級`
  )) +
  geom_line() +
  geom_point() +
  labs(x = "年", y = "失業率")

ggsave("output/unemployment_by_age.pdf", device = cairo_pdf)

## M字カーブ

# データ整理
years <- c(1970, 1990, 2010, 2020)
age_groups <- c(
  "15～19歳", "20～24歳", "25～29歳", "30～34歳", "35～39歳",
  "40～44歳", "45～49歳", "50～54歳", "55～59歳", "60～64歳"
)

m_curve <- lf_by_age %>%
  filter(
    (`性別` == "女" & year %in% years & `年齢階級` %in% age_groups) | # 女性
      (`性別` == "男" & year %in% c(years[1], years[length(years)]) &
        `年齢階級` %in% age_groups) # 男性は最初と最後の年のみ
  ) %>%
  mutate(`性別・年` = paste(`性別`, "(", year, ")", sep = ""))

# グラフ作成
m_curve %>%
  ggplot(aes(
    x = `年齢階級`, y = participation_rate, color = `性別・年`,
    linetype = `性別・年`, shape = `性別・年`, group = `性別・年`
  )) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c(brewer.pal(4, "Set1"), "grey", "grey")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_linetype_manual(values = c(
    "solid", "solid", "solid",
    "solid", "dotted", "dotted"
  )) +
  labs(y = "労働力率")

ggsave("output/m_curve.pdf", device = cairo_pdf)
