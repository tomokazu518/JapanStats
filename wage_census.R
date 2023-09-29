# 賃金構造基本統計調査を用いて，学歴間賃金格差，大学卒の企業規模間賃金格差をグラフにする

library(tidyverse)
library(readxl)
library(patchwork)

# ggplot2のフォント設定
# "IPAexGothic"は https://moji.or.jp/ipafont/ から入手可能
# 文字化けしなければとくに指定しなくても良い

theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 12)
)

# 賃金構造基本統計調査

## 令和4年　標準労働者
download.file(
  "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000040029203&fileKind=4",
  destfile = "files/wage_census_r4.xlsx",
  method = "curl"
)


# 学歴別賃金プロファイル

## Excelファイルの読み込み
adress <- c(
  "D370:E417", "D523:E570", "D574:E621",
  "D676:E723", "D829:E876", "D880:E927"
)
gender <- c(
  "M", "M", "M",
  "F", "F", "F"
)
edu <- c(
  "高校卒", "大学卒", "大学院卒",
  "高校卒", "大学卒", "大学院卒"
)

for (i in 1:6) {
  temp <-
    read_excel("files/wage_census_r4.xlsx",
      sheet = "産業計", range = adress[i], col_names = F
    ) %>%
    mutate(
      gender = gender[i],
      education = factor(
        edu[i],
        levels = c("高校卒", "大学卒", "大学院卒")
      )
    )

  if (i == 1) {
    wage_profile <- temp
  } else {
    wage_profile <- rbind(wage_profile, temp)
  }
}

wage_profile <-
  rename(wage_profile, regular_wage = ...1, bonus = ...2) %>%
  mutate(
    regular_wage = as.numeric(regular_wage),
    bonus = as.numeric(bonus),
    yearly_income = (regular_wage * 12 + bonus) / 10
  )

age <- rep(seq(18, 65, 1), 6)
wage_profile <- cbind(age, wage_profile)

# Plot

g1 <- wage_profile %>%
  filter(gender == "M" & age <= 60) %>%
  ggplot(aes(x = age, y = yearly_income, color = education)) +
  geom_line() +
  geom_point() +
  ylim(200, 1500) +
  scale_color_hue(name = "学歴") +
  labs(title = "男", x = "年齢", y = "年収(万円)") +
  theme(legend.position = "none")

g2 <- wage_profile %>%
  filter(gender == "F" & age <= 60) %>%
  ggplot(aes(x = age, y = yearly_income, color = education)) +
  geom_line() +
  geom_point() +
  ylim(200, 1500) +
  scale_color_hue(name = "学歴") +
  labs(title = "女", x = "年齢", y = "")

plot(g1 + g2)

ggsave("output/wage_profile_by_education.pdf",  device = cairo_pdf)

# 大卒の企業規模別賃金プロファイル

adress <- c(
  "G527:H570", "J527:K570", "M527:N570",
  "G833:H876", "J833:K876", "M833:N876"
)
gender <- c(
  "M", "M", "M",
  "F", "F", "F"
)
fs <- c(
  "1000人以上", "100～999人", "10～99人",
  "1000人以上", "100～999人", "10～99人"
)

for (i in 1:6) {
  temp <-
    read_excel("files/wage_census_r4.xlsx",
      sheet = "産業計", range = adress[i], col_names = F
    ) %>%
    mutate(
      gender = gender[i],
      firm_size = factor(
        fs[i],
        levels = c("1000人以上", "100～999人", "10～99人")
      )
    )

  if (i == 1) {
    wage_univ_firmsize <- temp
  } else {
    wage_univ_firmsize <- rbind(wage_univ_firmsize, temp)
  }
}

wage_univ_firmsize <-
  rename(wage_univ_firmsize, regular_wage = ...1, bonus = ...2) %>%
  mutate(
    regular_wage = as.numeric(regular_wage),
    bonus = as.numeric(bonus),
    yearly_income = (regular_wage * 12 + bonus) / 10
  )

age <- rep(seq(22, 65, 1), 6)
wage_univ_firmsize <- cbind(age, wage_univ_firmsize)

g3 <- wage_univ_firmsize %>%
  filter(gender == "M" & age <= 60) %>%
  ggplot(aes(x = age, y = yearly_income, color = firm_size)) +
  geom_line() +
  geom_point() +
  ylim(200, 1200) +
  scale_color_hue(name = "企業規模") +
  labs(title = "男", x = "年齢", y = "年収(万円)") +
  theme(legend.position = "none")

g4 <- wage_univ_firmsize %>%
  filter(gender == "F", age <= 60) %>%
  ggplot(aes(x = age, y = yearly_income, color = firm_size)) +
  geom_line() +
  geom_point() +
  ylim(200, 1200) +
  scale_color_hue(name = "企業規模") +
  labs(title = "女", x = "年齢", y = "")

plot(g3 + g4)

ggsave("output/wage_profile_by_firmsize.pdf",  device = cairo_pdf)
