# 国立社会保障・人口問題研究所からデータをダウンロードして
# 人口ピラミッドの推移を描く

library(tidyverse)
library(patchwork)
library(readxl)

# ggplot2のフォント設定
# "IPAexGothic"フォントは https://moji.or.jp/ipafont/ から入手可能

theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 12)
)

# 国立社会保障・人口問題研究所のホームページからデータをダウンロード
download.file("https://www.ipss.go.jp/site-ad/TopPageData/pyramidDataPP2023J_11.xlsx",
  destfile = "files/pyramidDataPP2023J_11.xlsx",
  method = "curl"
)

# データの整理 年，年齢階級，性別，人口のlong形式に
Male <- read_excel("files/pyramidDataPP2023J_11.xlsx",
  sheet = "M", range = "B3:W110", col_names = T
)
Female <- read_excel("files/pyramidDataPP2023J_11.xlsx",
  sheet = "F", range = "B3:W110", col_names = T
)
age <- paste(c("total", seq(0, 105, 1)))

Male <- cbind(age, Male) %>%
  pivot_longer(-age, names_to = c("year")) %>%
  filter(age != "total") %>%
  mutate(
    gender = "M",
    age = as.numeric(age),
    value = -value
  )

Female <- cbind(age, Female) %>%
  pivot_longer(-age, names_to = c("year")) %>%
  filter(age != "total") %>%
  mutate(
    gender = "F",
    age = as.numeric(age)
  )

population <- rbind(Male, Female)

# 人口ピラミッドを作成する年を指定
years <- c(1965, 1980, 1995, 2010, 2040, 2065)


# 人口ピラミッドの描画
for (i in years) {
  fig <- population %>%
    filter(year == i) %>%
    ggplot(aes(x = age, y = value, fill = gender)) +
    geom_bar(stat = "identity", color = "black") +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(
      limits = c(-1250, 1250),
      breaks = seq(-1000, 1000, 500),
      labels = abs(seq(-1000, 1000, 500))
    ) +
    scale_fill_hue(
      name = "",
      labels = c("F" = "女", "M" = "男")
    ) +
    labs(title = i, y = "", x = "") +
    coord_flip() +
    theme(legend.position = "none")

  if (i == years[1]) {
    g <- fig
  } else {
    g <- g + fig
  }
}

plot(g)

ggsave("output/population_pyramids.pdf", device = cairo_pdf)
