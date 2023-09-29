### e-statから四半期GDP速報と国民経済計算年次推計のデータを入手してグラフを描く

library(tidyverse)
library(estatapi)
library(patchwork)
library(RColorBrewer)

# e-statのappIDが必要
# 利用申請(無料)をすればだれでも入手できる

# appID = "入手したappIDをここに設定（行頭の#を外す）"

# ggplot2のフォント設定
# "IPAexGothic"フォントは https://moji.or.jp/ipafont/ から入手可能

theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 12)
)


#$ データ取得
# 四半期GDP速報のstatsDataIdは　"0003109750"
qgdp <- estat_getStatsData(appId = appID, statsDataId = "0003109750")

## データの整理

# 必要なものだけ残す

list <- c(
  "国内総生産(支出側)",
  "民間最終消費支出", "民間企業設備", "民間住宅", "民間在庫変動",
  "政府最終消費支出", "公的固定資本形成", "公的在庫変動",
  "財貨・サービス_純輸出", "開差"
)

QuarterlyGDP <- qgdp %>%
  filter(国内総生産_実質季節調整系列 %in% list) %>%
  select(国内総生産_実質季節調整系列, time_code, value)


# 列名変更

colnames(QuarterlyGDP) <- c("variable", "time_code", "value")


# データ作成

QuarterlyGDP <- QuarterlyGDP %>%
  pivot_wider(names_from = variable) %>% # wide型に変換
  mutate(
    number = row_number(), # 四半期に通し番号を振る
    year = str_sub(time_code, 1, 4), # time_codeから年の部分だけを抽出(グラフ作成時にラベルとして利用)
    # 扱いやすいように変数名を英数字に
    gdp = `国内総生産(支出側)`,
    consumption = `民間最終消費支出`,
    equip = `民間企業設備`,
    housing = `民間住宅`,
    stock = `民間在庫変動`,
    private_investment = equip + housing + stock,
    government = `政府最終消費支出`,
    pub_capital = `公的固定資本形成`,
    pub_stock = `公的在庫変動`,
    public_investment = pub_capital + pub_stock,
    net_export = `財貨・サービス_純輸出`,
    error = `開差`,
    # 経済成長率，寄与度の計算
    growth = gdp / lag(gdp, 1) - 1,
    contF_consumption =
      (consumption - lag(consumption, 1)) / lag(gdp, 1),
    contE_private_investment =
      (private_investment - lag(private_investment, 1)) / lag(gdp, 1),
    contD_government =
      (government - lag(government, 1)) / lag(gdp, 1),
    contC_public_investment =
      (public_investment - lag(public_investment, 1)) / lag(gdp, 1),
    contB_net_export =
      (net_export - lag(net_export, 1)) / lag(gdp, 1),
    contA_error =
      (error - lag(error, 1)) / lag(gdp, 1)
    # 寄与度は積み上げ棒グラフの順でA~Fの記号を振る
  ) %>%
  select(time_code, 12:32) # 変数名が漢字の元の変数を削除


## グラフ作成

# グラフ作成に必要な経済成長率と寄与度だけ残してlong型に変換
Growth <- select(
  QuarterlyGDP, number, year, time_code, growth,
  contF_consumption, contE_private_investment, contD_government,
  contC_public_investment, contB_net_export, contA_error
) %>%
  pivot_longer(!c(time_code, number, year))

# 色覚の多様性に配慮した配色
cbPalette <-
  c(
    "#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  )

# グラフの凡例
legends <- c(
  contA_error = "開差",
  contB_net_export = "純輸出",
  contC_public_investment = "公的投資",
  contD_government = "政府支出",
  contE_private_investment = "民間投資",
  contF_consumption = "民間消費"
)

# グラフを描く期間の指定
start <- c(1, 41, 81) # 期間の始めと終わりを通し番号で指定
end <- c(40, 80, 118) # startは必ず第一四半期(4の倍数+1)になるように

# グラフの作成
for (i in 1:length(start)) {
  file_name <- paste("output/quarterly_gdp-", i, ".pdf", sep = "")

  # 期間の限定

  GraphData <- filter(Growth, number >= start[i] & number <= end[i])

  # グラフ作成のためデータを成長率と寄与度に分ける
  # （もっとスマートな方法があるかもしれない）

  GraphGrowth <- filter(GraphData, name == "growth")
  GraphContribution <- filter(GraphData, name != "growth")

  # x軸の目盛りラベル（各年の第一四半期にのみラベル）

  xlabels <- paste(GraphGrowth$year[seq(1, end[i] - start[i] + 1, by = 4)], "Q1", sep = "")

  # ggplot

  ggplot(GraphContribution) +
    geom_bar(
      stat = "identity", color = "black", width = 0.7, # 積み上げ棒グラフ
      aes(x = number, y = value, fill = name)
    ) +
    scale_fill_manual(name = "寄与度", values = cbPalette, labels = legends) + # 色と凡例の設定
    geom_line(
      data = GraphGrowth,
      aes(x = number, y = value, color = name), size = 0.8
    ) + # 折れ線グラフ
    geom_point(
      data = GraphGrowth,
      aes(x = number, y = value, color = name)
    ) + # マーカー
    scale_color_manual(name = "", values = "red", labels = "経済成長率") + # 色と凡例の設定
    scale_x_continuous(
      name = "", # x軸のラベルの設定
      breaks = seq(start[i], end[i], by = 4),
      labels = xlabels
    ) +
    scale_y_continuous(name = "", limits = c(-0.1, 0.1)) +
    ggtitle(paste("四半期GDP速報"))

  ggsave(file_name, device = cairo_pdf)
}
