library(tidyverse)
library(estatapi)
library(patchwork)
library(RColorBrewer)

# e-statのappIDが必要
# 利用申請(無料)をすればだれでも入手できる

appID = "350cf4b7b2dfe33a910d91a083ded45a3f87595e"

# ggplot2のフォント設定
# "IPAexGothic"は https://moji.or.jp/ipafont/ から入手可能
# 文字化けしなければとくに指定しなくても良い

theme_set(
  theme_bw(base_family = "IPAexGothic", base_size = 18)
)

##### 平成29年就業構造基本統計調査

# 人口・就業に関する統計表
# 表番号06700
# 男女，配偶関係，所得（主な仕事からの年間収入・収益），年齢，就業調整の有無，現職の従業上の地位・雇用形態別人口（非正規の職員・従業員）－全国

shugyo <-
  estat_getStatsData(appId = appID, statsDataId = "0003222504") 


manup <- shugyo %>% 
  filter(cat01_code == 2 &
           cat04_code == "00" &
           cat06_code == 0)　 %>% 
  select(配関,所得, 調整有無, value) %>% 
  pivot_wider(names_from = 調整有無) 

single <- cbind(
  select(filter(manup, 配関=="総数"), 1:2),
  select(filter(manup, 配関=="総数"), 3:5) -
  select(filter(manup, 配関!="総数"), 3:5)
) %>% 
  mutate(配関="配偶者なし")

married <- filter(manup, 配関=="うち配偶者あり") %>%
  mutate(配関="配偶者あり")

married <- married %>%  
  mutate(yes = 就業調整をしている / 総数,
         no  = 就業調整をしていない / 総数,
         unknown = 1 - yes - no,
         所得 = fct_inorder(所得))

single <- single %>%  
  mutate(yes = 就業調整をしている / 総数,
         no  = 就業調整をしていない / 総数,
         unknown = 1 - yes - no,
         所得 = fct_inorder(所得))
  
graph_married <- married %>% 
  ggplot(aes(x = 所得, y = yes))+
  geom_bar(stat = "identity") +
  ylim(0, 0.6) +
  labs(x = "所得", y="", title = "配偶者あり") +
  coord_flip()

graph_single <- single %>% 
  ggplot(aes(x = 所得, y = yes))+
  geom_bar(stat = "identity") +
  ylim(0, 0.6) +
  labs(x = "", y = "", title = "配偶者なし") +
  theme(axis.text.y = element_blank()) +
  coord_flip()

graph_married + graph_single

ggsave("output/manup.pdf", device = cairo_pdf,
       width = 11.69, height = 8.27)


