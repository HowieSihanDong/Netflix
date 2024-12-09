library(tidyverse)
library(here)
library(ggplot2)
library(lsr)
library(RColorBrewer)
library(car)

# 加载必要的包
library(dplyr)
library(ggplot2)
library(scales) # 用于百分比格式化
library(reshape2) # 用于数据重整形

# 读取数据
df <- read_csv(file=here("data_after_Q1.csv"))

# 过滤reality类型数据
reality_df <- df %>% 
  filter(genre_reality == 1) %>%
  select(imdb_category, starts_with("genre_"))

# 计算每个imdb_category中reality类型的百分比
reality_dist <- reality_df %>%
  group_by(imdb_category) %>%
  summarise_all(mean) %>%
  pivot_longer(cols = -imdb_category, names_to = "genre", values_to = "percentage")

# 将百分比数据格式化为百分数形式
reality_dist$percentage <- scales::percent(reality_dist$percentage)

# 重新塑形数据框以适合热图
heatmap_data <- reality_dist %>%
  dcast(imdb_category ~ genre, value.var = "percentage")

# 绘制热图
ggplot(melt(heatmap_data), aes(x=Var2, y=Var1, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="white", high="red") +
  geom_text(aes(label=value), color="black", size=4) +
  theme_minimal() +
  labs(x="Genre", y="IMDB Category", fill="Percentage") +
  ggtitle("Percentage Distribution of Reality Genre in Each IMDB Category")




