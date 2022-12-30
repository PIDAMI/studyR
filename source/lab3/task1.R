# ЗАДАНИЕ 5
# trades.RData Объедините таблицы в одну, уберите столбец с территорией торговли, 
# т.к. там только Евросоюз, оставим только строки с экспортом и импортом, вынесете данные по экспорту и импорту в отдельные переменные. 
# Отобразите структуру экспорта продукции из Евросоюза по ведущим партнерам в виде круговых диаграммы на одном рисунке. 
# Обозначьте разные группы экспортируемых товаров разными цветами. Для каждого партера подпишите значение экспорта. 
# Для каждого партнера подпишите какова доля (процент) каждой из групп экспортируемых товаров в общем экспорте. 
# Отрегулируйте местоположение подписей так, чтобы рисунок был максимально наглядным. 
# Придумайте новые названия графика и осей.
install.packages("tidyverse")

load("data/trades.RData")
library(dplyr)
library(tidyr)

df <- bind_rows(trades)
df <- df[, !(names(df) %in% c("geo"))]
k = df %>% 
  filter(grepl("[Im,Ex]ports", indic_et)) %>% 
  group_by(partner, indic_et, sitc06) %>% 
  summarise(val = mean(values), .groups = "drop") %>%
  pivot_wider(names_from=indic_et, values_from=val) %>%
  rename(export="Exports in million of ECU/EURO", import="Imports in million of ECU/EURO", 
         export_share = "Share of exports by partner (%)",
         import_share = "Share of imports by partner (%)")
k  

library(ggplot2)

k %>% sort()
ggplot(k, aes(x="", y=export, fill=partner)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric label