# ЗАДАНИЕ 5
# trades.RData Объедините таблицы в одну, уберите столбец с территорией торговли, 
# т.к. там только Евросоюз, оставим только строки с экспортом и импортом, вынесете данные по экспорту и импорту в отдельные переменные. 
# Отобразите структуру экспорта продукции из Евросоюза по ведущим партнерам в виде круговых диаграммы на одном рисунке. 
# Обозначьте разные группы экспортируемых товаров разными цветами. Для каждого партера подпишите значение экспорта. 
# Для каждого партнера подпишите какова доля (процент) каждой из групп экспортируемых товаров в общем экспорте. 
# Отрегулируйте местоположение подписей так, чтобы рисунок был максимально наглядным. 
# Придумайте новые названия графика и осей.
setwd("~/develop/studyR")
load("data/trades.RData")
library(dplyr)
library(tidyr)


df <- bind_rows(trades)
df <- df[, !(names(df) %in% c("geo"))]
averaged = df %>% 
  filter(grepl("[Im,Ex]ports", indic_et)) %>% 
  group_by(partner, indic_et, sitc06) %>% 
  summarise(val = mean(values), .groups = "drop") %>%
  pivot_wider(names_from=indic_et, values_from=val) %>%
  rename(export="Exports in million of ECU/EURO", import="Imports in million of ECU/EURO", 
         export_share = "Share of exports by partner (%)",
         import_share = "Share of imports by partner (%)")


top_export = averaged  %>% group_by(partner) %>% summarise(w = sum(export)) %>% arrange(desc(w)) %>% slice_head(n=5)
par(oma = c(5,1,1,1), mfrow = c(2, 2), mar = c(2, 2, 1, 1))

products = unique(averaged$sitc06)
for (i in 1:4){
  country <- top_export[i,"partner"]
  exports <- averaged[averaged$partner == country$partner, ]
  title  <- paste(country$partner, sum(exports$export), sep=',')
  pie(exports$export, 
      labels=paste(exports$export_share, "%", sep=' '),
      main = paste(country$partner, sum(exports$export)),
      col = rainbow(length(products)))
}

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0)
legend('bottom',legend = products, fill=rainbow(length(products)), lwd = 3, xpd = TRUE, horiz = TRUE, cex = 0.45, seg.len=0.2, bty = 'n')

mtext("Countries having most export with EU, averaged over 2009-2019",                 
      side = 1,
      line = - 2,
      outer = TRUE)

