# ЗАДАНИЕ 13(у меня должно быть задание 10, но данных для него нет, поэтому выбрал это задание)
# data2.csv, data1.txt Объедините data.frames по идентификаторам сортов. 
# Исключите те сорта, для которых есть пропущенные значения. 
# Выбрать одну количественную и две качественные переменные .
# Разбить значения количественной переменной на группы в соответствии со значениями одной из качественных переменных. 
# Нарисовать график, на котором отображены плотности распределений и гистограммы для получившихся групп,
# обозначить разные группы разными цветами.
install.packages("hrbrthemes")

library(tidyr)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(gridExtra)


df1 <- read.csv("data/data1.txt", sep=" ", skip=1)
rownames(df1) <- df1$X1
df1 <- subset(df1, select=-c(X1))

df2 <- read.csv("data/data2.csv", sep=',', header=FALSE)
df2 <- data.frame(t(df2))
names(df2) <- df2[1, ]
df2 <- df2[-1, ]
names(df2)[1] = "Sample" 

df <- merge(df1,df2,by="Sample") %>% select(-c(Sample))
df 
t <- df %>% drop_na() %>% select(c(Height, MaturType, GrowthType)) %>% group_by(MaturType)
k <-  df %>% drop_na() %>% select(c(Height, MaturType, GrowthType))
hist(t$Height)
hist(k$Height)


p1 <-t %>%
ggplot( aes(x=Height, fill=MaturType)) +
  geom_histogram( position = 'identity', binwidth=5, alpha=0.6) +
  theme_ipsum() +
  labs(fill="")

p2 <- t %>%
  ggplot(aes(x=Height, fill=MaturType)) +
  geom_density(aes(Height))

grid.arrange(p1, p2, nrow=2,top="Height value statistics for each MaturType")


