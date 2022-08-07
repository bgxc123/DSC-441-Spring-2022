#Part a
df = read.csv("adult.csv")
summary(df)
head(df)

#Part b
library(ggplot2)
ggplot(data = df, aes(x=age)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age Histogram") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

ggplot(data = df, aes(x=fnlwgt)) +
  geom_histogram(binwidth = 100000) +
  ggtitle("Final Weight Histogram") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

#Part c
df_nums = df[c(1,3,5,11:13)]
head(df_nums)
plot(df_nums)

#Part d
library(tidyverse)
df %>% count(race)

ggplot(df, aes(x=relationship)) + geom_bar() +
  ggtitle("Relationship Bar Plot") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

#Part e
crossTab = table(df$education,df$income)
ftable(crossTab)
