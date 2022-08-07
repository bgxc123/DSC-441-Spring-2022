df = read.csv("population_even.csv")
df2 = read.csv("population_odd.csv")

#a.)
head(df)
head(df2)

joined_df = df %>% inner_join(df2, by="NAME")

#b.)
##deleting state column
df3 = joined_df %>% select(-STATE.y)
head(df3)

##renaming columns
df3 = df3 %>% rename("2010" = POPESTIMATE2010) %>%
  rename("2011" = POPESTIMATE2011) %>%
  rename("2012" = POPESTIMATE2012) %>%
  rename("2013" = POPESTIMATE2013) %>%
  rename("2014" = POPESTIMATE2014) %>%
  rename("2015" = POPESTIMATE2015) %>%
  rename("2016" = POPESTIMATE2016) %>%
  rename("2017" = POPESTIMATE2017) %>%
  rename("2018" = POPESTIMATE2018) %>%
  rename("2019" = POPESTIMATE2019)
head(df3)

##reordering columns
df3 = df3 %>% relocate("2011",.after = "2010") %>%
  relocate("2013", .after = "2012") %>%
  relocate("2015", .after = "2014") %>%
  relocate("2017", .after = "2016") %>%
  relocate("2019", .after = "2018")
head(df3)

#c.)
##Arizona 2011
df3$"2011"[3] = mean(df3$"2010"[3]:df3$"2012"[3])

##Idaho 2015
df3$"2015"[13] = mean(df3$"2014"[13]:df3$"2016"[13])

##Montana 2017
df3$"2017"[27] = mean(df3$"2016"[27]:df3$"2018"[27])

##Ohio 2013
df3$"2013"[36] = mean(df3$"2012"[36]:df3$"2014"[36])

##Wisconsin 2019
df3$"2019"[50] = mean(df3$"2018"[50]:df3$"2017"[50])
summary(df3)

#d.)
df3 %>%
  rowwise() %>%
  mutate(m = max(c("2010","2011","2012","2013","2014","2015","2016","2017",
                   "2018","2019")))

df3 %>%
  rowwise() %>%
  mutate(m = sum(c("2010","2011","2012","2013","2014","2015","2016","2017",
                   "2018","2019")))


#e.)
sum(df3[3])

#Problem #3
head(df3)

df_pivot = df3 %>% pivot_longer(cols = starts_with("20"),
                     names_to = "year", 
                     values_to = "population")
df_3states = df_pivot[1:30,]

library(ggplot2)

df_3states$population = as.integer(df_3states$population)
df_3states$year = as.integer(df_3states$year)
alabama = df_3states[1:10,]
alaska = df_3states[11:20,]
arizona = df_3states[21:30,]

azplot = ggplot(arizona, aes(x=year, y = population, color = NAME)) +
  geom_line(color = "blue") +
  ggtitle("Population Trend by State YoY") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))
abplot = ggplot(alabama, aes(x=year, y = population, color = NAME)) +
  geom_line() +
  ggtitle("Population Trend by State YoY") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

alplot = ggplot(alaska, aes(x=year, y = population, color = NAME)) +
  geom_line(color = "green") +
  ggtitle("Population Trend by State YoY") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))
install.packages("patchwork")
library(patchwork)

ttlplot = ggplot(df_3states, aes(x=year, y = population, color = NAME)) +
  geom_line() +
  ggtitle("Population Trend by State YoY") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

azplot + abplot + alplot + ttlplot + plot_layout(ncol = 2)
