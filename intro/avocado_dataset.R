# remove Environment ----
rm(list =ls())

# load package
library(gapminder) ; library(dplyr) ; library(ggplot2)

gapminder %>% 
  group_by(continent, country) %>% 
  summarize(pop_avg = mean(pop))

temp1 <- filter(gapminder, country == 'Croatia')
temp2 <- select(temp1, country, year, lifeExp)
temp3 <- apply(temp2[ , c('lifeExp')], 2, mean)
temp3

gapminder %>% 
  filter(country == 'Croatia') %>% 
  select(country, year, lifeExp) %>% 
  summarize(lifeExp_avg = mean(lifeExp))


library(readxl)

# setting directory ----
path <- 'C:\\Users\\EonKim\\Desktop\\git_upload\\khu\\study\\bigdata visualization\\dataset'
setwd(path)

# checking directory ----
getwd()


# load dataset of kaggle
avocado <- read.csv('avocado.csv', header = TRUE, sep = ',')

str(avocado)


x_avg <-  avocado %>%
  group_by(region, year, type) %>% 
  summarise(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice))

# sorting (ascending = True) ----
# arrange, sort, order
# sort(x, decreasing = TRUE or FALSE)

# decreasing
arrange(x_avg, desc(V_avg))

# increasing
arrange(x_avg, V_avg)

x_avg1 <- x_avg %>% 
  filter(region != 'TotalUS')

x_avg1[x_avg1$V_avg == max(x_avg1$V_avg), ]

# ggplot ----

x_avg %>% 
  filter(region != 'TotalUS') %>% 
  ggplot(aes(year, V_avg, col = type)) + 
  geom_line()


# 날짜 데이터를 처리할 떄 유용한 패키지
if (!require('lubridate')) install.packages('lubridate')
library(lubridate)

x_avg <- avocado %>% 
  group_by(region, year, month(Date), type) %>% 
  summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice))























