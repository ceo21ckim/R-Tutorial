if (!require('gapminder')) install.packages('gapminder')
if (!require('dplyr')) install.packages('dplyr')
library(gapminder) ; library(dplyr)

str(gapminder)
summary(gapminder)

# dplyr -----
# filter, select , ...
glimpse(gapminder)

q <- gapminder

gapminder[,c('country', 'lifeExp', 'year')]

gapminder[1:15,]

gapminder[gapminder$country=='Korea, Dem. Rep.',]

q[q$country=='Korea, Dem. Rep.', c('lifeExp', 'pop')]
q[q$country=='Korea, Dem. Rep.' & q$year > 1990, c('lifeExp', 'pop')]


# 2 = columns 1 = rows
apply(q[q$country=='Korea, Dem. Rep.', c('lifeExp', 'pop')], 2, mean)


gapminder %>% 
  select(country, year, lifeExp)


gapminder %>% 
  filter(year > 1990, country=='Korea, Dem. Rep.')

gapminder %>% 
  summarise(pop_avg = mean(pop)) 


gapminder %>% 
  group_by(continent) %>% 
  summarise(pop_avg = mean(pop))
