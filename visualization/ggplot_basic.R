head(cars)

plot(cars, type = 'p', main = 'cars')

plot(cars, type = 'l', main = 'cars')

plot(cars, type = 'b', main = 'cars')

plot(cars, type = 'h', main = 'cars')


library(gapminder) ; library(dplyr)
x <- gapminder %>% 
  filter(year == 1952 & continent == 'Asia') %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  select(country, gdp) %>% 
  arrange(desc(gdp)) %>% 
  head()
  

pie(x$gdp, x$country)

barplot(x$gdp, names.arg = x$country)

x <- gapminder %>% 
  filter(year == 2007 & continent == 'Asia') %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  select(country, gdp) %>% 
  arrange(desc(gdp)) %>% 
  head()

pie(x$gdp, x$country)
barplot(x$gdp, names.arg = x$country)

# matplot ----
# 여러그래프를 그려주는 함수.
matplot(iris[, 1:4], type = 'l')

hist(cars$speed)

# ggplot, geom_point ----
library(ggplot2)
gapminder %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, col = continent)) + 
  geom_point(alpha = 0.2)

# ggplot, geom_histogram ----
gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(mapping = aes(lifeExp, col = continent)) + 
  geom_histogram(binwidth = 1 )


gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(lifeExp, col = continent)) + 
  geom_histogram(position = 'dodge', binwidth = 1)

# ggplot, geom_boxplot ----
gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(continent, lifeExp, col = continent)) + 
  geom_boxplot()



gapminder %>% 
  ggplot( aes(x = gdpPercap, y = lifeExp, col = continent)) +
  geom_point(alpha = 0.2)

# scale_x_log10 ----
gapminder %>% 
  ggplot(aes(x =gdpPercap, y = lifeExp, col = continent)) + 
  geom_point(alpha = 0.2) + 
  scale_x_log10()


gapminder %>% 
  filter(continent == 'Asia') %>% 
  ggplot(aes(country, lifeExp)) + 
  geom_bar(stat = 'identity')


# coord_flip ----
# x, y 축을 바꿔준다. 
gapminder %>% 
  filter(continent == 'Asia') %>% 
  ggplot(aes(country, lifeExp)) + 
  geom_bar(stat = 'identity') + 
  coord_flip()



# RColorBrewer ----
# 내장된 색을 보여준다.
library(RColorBrewer)
display.brewer.all()

# 기본
gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarize(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n )) + 
  geom_bar(stat = 'identity', aes(fill=continent))



gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarize(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n )) + 
  geom_bar(stat = 'identity', aes(fill=continent)) + 
  scale_fill_brewer(palette = 'Spectral')



gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(year, continent) %>% 
  summarize(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n )) + 
  geom_bar(stat = 'identity', aes(fill=continent)) +
  scale_fill_brewer(palette = 'Blues')



gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarize(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n )) + 
  geom_bar(stat = 'identity', aes(fill=continent)) + 
  scale_fill_brewer(palette = 'Oranges')


# 데이터 순서를 조정해주기 위해서는?
# reorder ----

gapminder %>% 
  filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarize(n = n_distinct(country)) %>% 
  ggplot(aes(x = reorder(continent, -n), y = n)) + 
  geom_bar(stat = 'identity', aes(fill=continent)) +
  scale_fill_brewer(palette = 'Blues')

# facet_wrap ----
gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, col = continent)) + 
  geom_point(alpha = 0.2) + 
  facet_wrap(~year) + 
  scale_x_log10()

gapminder %>% 
  filter(year == 1952 & gdpPercap > 10000 & continent == 'Asia')

gapminder %>% 
  filter(country == 'Kuwait') %>% 
  ggplot(aes(year, gdpPercap)) + 
  geom_point() + 
  geom_line()


gapminder %>% 
  filter(country == 'Kuwait') %>% 
  ggplot(aes(year, pop)) + 
  geom_point() + 
  geom_line()


gapminder %>% 
  filter(country == 'Korea, Rep.') %>% 
  ggplot(aes(year, gdpPercap)) + 
  geom_point() + 
  geom_line()


gapminder %>% 
  filter(country == 'Korea, Rep.') %>% 
  ggplot(aes(year, pop)) + 
  geom_point() + 
  geom_line()


gapminder %>% 
  filter(country == 'Kuwait' | country == 'Korea, Rep.') %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  ggplot(aes(year, gdp, col = country)) + 
  geom_point() + 
  geom_line()


gapminder %>% 
  filter(country == 'Kuwait' | country == 'Saudi Arabia' | country == 'Iraq' | country == 'Iran' | country == 'Korea, Rep.' |
         country == 'China' | country == 'Japan') %>% 
  ggplot(aes(year, gdpPercap, col = country)) +
  geom_point() + 
  geom_line()


gapminder %>% 
  filter(country == 'Kuwait' | country == 'Saudi Arabia' | country == 'Iraq' | country == 'Iran' | country == 'Korea, Rep.' |
         country == 'China' | country == 'Japan') %>% 
  ggplot(aes(year, pop, col = country)) +
  geom_point() + 
  geom_line() + 
  scale_y_log10()


gapminder %>% 
  filter(country == 'Kuwait' | country == 'Saudi Arabia' | country == 'Iraq' | country == 'Iran' | country == 'Korea, Rep.' |
         country == 'China' | country == 'Japan') %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  ggplot(aes(year, gdp, col = country)) +
  geom_point() + 
  geom_line() + 
  scale_y_log10()





