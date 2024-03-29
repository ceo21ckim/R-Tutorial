library(gapminder)

attach(gapminder)

# mean
apply(anscombe, 2, mean)

# var
apply(anscombe, 2, var)

# corr
cor(anscombe$x1, anscombe$y1)

cor(anscombe$x2, anscombe$y2)

cor(anscombe$x3, anscombe$y3)

cor(anscombe$x4, anscombe$y4)


library(dplyr)

y <- gapminder %>% 
  group_by(year, continent) %>% 
  summarize(c_pop = sum(pop))

head(y, 20)

plot(y$year, y$c_pop)

plot(y$year, y$c_pop, 
     col = y$continent, 
     pch = c(1:length(levels(y$continent)))
     )
legend('topleft', legend=levels(y$continent),
       pch = c(1:length(levels(y$continent))), 
       col = c(1:length(levels(y$continent))))

level = levels(y$continent)



plot(gapminder$gdpPercap, gapminder$lifeExp, col = gapminder$continent)
legend('bottomright', legend = level, pch = c(1:length(level)), col = c(1:length(level)))


# log scale
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, col = gapminder$continent)
legend('bottomright', legend = level, pch = c(1:length(level)), col = c(1:length(level)))




library(ggplot2) ; library(dplyr) ; library(gapminder)

gapminder %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, col = continent)) + 
  geom_point() + 
  scale_x_log10()

# geom_point : alpha  = 투명도 
gapminder %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp,size = pop, col = continent)) + 
  geom_point(alpha = 0.5) + 
  scale_x_log10()

# facet_wrap : 같은 그래프를 반복해서 그려준다.
gapminder %>% 
  ggplot(mapping = aes(gdpPercap, y = lifeExp, col = continent, size= pop)) + 
  geom_point() +
  scale_x_log10() + 
  facet_wrap(~year)

# coord_flip : x, y 축을 swap해준다. 
# reorder : 정렬해준다. 
gapminder %>% 
  filter(year == 1952 & continent == 'Asia') %>% 
  ggplot(mapping = aes(reorder(country, pop), pop)) + 
  geom_bar(stat = 'identity') + # identity를 하면 빈도수가 아닌 해당 값을 그대로 가져온다. 
  coord_flip()