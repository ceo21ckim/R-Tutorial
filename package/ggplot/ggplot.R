# ggplot2 ----

library(gapminder) ; library(ggplot2)

lfp <- gapminder

lfp %>% 
  select(country, year, pop, gdpPercap) %>% 
  mutate(lfp.gdp = gdpPercap/pop) %>% 
  group_by(year) %>% 
  summarise( N = n(),
             gdp = mean(lfp.gdp),
             sd = sd(lfp.gdp)) %>% 
  ggplot(mapping = aes(x = year,
                       y = gdp)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_ribbon(aes(ymin = gdp - sd, 
                  ymax = gdp + sd),
              alpha = 0.2) + 
  xlim(min = 2000, max = 2010)


