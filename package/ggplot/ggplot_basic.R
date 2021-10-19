# histogram ----

if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

car <- mtcars

car %>% 
  ggplot(mapping = aes(x = mpg)) + 
  geom_histogram(bins = 10,
                 alpha = 0.5,
                 fill = 'black',
                 col = 'grey')

# 색 조합들을 보여준다. 
# color ----
RColorBrewer::display.brewer.all()

library(RColorBrewer)
car$vs <- as.factor(car$vs)
car %>% 
  ggplot(mapping = aes(mpg, fill = vs)) + 
  geom_histogram(bins = 10) + 
  scale_fill_brewer(palette = 'Pastel2')


library(gapminder)

gap <- gapminder

gap <- gap %>% 
  mutate(logpop = log(pop))

gap %>% 
  ggplot(mapping = aes(logpop)) +
  geom_histogram(aes(y = ..density..), # 축이 density로 나온다.
                 alpha = 0.3,
                 fill = 'blue',
                 bins =20) +
  xlab('log(pop)')

# 등고선 느낌.
# density ----
gap %>% 
  ggplot(mapping = aes(pop, year)) + 
  stat_density2d(alpha = 0.5)


gap %>% 
  ggplot(mapping = aes(logpop)) + 
  stat_density(alpha = 0.5,
               fill = 'steelblue')


gap %>% 
  ggplot(mapping = aes(x = logpop, y= ..density..)) +
  geom_histogram(alpha = 0.3,
                 fill = 'blue',
                 bins = 30) + 
  stat_density(geom = 'line',
               color = 'blue',
               kernel = 'gaussian')


# labs ----
gap %>% 
  ggplot(aes(logpop, ..density..)) +
  geom_histogram(alpha = 0.3, 
                 fill = 'blue',
                 bins = 30) + 
  stat_density(geom = 'line') + 
  labs(x = '인구', y = '밀도')

car
# correlation graph ----
# scatter ----
car %>% 
  ggplot(mapping = aes(mpg, disp)) + 
  geom_point()

car %>% 
  ggplot(mapping = aes(mpg, disp, color = vs)) +
  geom_point()


car %>% 
  ggplot(mapping = aes(mpg, disp, color = vs)) +
  geom_point() + 
  scale_color_brewer(palette = 'Set2') + 
  theme(legend.title = element_blank(),
        legend.position = 'bottom')



car %>% 
  ggplot(aes(mpg, disp, colour = vs)) +
  geom_smooth(method = 'lm') + # method를 바꾸면 다른 추세선을 만들 수 있다 lm, loess
  geom_point(size = 2, 
              alpha = 0.5) + 
  scale_color_brewer(palette = 'Set1') +
  theme_classic() # 배경이 없어진다.

# theme ----
# background

# theme_bw
# theme_void
# theme_grey
# theme_classic
# theme_dark
# theme_gray
# theme_linedraw
# theme_minimal .....


car %>% 
  ggplot(aes(mpg, disp, colour = vs)) +
  geom_smooth(method = 'loess') + 
  geom_point(size = 2, 
             alpha = 0.5) + 
  scale_color_brewer(palette = 'Set1') +
  theme_linedraw()


# facet_wrap ----
# separate 
car %>% 
  ggplot(aes(mpg, disp, colour = vs)) +
  geom_smooth(method = 'lm') + 
  geom_point(size = 2, 
             alpha = 0.5) + 
  facet_wrap(~vs, ncol = 2) + 
  scale_color_brewer(palette = 'Set1') +
  theme(legend.position = 'bottom')




# ggExtra ----
# 상관관계를 그린 그래프 옆에 히스토그램을 같이 그려 넣을 수 있다.
if (!require(ggExtra)) install.packages('ggExtra')
library(ggExtra)

car %>% 
  ggplot(mapping = aes(x = mpg, y = disp)) + 
  geom_point() + 
  theme(legend.position = 'none') -> fig1

ggMarginal(fig1, 
           type = 'histogram',
           margins = 'both', # both, x, y
           size = 10) # histogram의 사이즈가 아니고 fig size 



# rug ----

car %>% 
  ggplot(aes(mpg, disp)) +
  geom_point() + 
  geom_rug(col = 'red',
           alpha = 0.3,
           sides = 'b', # b or trbl(all)
           size = 1.0, 
           position = 'identity')



rm(list = ls())
if (!require('AER')) install.packages('AER')
if (!require('grid')) install.packages('grid')
if (!require('gridExtra')) install.packages('gridExtra')
if (!require('dplyr')) install.packages('dplyr')
if (!require('ggplot2')) install.packages('ggplot2')


library('AER'); library(dplyr); library('ggplot2'); library('grid'); library('gridExtra')

# AER 패키지에 내장된 데이터 셋
data(PSID1976)

attach(PSID1976)

PSID1976 <- PSID1976 %>% 
  mutate(logwage = log(wage))

is.na(PSID1976) <- sapply(PSID1976, is.infinite) ## replace infinite by NA

# histogram for level-variable
PSID1976 %>% 
  ggplot(mapping = aes(x = wage)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 alpha = 0.5,
                 fill = 'black',
                 col = 'grey') + 
  stat_function(fun = dnorm,
                color = 'red',
                args = list(mean = mean(wage, na.rm = TRUE),
                            sd = sd(wage, na.rm = TRUE))) +
  xlab('wage') -> fig1



# histogram for log-tranfored variable
PSID1976 %>% 
  ggplot(mapping = aes(x = logwage)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.3,
                 alpha = 0.5,
                 fill = 'black',
                 col = 'grey',
                 na.rm = TRUE) +
  stat_function(fun = dnorm, 
                color = 'red',
                args = list(mean = mean(PSID1976$logwage, na.rm = TRUE),
                            sd = sd(PSID1976$logwage, na.rm = TRUE))) +
  xlab('log(wage)') -> fig2


grid.arrange(fig1, fig2, ncol = 2)



