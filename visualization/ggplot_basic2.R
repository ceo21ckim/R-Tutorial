# ggplot basic 2 ----

if (!require('ggplot2')) install.packages('ggplot2')
if (!require('dplyr')) install.packages('dplyr')

library(ggplot2) ; library(dplyr)

head(mtcars)

# geom_point ----

# 1
p <- ggplot(mtcars, aes(wt, mpg, colour = cyl))
p <-  p + geom_point()
p

# 2 
mtcars %>% 
  ggplot(aes(wt, mpg, color = cyl)) + 
  geom_point()

# 3 
mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) +
  geom_point(color = 'orange', size = 6)

mtcars %>% 
  ggplot(aes(x = wt, y = mpg)) + 
  geom_point(aes(color = cyl, size = gear))



# geom_bar ----
# facet_grid ----

mtcars %>% 
  ggplot(aes(factor(cyl), fill=factor(cyl))) + 
  geom_bar(width = .5) + 
  facet_grid(.~ gear)

# coord_flip ----
# 좌표계를 회전시키는 함수.
# facet_wrap ----
# 패싯을 나누는 함수.

mtcars %>% 
  ggplot(aes(factor(cyl))) +
  geom_bar(aes(fill = cyl), color = 'black') # color = black 막대 그래프의 테두리를 칠함.

mtcars %>% 
  ggplot(aes(factor(cyl))) + 
  geom_bar(aes(fill = factor(gear)), color = 'black')

mtcars %>% 
  ggplot(aes(factor(cyl)))  + 
  geom_bar(aes(fill=factor(cyl)), color = 'black') + 
  coord_flip()

mtcars %>% 
  ggplot(aes(factor(cyl)))  + 
  geom_bar(aes(fill=factor(cyl)), color = 'black') + 
  facet_wrap(.~gear)


# geom_text ----
# 행의 이름을 출력해준다.

# hjust = 0 으로 지정하면 point의 오른쪽에 text가 위치함.
mtcars %>% 
  ggplot(aes(wt, mpg, label = rownames(mtcars))) + 
  geom_point() + 
  geom_text(aes(x = wt+0.05, color = factor(cyl), size = 5, hjust = 0)) 

# scale_x_date ----
# 축의 범위를 지정하거나 틱의 개수 및 라벨을 지정.
economics %>% 
  ggplot(aes(x = date, y = psavert)) + 
  geom_path() +
  scale_x_date('21 century', limits = c(as.Date('2000-01-01'), max(economics$date)))


# expand_limits ----
# 축의 범위를 확장하는데 사용.

range(mtcars$mpg)
range(mtcars$wt)

mtcars %>% 
  ggplot(aes(x = mpg, y = wt)) + 
  geom_point() + 
  expand_limits(x = 0, y = c(-1, 10))

# xlim, ylim ----

mtcars %>% 
  ggplot(aes(x = mpg, y = wt)) + 
  geom_point(size = 5) + 
  xlim(15, 25) + ylim(2, 4.5)
