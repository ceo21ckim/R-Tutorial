# visualization 
# week3 ----


str(cars)

cars

# plot ----
plot(women)

plot(women, col='blue')

# lab = plt.ylabel, plt.xlabel
plot(women, xlab = '�ӵ�', ylab = '�Ÿ�')

plot(women, xlab = '�ӵ�', ylab = '�Ÿ�', pch =18)


example(plot)

Speed <- cars$speed
Distance <- cars$dist

# pannel.first -----
# ���ڸ� �׷��ش� grid(8, 8)

plot(Speed, Distance, panel.first = grid(8, 8), pch = 0, cex = 1.2, col = 'blue')

# line �� ���� �׸� �� �ִ�.
# c(line, grid)
plot(Speed, Distance, panel.first = c(lines(stats::lowess(Speed, Distance), grid(8,8)), lty = 'dashed'), pch = 0, cex = 1.2, col = 'blue')

if (!require('dplyr')) install.packages('dplyr')
if (!require('ggplot2')) install.packages('ggplot2')

library(dplyr)
library(ggplot2)

search()


str(iris)

head(iris,10)

plot(iris)

plot(iris$Petal.Width, iris$Petal.Length, col = iris$Species)

tips <- read.csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv')

str(tips)

head(tips,10)

summary(tips)

tips %>% 
  ggplot(aes(size)) +
  geom_histogram()


tips %>% 
  ggplot(aes(x = total_bill, y = tip)) +
  geom_point()

tips %>% 
  ggplot(aes(x = total_bill, y = tip)) + 
  geom_point(aes(col = smoker))

tips %>% 
  ggplot(aes(x = total_bill, y = tip)) + 
  geom_point(aes(col = day, pch = sex), size = 3)

