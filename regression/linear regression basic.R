# linear model ----

x <- c(3, 6, 9, 12)
y <- c(3, 4, 5.5, 6.5)

linear_model <- lm(y ~x)

linear_model

plot(x, y)
abline(linear_model, col = 'red')

coef(linear_model)

# y_hat
fitted(linear_model)

# residual
residuals(linear_model)

# MSE
deviance(linear_model) / length(x)

summary(linear_model)

newx <- data.frame(x = c(1.2, 2.0, 20.65))

predict(linear_model, newdata = newx)

str(cars)

head(cars)

plot(cars)


car_model <- lm(dist~speed, data = cars)

coef(car_model)

plot(cars$speed, cars$dist)
abline(car_model, col = 'red')


fitted(car_model)

nx1 <- data.frame(speed = c(21.5))
predict(car_model, nx1)

nx2 <- data.frame(speed = c(25.0, 25.5, 26.0, 26.5, 27.0, 27.5, 28.0))
predict(car_model, nx2)

nx <- data.frame(speed = c(21.5, 25.0, 25.5, 26.0, 26.5, 27.0, 27.5, 28.0))
plot(nx$speed, predict(car_model, nx), col = 'red', cex = 2, pch = 20)
abline(car_model)

str(women)

women

women_model <- lm(weight ~ height, data = women)
coef(women_model)

plot(women)
abline(women_model, col = 'red')


summary(women_model)


car_model = lm(dist ~ speed, data = cars)
summary(car_model)


if (!require('scatterplot3d')) install.packages('scatterplot3d')
library(scatterplot3d)

x <- c(3.0, 6.0, 3.0, 6.0)
u <- c(10.0, 10.0, 20.0, 20.0)
y <- c(4.65, 5.9, 6.7, 8.02)

scatterplot3d(x, u, y, xlin = 2:7, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h')

# multiple linear model ----
m <- lm(y ~ x + u)
coef(m)

s <- scatterplot3d(x, u, y, xlim = 2:7, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h')
s$plane3d(m)


fitted(m)

residuals(m)

deviance(m)

deviance(m)/length(x)

nx <- c(7.5, 5.0)
nu <- c(15.0, 12.0)
new_data <-  data.frame(x = nx, u = nu)
ny <- predict(m, new_data)
ny

s <- scatterplot3d(x, u, y, xlim = 2:7, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h', color = 'red', angle = 60)
s$plane3d(m)



str(trees)

summary(trees)

scatterplot3d(trees$Girth, trees$Height, trees$Volume)

m <- lm(Volume ~ Girth + Height, data = trees)
m

s <- scatterplot3d(trees$Girth, trees$Height, trees$Volume, pch = 20, type = 'h', angle = 55)
s$plane3d(m)

ndata <- data.frame(Girth = c(8.5, 13.0, 19.0), Height = c(72, 86, 85))
predict(m, newdata = ndata)