if (!require('multilevel')) install.packages('multilevel')
library(multilevel)

# pred : x
# med : mediation
# out = y
# sobel ----
model.sob <- sobel(pred = mtcars$disp, med=mtcars$wt, out=mtcars$mpg)

model.sob$Indirect.Effect # 간접효과

model.sob$z.value # 모델을 통한 z.value

# p.value 가 0.005로 0.05 보다 작아 wt는 disp의 매개변수다.
pnorm(abs(model.sob$z.value), lower.tail = FALSE) *2


if (!require('bda')) install.packages('bda')
library(bda)

mediation.test(mv=mtcars$wt, iv=mtcars$disp, dv=mtcars$mpg)

# mediation ----
# 부스트래핑 방법에 의한 매개효과 분석을 수행할 수 있다.
if (!require('mediation')) install.packages('mediation')
library(mediation)

model.M <- lm(wt ~ disp , data = mtcars)
model.Y <- lm(mpg ~ disp + wt, data = mtcars)
model.mediation <- mediate(model.m = model.M, model.y = model.Y,
                           treat  ='disp', mediator = 'wt', boot = TRUE, sims = 500)

summary(model.mediation)

# error
plot.mediate(model.mediation, cex=1.2, col='royalblue', lwd = 2, 
             main = 'Mediation Effect Analysis')
