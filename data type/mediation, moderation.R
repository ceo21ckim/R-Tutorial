if (!require('multilevel')) install.packages('multilevel')
library(multilevel)

# pred : x
# med : mediation
# out = y
# mediation effect ----
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



# moderation effect ----
mtcars.lm <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(mtcars.lm)

if (!require('effects')) install.packages('effects')
library(effects)

m <- round(mean(mtcars$wt),1) ; m
s <- round(sd(mtcars$wt), 1) ;s

# term : 모델에서 시각화할 부분의 포뮬러
# mod : lm() 함수에 의해 반환된 조절효과 회귀모델
# xlevels 인수에는 일정하게 유지할 변수명과, 그 변수값을 지정한다. 
# wt를 m-s, m, m+s 이렇게 각각 고정했을 때의 mpg~hp의 관계를 보려고 하는 것.
# lines 인수를 이용해 선의 형태 및 속성을 지정할 수 있음.

plot(effect(term = 'hp:wt', mod = mtcars.lm, xlevels = list(wt = c(m-s, m, m+s))),
     lines = list(multiline = TRUE, lwd = 2, lty = c(3, 2, 1),
                  col = c('royalblue', 'violet', 'maroon')),
     main = 'Interaction Plot for Horsepower and Weight')



if (!require('rockchalk')) install.packages('rockchalk')
library(rockchalk)
# modVals : std.dev로 지정하면 조절변수의 값이 평균과 평균의 중심으로 1 sigma 만큼 떨어질때의 회귀선을 그려준다.
# plotx : x의 변수
# modex : moderation 변수 조절변수
plotSlopes(model = mtcars.lm, plotx='hp', modx='wt', modxVals = 'std.dev.',
           pch=21, col=rainbow(3), cex=1, bg='dimgray',
           main='Interaction Plot for Horsepower and Weight')


# moderated mediation effect ----
# 매개변수에 의해 매개된 두 변수 간 직접적 또는 간접적 영향관계에 제 4의 변수(조절변수)가 영향을 미치는지 검정

model.M <- lm(mpg ~ disp*am, data=mtcars)
model.Y <- lm(mpg ~ disp*am + wt*am, data=mtcars)

# 매개된 관게에 대해 조절효과가 존재한다면 독립변수, 매개변수, 종속변수 간의 관계 패턴은 조절변수에 따라 달라진다.
library(mediation)

# sims : 몇번 반복할 것인가?
model.med1 <- mediate(model.m = model.M, model.y = model.Y, covariates = list(am=0),
                      treat='disp', mediator ='wt', boot=TRUE, sims = 500)

summary(model.med1)


model.med2 <- mediate(model.m = model.M, model.y = model.Y, covariates = list(am=1),
                      treat='disp', mediator='wt', boot = TRUE, sims = 500)

summary(model.med2)



# am=0, am=1 사이의 차이가 통계적으로 유의미한 차이가 있는지 검정
# test.modemed ----
set.seed(12)
model.med <- mediate(model.m = model.M, model.y = model.Y, 
                     treat = 'disp', mediator = 'wt', sims = 500)

set.seed(12)
test.modmed(object=model.med, 
            covariates.1=list(am=0), covariates.2 =list(am=1), sims = 500)

 