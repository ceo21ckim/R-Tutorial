# multiple regression ----
# multiple linear regression analysis

str(mtcars)

mtcars <- mtcars[c('mpg', 'hp', 'wt', 'disp', 'drat')]

summary(mtcars)

cor(mtcars)

library(car) ; library(stargazer)
# car package 안의 scatterplotMatrix로 산점도 행렬을 생성
# seaborn의 pairplot 과 유사하다. 좀 더 이쁨.
scatterplotMatrix(mtcars, pch=19, col='royalblue', cex=1.2, 
                  regLine=list(method=lm, lty=1, lwd=3, col='salmon'),
                  smooth=list(smoother=loessLine, spread=FALSE,
                              lty.smooth=1, lwd.smooth=3, col.smooth='forestgreen'),
                  main='Car Performance')

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)

summary(mtcars.lm)

stargazer(mtcars.lm, type='text', no.space=TRUE)

# 단순히 회귀계수의 크기만 보고 어떤 변수의 예측력이 더 큰지는 알 수 없다.
# scale 함수를 통해서 normalized 해준다. 

mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), data =mtcars)
summary(mtcars.lm)


# QuantPsyc package 의 lm.beta를 통해 조금 더 쉽게 구할 수 있다.
if (!require('QuantPsyc')) install.packages('QuantPsyc')
library(QuantPsyc)

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)

lm.beta(mtcars.lm)


# 회귀분석 가정과 진단
# 선형성(linearity) : 종속변수와 독립변수 간의 관계는 선형이다.
# 정규성(normality) : 독리볍수값에 대해 대응되는 종속변수값들의 분포는 정규분포이다.
# 등분산성(homoscedasticity, equality of variance) : 독립변수값에 대해 대응되는 종속변수값들의 분포는 모두 동일한 분산을 갖는다.
# 독립성(independence) : 모든 관측값은 서로 독립이다. 하나의 관측값은 다른 관측값에 영향을 미치지 않는다.

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
plot(mtcars.lm)

