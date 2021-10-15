# select regression model
# 회귀모델의 선택은 모델의 예측정확도(predictive accuracy)와 간명도(parsimony)를 바탕으로 한다.
# 많은 변수를 사용해 모델을 만들면 적합도는 증가하나 일반화가 어렵다.

# anova 함수를 이용해 중첩모델(nested model)간의 적합도를 비교할 수 있다.

summary(mtcars)
str(mtcars)

mtcars.lm1 <- lm(mpg ~ hp + wt, data = mtcars)
mtcars.lm2 <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)

anova(mtcars.lm1, mtcars.lm2)
# reduce model 이 더 좋은 것을 확인할 수 있다.
# 추가적으로 두 변수를 더 넣더라도 설명력에 유의미한 영향을 주지 않는다.

# AIC ----
# Akaike Information Criterion
# AIC가 낮은 값은 적은 개수의 파라미터로 적절한 적합도를 달성하고 있다는 의미
# AIC가 낮을수록 우수한 모델로 평가된다.

AIC(mtcars.lm1, mtcars.lm2)

# forward, backward, stepwise -----

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
step(mtcars.lm, direction = 'backward')

step(mtcars.lm, directions = 'both')

step(mtcars.lm, direction = 'forward')

# 변수선택법을 통해서 변수를 제거하는 방법이 효율적이지만 이 경우 역시 최적의 모델을 제공한다고 할 수는 없다.
# leaps 패키지의 regsubsets은 가능한 모든 회귀모델을 탐색하고 각 모델의 적합도를 계산한다.
# regsubsets ----

if (!require('leaps')) install.packages('leaps')

library(leaps)

# nbest=2라고 하면 한 개의 독립변수를 갖는 상위 두 개의 최적 모델, 두 개의 독립변수를 갖는 상위 두 개의 최적모델,...
# 이런식으로 4개의 변수를 갖는 모델까지 진행하게 된다. 
mtcars.regsubsets <- regsubsets(x=mpg ~ hp + wt + disp + drat, data=mtcars, nbest = 4)
mtcars.regsubsets

if (!require('RColorBrewer')) install.packages('RColorBrewer')
library(RColorBrewer)

plot(mtcars.regsubsets, scale = 'adjr2', col = brewer.pal(9, 'Pastel1'),
     main = 'All Subsets Regression')


names(summary(mtcars.regsubsets))
summary(mtcars.regsubsets)
# 어떤 변수를 사용했는지 사용했는지 나온다.

# 11번째의 adjr2 이 가장 크다.
summary(mtcars.regsubsets)$adjr2

which.max(summary(mtcars.regsubsets)$adjr2)

coef(mtcars.regsubsets, 11)
