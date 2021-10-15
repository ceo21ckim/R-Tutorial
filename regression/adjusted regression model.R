# variable tuning ----
# 회귀모델이 선형성, 정규성, 등분산성 가정을 충족하지 못할 경우 변수 변환을 통해 문제를 해결하거나 개선할 수 있다.

# 정규성의 가정을 위배할 경우.
# powerTransform ----
if (!require('car')) install.packages('car')
library(car)
# lambda 0 == ln(x)
summary(powerTransform(mtcars$mpg))

# 위 경우 lambda 를 0.0296으로 해서 계산을 한다.
# lambda = 1이라는 가설을 기각하지 못하기 때문에 이 경우 변수의 변환이 필요하지 않을 수 있다.


# 선형성의 가정을 위배할 경우
# boxTidwell ----
boxTidwell(mpg ~ hp + wt, data = mtcars)

# 위 경우 귀무가설을 기각하기 때문에 hp = hp^(-0.56824), wt = wt^(-0.41743) 으로 변환할 수 있다.


# 등분산성의 가정을 위배할 경우
# spreadLevelPlot ----
# 종속변수를 변환하는 값을 제공해준다. 

spreadLevelPlot(lm(mpg~hp+wt, data=mtcars))

# 위 경우 mpg = mpg^(0.5853955)

