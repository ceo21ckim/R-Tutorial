
# missing value

# is.na() ----
# NA 값을 TRUE로 반환한다.
x <- c(1, 2, 3, NA)
is.na(x)

x[!is.na(x)]

# complete.cases() ----
# na.omit()
# 행렬이나 데이터프레임에서 결측값이 포함되지 않은 행을 식별한다.
# 누락된 관측값이 없는 행에 대해서는 TRUE를 반환하고, 하나라도 결측치가 있으면 FALSE를 반환한다.


str(airquality)

# complete.cases() == na.omit()
head(airquality[complete.cases(airquality),])

na.omit(airquality)


# reverse
airquality[!complete.cases(airquality),]

# sum, mean

# 왜 다르게 나올까?
# is.na는 NA값인 모든 값을 가져오는 함수이고, complete.cases는 하나라도 NA면 열을 가져오는거라
# 2개이상 NA값을 가진 경우가 있을 수 있기 때문이다. 
sum(is.na(airquality)) # 44

# 4.7%에 해당한다. 44 / (153*6) = 0.047
sum(!complete.cases(airquality)) 

# question1 ----
# 하나라도가 아니라 columns별로 결측치가 일정 개수 이상 있는 값을 제거하고 싶을땐 어떻게 해주어야할까?
dt = data.frame(x = c(1, 2, 3, NA, 4), y = c(NA, 1, 2, 3, 4))
dt[complete.cases(dt),]

# 하나라도 결측치인 row의 개수를 출력
sum(!complete.cases(airquality)) # 42

# 27% 에 해당한다. 42/153 = 0.27
mean(!complete.cases(airquality))



# visualization ----
if (!require(mice)) install.packages('mice')

library(mice)


# md.pattern() ----
# 출력되는 0과 1은 결측값과 비결측값을 보여준다.
# 1인 경우 결측값이 전혀 없는 패턴
# 왼쪽의 숫자는 패턴의 개수
# 오른쪽의 숫자는 결측값이 발생한 변수의 개수 
# 아래의 값은 해당 열의 결측치 수

md.pattern(airquality)

# VIM package ----

if (!require(VIM)) install.packages('VIM')
library(VIM)

# aggr ----
# prop : default = TRUE, 확률값이 나오고, FALSE는 결측치의 개수.
# number : default = FALSE,오른쪽의 각 대응되는 패턴의 개수를 의미한다.
# sortVar : sort_values
miss <- aggr(airquality, prop=FALSE ,numbers=TRUE, sortVar=TRUE)

summary(miss)$combinations

# matrixplot ----
# sortby : 해당 열을 가지고 정렬을 해준다.
# 내림차순으로 정령해준다. 
matrixplot(airquality, sortby= 'Month') # Month 대신에 5 숫자로 넣어도 가능.

# marginplot ----
# scatterplot은 Solar.R과 Ozone의 관계를 보여주며, 누락된 값은 제외한다.
# Y축 margin region의 boxplot은 Solar.R 변수값이 존재하는 경우와 누락된 경우의 Ozone 변수의 분포.
# X축 margin region의 boxplot은 Ozone 변수값이 존재하는 경우와 누락된 경우의 Solar.R 변수의 분포.

# 결측값은 빨간색으로 표시되어 있다. (orangered)
# Y축 barplot 아래의 숫자는 Ozone 변수에 결측치가 발생한 케이스의 수
# X축 barplot 아래의 숫자는 Solar.R 변수에 결측치가 발생한 케이스의 수
# 구석의 교차지점은 두 변수 모두 결측치가 발생한 케이스의 수 

marginplot(airquality[c('Solar.R', 'Ozone')],
           pch=20, col=c('cornflowerblue', 'orangered', 'purple'))


# correlation ----

# boolean 값에 abs를 해주면 0, 1 로 변환된다. 
x <- data.frame(abs(is.na(airquality)))
head(airquality)
head(x)

# 결측치가 있는 columns을 가져온다.
# 결측치가 있는 변수 간 상관계수를 check할 때 그닥 유의미하다고는 볼 수 없다.
y <- x[colSums(x) > 0]
cor(y)

with(y, cor.test(Ozone, Solar.R))

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

head(y)


y %>% 
  cor.test(Ozone, Solar.R)

with(y, cor.test(Ozone, Solar.R))


cor.test(y$Ozone, y$Solar.R)


