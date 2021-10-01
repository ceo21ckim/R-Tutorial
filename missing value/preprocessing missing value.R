# 581p ----

# preprocessing with missing values
# listwise deletion ----
# listwise deletion은 하나라도 결측치가 있으면 제거하는 방식이다. 
# 이 경우 앞단에서 사용한 complete.cases나 na.omit을 사용한다. 

cor(airquality[complete.cases(airquality),])

cor(na.omit(airquality))

anova(lm(Ozone~Temp, data = na.omit(airquality)))

# 동일한 결과
# method : perason, kendall, spearman 
# use : everthing, all.obs, complete.obs, na.or.complete, pairwise.complete.obs
cor(airquality, use='complete.obs') 

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

# pairwise deletion ----
cor(airquality, use='pairwise.complete.obs')

# corrplot ----
# method : circle, square, ellipse, number, shade, color, pie
corrplot(cor(airquality, use='pairwise.complete.obs'), method = 'circle')
plot(airquality)

# 37개의 결측치가 제거된 것을 볼 수 있다. 
# lm 에서는 pairwise deletion을 하는데, 이 경우 변수간 NA값을 제거하기 때문에 
# 다른 변수가 들어갈 경우 결측치의 개수가 달라져 분석에 문제가 생길 수도 있다. 
air.lm <- lm(Ozone ~Temp, data=airquality)
summary(air.lm)


# simple imputation ----
# 평균값이나 중위수 등 하나의 값으로 채워넣는 것을 말한다. 
airquality.new <- airquality
for ( i in 1:ncol(airquality.new)){
  if(sum(is.na(airquality.new[,i])) > 0){ # 결측치가 하나라도 있는 것을 찾는다.
    na.idx <- which(is.na(airquality.new[,i])) # 해당 위치의 idx를 저장한다.
    airquality.new[na.idx, i] <- mean(airquality.new[,i], na.rm=TRUE) # 저장한 idx에 mean값을 대체한다.
  }
}

mean(airquality$Ozone, na.rm=TRUE)
mean(airquality$Solar.R, na.rm=TRUE)

# multi imputation ----
# mice ----
# 결측치 제거하는 pack.에는 Aemlia, mi 등이 있다. 
if (!require(mice)) iinstall.packages('mice')

library(mice)

# method : norm.predict, mean, pmm(predictive mean matching)
# 데이터가 연속형일 경우 pmm을 쓴다. 
# m : 생성할 대체값의 개수 default = 5 
# maxit : 알고리즘 반복 횟수 default = 5
imp <- mice(airquality, method='mean', m=1, maxit=1)
imp

head(complete(imp))

# nhanes dataset
# columns : age, bmi, hyp(고혈압 여부), chl(혈청콜레스테롤)

str(nhanes)

head(nhanes)

imp <- mice(nhanes, seed = 123)
imp

# attributes : mice() 함수로부터 반환된 객체에 저장된 정보 목록을 추출하는 함수 
attributes(imp)

imp$data

imp$imp
# 3번째 대체 데이터셋을 추출.
c3 <- complete(imp, action=3)
md.pattern(c3)

# 5개의 대체 데이터셋을 long format으로 출력한다. 
c.long <- complete(imp, action = 'long')
c.long

c.median <- aggregate(c.long[3:6], by=list(id=c.long$.id), median)
head(c.median[-1])


ini <- mice(nhanes, maxit = 0 )
pred <- ini$predictorMatrix
pred
pred[,'hyp'] <- 0
pred

imp <- mice(nhanes, predictorMatrix = pred)
imp


# quickpred ----
# 일정 기준에 따라 자동으로 예측변수를 선택할 수 있다.
# 상관계수가 0.3이상인 변수만을 선택.

imp <- mice(nhanes, predictorMatrix = quickpred(nhanes,mincor=0.3))
imp


# nhanes2 : nhanes 데이터와 동일하지만, age와 hyp변수가 factor로 되어있다.
str(nhanes2)

# data가 범주형인 경우 logistic reg. 를 사용한다.
imp <- mice(nhanes2)
imp


# 사용가능한 결측치 추정 방법을 확인할 때 출력해보자.
methods(mice)


ini <- mice(nhanes2, maxit=0)
meth <- ini$method
meth['bmi'] <- 'norm.predict'

imp <- mice(nhanes2, method=meth)
imp

# 두 값이 동일하다.
summary(aov(lm(age~ bmi+hyp+chl, data = nhanes)))
anova(lm(age~ bmi+hyp+chl, data = nhanes))


# stripplot ----
# 빨간색은 대체값, 파란색은 관측값
imp <- mice(nhanes2, seed=123)
stripplot(imp, bmi~.imp, pch=21, cex=1.5)
stripplot(imp, pch=21, cex=1.2)

nhanes
head(nhanes)
# imp <- mice(nhanes2, m=1, seed=123)
fit <- with(imp, lm(chl~bmi+hyp))
fit

with(nhanes, lm(chl~bmi+hyp))


summary(fit$analyses[[3]])


# pool ----
# mice에서 m=5로 설정했기 때문에 5개의 예측값을 평균내어 lm을 해주는 함수
pooled <- pool(fit)
pooled

summary(pooled)





