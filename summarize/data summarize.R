# summarize

# Categorical variable ----

if (!require('MASS')) install.packages(MASS)
library(MASS)

str(survey)

# 레벨을 가지는 factor형 변수이다.
# table ----
levels(survey$Smoke)

frqtab <- table(survey$Smoke)
frqtab

class(frqtab)

frqtab[2]

frqtab==max(frqtab)


frqtab[frqtab==max(frqtab)]

names(frqtab[frqtab==max(frqtab)])


# which.max ----
which.max(frqtab) # max value의 위치에 대한 값을 반환해준다.

frqtab[which.max(frqtab)]

names(frqtab[which.max(frqtab)])


# prop.table ----
frqtab.prop <- prop.table(frqtab)

frqtab.prop['Never']


# broadcasting 해서 계산이 된다.
frqtab.prop * 100


# 상대적 빈도, 비율을 구할 수 있다.
sum(survey$Smoke == 'Never', na.rm = TRUE) / length(survey$Smoke)

mean(survey$Smoke == 'Never', na.rm=TRUE)

anorexia

mean(anorexia$Postwt > anorexia$Prewt)

# 정규분포에서 2 sigma 보다 멀리 있는 건 대충 99% 밖에 있다고 표현한다.
mean(abs(mammals$brain - mean(mammals$brain)) > 2*sd(mammals$brain))

# diff ----
# diff 함수는 주어진 벡터로부터 연속된 두 숫자의 차이를 계산하여 반환한다. 
mean(diff(SP500) > 0)



if (!require('vcd')) install.packages('vcd')
library(vcd)

# 류마티스 관절염 데이터 
str(Arthritis)

crosstab <- table(Arthritis$Improved, Arthritis$Treatment)
crosstab

crosstab['Marked', 'Treated']

?xtabs
# xtabs ----
crosstab <- xtabs(~ Improved + Treatment, data = Arthritis, sparse = FALSE)
crosstab


# margin.table ----
crosstab
# margin = 1 : row  / 2 : column
margin.table(crosstab, margin=1)

# 동일하다 margin이 생략된거.
prop.table(crosstab, 2)

# margin을 하지 않으면 전체 비율로 된다.
prop.table(crosstab)


# addmargins ----
addmargins(crosstab, margin = 1)


# 비율의 교챠표에 addmargins 함수를 적용하면 sum column 까지 제공해주어 편하다.
prop.table(crosstab, 2)

addmargins(prop.table(crosstab, 2), 1)

addmargins(prop.table(crosstab, 2), 2)


# CrossTable ----
# 다양한 정보를 담고 있는 교차표를 생성할 수 있다. 
if (!require('gmodels')) install.packages("gmodels")
library(gmodels)

# dnn 안에서 행의 이름과 열의 이름을 지정해줄 수 있다. 
# CRBD?
# randomized Complete Block Design
CrossTable(Arthritis$Improved, Arthritis$Treatment, prop.chisq=FALSE, 
           dnn = c('Improved', 'Treatment'))

# format : SAS, SPSS
CrossTable(Arthritis$Improved, Arthritis$Treatment, prop.chisq=TRUE, chisq=TRUE, fisher = TRUE, resid=TRUE, 
           dnn = c('Improved', 'Treatment'))



# ftable ----
# 둘다 동일하다.
multtab <- with(Arthritis, table(Improved, Sex, Treatment)) # 마지막에 오는건 두개로 만들어주네.
multtab <- xtabs(~ Improved + Sex + Treatment, data = Arthritis)
multtab

ftable(multtab) # default : row.vars = c(1, 2), col.vars = c(3)


ftable(multtab, row.vars = c(1, 2, 3))

ftable(multtab, row.vars = c(1, 2))

ftable(multtab, col.vars = c(1, 2))

ftable(multtab, col.vars = c(1, 2, 3))


# 3차원 확장
margin.table(multtab, 1)

margin.table(multtab, 2)

margin.table(multtab, 3)

margin.table(multtab, c(1, 3))


# Sex와 Treatment에 대한 Improved 세부 값을 구할 수 있다. 
ftable(prop.table(multtab, c(2, 3)))

ftable(addmargins(prop.table(multtab, c(2, 3)), 1))





# continuous variable ----
# median ----
median(survey$Pulse, na.rm=TRUE)

# quantile ----
quantile(survey$Pulse, probs = 0.05, na.rm = TRUE)

quantile(survey$Pulse, probs = 0.5, na.rm = TRUE)

quantile(survey$Pulse, probs = c(0.05, 0.95), na.rm = TRUE)

quantile(survey$Pulse, na.rm=TRUE)


# mean ----
mean(survey$Pulse <= 80, na.rm=TRUE)
iris.lst <- as.list(iris)
summary(iris.lst)

# range ----
range(survey$Pulse, na.rm=TRUE)

# var ----
var(survey$Pulse, na.rm=TRUE)

# sd ----
sd(survey$Pulse, na.rm=TRUE)

str(mtcars)


# pastecs ----
# nbr.val : 관측값 개수, nbr.null : null 개수, nbr.na : na 개수

if (!require('pastecs')) install.packages('pastecs')

library(pastecs)

stat.desc(mtcars[c('mpg', 'hp', 'wt')])


# psych ----
if (!require('psych')) install.packages('psych')

library(psych)

# skew : 왜도, kurtosis : 첨도
describe(mtcars[c('mpg', 'hp', 'wt')])

# tapply ----
# 첫번째 인수에는 벡터 형식의 데이터 셋을 지정하ㅏ고,
# 두번째 인수에는 집단을 나타내는 범주형 변수
# 세번째 인수에는 사용하고자 하는 통계량 계산 함수.
tapply(survey$Pulse, INDEX = survey$Exer, FUN=mean, na.rm=TRUE)

tapply(survey$Pulse, list(survey$Exer, survey$Sex), FUN=mean, na.rm=TRUE)


# aggregate ----
# 비슷하지만 출력 형태가 조금 다르다.
aggregate(survey$Pulse, by = list(Exercise = survey$Exer), FUN=mean, na.rm=TRUE)

aggregate(survey$Pulse, by = list(Exercise = survey$Exer , Sex = survey$Sex), FUN=mean, na.rm=TRUE)


# aggregate는 tapply와 달리 데이터프레임 형식의 데이터셋을 처리할 수 있다. 
# tapply는 첫번째 인수에는 vector 형태가 들어가야한다. 
aggregate(survey[c('Pulse', 'Age')],
          list(Exercise = survey$Exer), mean, na.rm=TRUE)

# 내부 FUN은 lambda처럼 지정해줄 수 도있다. 
 myStats <- function(x, na.rm=FALSE){
   if (na.rm) x <- x[!is.na(x)]
   n <- length(x)
   mean <- mean(x)
   sd <- sd(x)
   skew <- sum((x-mean)^3/sd^3) / n
   kurt <- sum((x-mean)^4/sd^4) / n -3
   return (c(n=n, mean=mean, sd=sd, skewness = skew, kurtosis=kurt))
 }

 aggregate(survey[c('Pulse', 'Age')],
           list(Exercise = survey$Exer, Sex = survey$Sex), FUN=myStats, na.rm=TRUE)


# by ----
by(survey[c('Pulse', 'Age')], INDICES=list(Exercise = survey$Exer), FUN=summary)

aggregate(survey[c('Pulse', 'Age')], list(Exercise = survey$Exer), FUN=summary) 

by(survey[c('Pulse', 'Age')], list(Exercise = survey$Exer), FUN= function(x) sapply(x, myStats, na.rm=TRUE))

# describeBy ----
# describeBy 는 aggregate나 by와 달리 사용자 정의 함수를 정하지 못한다. 

describeBy(survey[c('Pulse', 'Age')], group=list(Exercise = survey$Exer))

