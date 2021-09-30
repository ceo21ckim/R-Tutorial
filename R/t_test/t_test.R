#T test

# t = (x.bar - mean(x)) / (standard error / sqrt(n))
# t-value ( t statistic ) 을 test staticic으로 활용하여 검정한다. 

# 


# chi-squared test

if (!require('devtools')) install.packages("devtools")

devtools::install_github("cardiomoon/webr")

require(moonBook)
require(webr)

# chi-squared test
x=chisq.test(table(acs$sex,acs$DM))
x

plot(t.test(mtcars$mpg, mu=22))

plot(rt(10, 4))
x <- t.test(rt(10, 4))
y <- t.test(rt(10, 9))
z <- t.test(rt(10, 1000))

plot(x, col = 'red')

plot(y, col = 'blue')

plot(z, col = 'green')


##################################################################

pt(3.58, df=19, lower.tail=FALSE) * 2

# 검정통계량을 확인하고 싶을 때 사용한다.
qt(0.005, df = 19, lower.tail = FALSE)

# Sex : 성별
# Bwt : 몸무게 (kg)
# Hwt : 심장무게 (gram)
library(MASS)
str(cats)

t.test(x=cats$Bwt, mu=2.65)

plot(t.test(x=cats$Bwt, mu=2.7))


# one - side test ----
t.test(cats$Bwt, mu=2.6, alternative = 'greater')

t.test(cats$Bwt, mu=2.6, alternative = 'less')


cats.t <- t.test(cats$Bwt, mu=2.6)
str(cats.t)

cats.t$statistic

cats.t$conf.int

cats.t$p.value

t.test(cats$Bwt, mu=2.61, conf.level = 0.96)

# prop.test ----
prop.test(x = 18, n = 30, p = 0.5, alternative = 'greater')


# two - independent t test -----

t.test(formula = Bwt ~ Sex, data = cats)

bars <- tapply(cats$Bwt, cats$Sex, mean)
lower <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[1])
upper <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[2])

if (!require('gplots')) install.packages('gplots')

library(gplots)


barplot2(bars, space=0.4, ylim=c(0, 3.0),
         plot.ci=TRUE, ci.l=lower, ci.u=upper, ci.color='maroon', ci.lwd=4,
         names.arg=c('Female', 'Male'), col=c('coral', 'darkkhaki'),
         xlab='Cats', ylab='Body Weight (kg)',
         main='Body Weight by Sex \n with Confidence Interval')

Bwt.f <- cats$Bwt[cats$Sex=='F']
Bwt.m <- cats$Bwt[cats$Sex=='M']

t.test(Bwt.f, Bwt.m)


smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)


# 폐질환자 대비 흡연자 비율을 확인
# 네 병원의 폐질환자 대비 흡연자 비율이 같다는 귀무가설을 기각한다.
prop.test(smokers, patients)

str(sleep)

# paired - smaples t test

sleep[seq(1, 20, 2), ]  

# extra = 수면제를 먹은 후 수면시간의 차이
t.test(extra ~ group, data = sleep, paired = TRUE)
library(tidyr)
# wide format
sleep.wide <- spread(sleep, key=group, value=extra)
sleep.wide
# 위 분석 결곽와 동일하다.
t.test(sleep.wide$'1', sleep.wide$'2', paired=TRUE)



########################################################
x1 <-c(246.3,255,245.8,250.7,247.7,246.3,214,242.7,287.5,284.6,268.7,302.6,248.3,243.7,276.7,254.9)
x2 <-c(340.7,270.1,371.6,306.6,263.4,341.6,307,319.1,272.6,332.6,362.2,358.1,271.4,303.9,324.7,360.1)

# var test / F test
var.test(x1, x2)




tstar = function(x1,x2,mu){
  a=(mean(x1) - mean(x2)-mu) / sqrt(var(x1)/length(x1) + var(x2)/length(x2))
  a
}
# tstar 를 구했을 때 -5.882092가 나온다 
tstar(x1,x2,mu = 0)
# default가 5자리에서 반올림하는 것을 확인할 수 있다. 
t.test(x1,x2,mu=0)


df = function(x1,x2){
  n1 = length(x1) ; n2 =length(x2)
  a=((var(x1)/n1+var(x2)/n2)^2 / ((var(x1)/n1)^2/(n1-1)+(var(x2)/n2)^2/(n2-1)))
  a
  
} 

df(x1,x2)


pt(-5.882092,24.64349)*2 
t.test(x1,x2,mu=0)$p.value

pv <- function(x1,x2,l){
  n1 = length(x1) ; n2 = length(x2)
  tstar=(mean(x1) - mean(x2)-l) / sqrt(var(x1)/n1+ var(x2)/n2)
  df=((var(x1)/n1+var(x2)/n2)^2 / ((var(x1)/n1)^2/(n1-1)+(var(x2)/n2)^2/(n2-1)))
  
  cat(pt(tstar,df)*2)
}

pv(x1,x2,0)
t.test(x1,x2,mu=0)$p.value


pvalu = function(tstar,df){
  if(tstar>0) {result <-pt(tstar,df,lower.tail=F)*2}
  else {result<-pt(tstar,df)*2}
  cat(result)
}
pvalu(tstar(x1,x2,0),df(x1,x2))
pvalu(-5.882092,24.64349) 




p.value = function(tstar,df,sided){
  if(sided=="two.sided"){
    ifelse(tstar>0,p<-pt(tstar,df,lower.tail=F)*2,p<-pt(tstar,df)*2)
  }
  if(sided=="less"){
    p<-pt(tstar,df)
  }
  if(sided=="greater"){
    p<-pt(tstar,df,lower.tail=F)
  }
  
  p
}

all.equal(p.value(tstar(x1,x2,0),df(x1,x2),"two.sided"),t.test(x1,x2,"two.sided")$p.value)

all.equal(p.value(tstar(x1,x2,0),df(x1,x2),"less"),t.test(x1,x2,"less")$p.value)

all.equal(p.value(tstar(x1,x2,0),df(x1,x2),"greater"),t.test(x1,x2,"greater")$p.value)

# 아직 alpha는 고려하지 않았으므로 완전 치 못하다.

pt(-5.882092,24.64349)*2

p.value(tstar(x1,x2,0),df(x1,x2),"two.sided")
p.value(tstar(x1,x2,0),df(x1,x2),"less")
p.value(tstar(x1,x2,0),df(x1,x2),"two.sided")

t.test(x1,x2,mu=0)$p.value