# ANOVA ----
# 집단을 구분하는 범주형 변수가 한 개인 경우를 one-way ANOVA라고 하며, 두 개인 경우 two-way ANOVA라고 한다.
# 삼원분산분석, 사원분산분석도 이론상 가능하지만 해석상의 어려움으로 한개 내지 두개의 범주형 변수를 다룬다.

# 차이가 없다는 귀무가설을 기각할 수 있다. 
pf(9.59, df1 =1, df=8, lower.tail = FALSE)


qf(0.05, df1=1, df=8, lower.tail = FALSE)

# one-way ANOVA ----
# 여섯종류의 살출제를 각각 12개의 실험 공간에 살포한 후 살아남은 해충의 개수를 기록했다.
# spray : 살충제 종류
# count : 살아남은 해충의 수 
str(InsectSprays)

anova(lm(count ~ spray, data = InsectSprays))
aov(lm(count ~ spray, data = InsectSprays))
lm(count ~ spray, data = InsectSprays)

# resid
sum(resid(aov(lm(count ~ spray, data = InsectSprays))))

# 평균은 F가 가장 크다.
tapply(InsectSprays$count, InsectSprays$spray, mean)


# 표분편차는 F가 가장 크다. 
tapply(InsectSprays$count, InsectSprays$spray, sd)


# 길이는 동일하다.
tapply(InsectSprays$count, InsectSprays$spray, length)


library('gplots')
plotmeans(count ~ spray, data = InsectSprays,
          barcol = 'tomato', col = 'cornflowerblue', lwd = 2, barwidth = 3,
          xlab = 'Type of Sprays', ylab = 'Insect Count', 
          main = 'Performance of Insect Sprays \n with 95% Confidence Interval of Mean')



boxplot(count ~ spray, data = InsectSprays, col = 'tomato', 
        xlab = 'Type of Sprays', ylab = 'Insect Count', 
        main = 'Performance of Insect Sprays')

spray.aov <- aov(count ~ spray, data = InsectSprays)
spray.aov


anova(lm(count ~ spray, data = InsectSprays))

# Df : 자유도
# Sum Sq : 제곱합
# Mean Sq : 제곱합을 자유도로 나눈 분산
# F : MStreatment / MSerror
summary(spray.aov)



# 다중비교
# 위 분산분석을 통해서 여섯 종류 살충제 간 살충효과의 차이는 없다라는 귀무가설을 기각할 수 있었다.
# 이 결과를 가지고 모집단 평균이 모두 동일하다는 주장을 기각할 수 있을 뿐이며, 
# 어느 집단이 서로 달라서 이러한 결과가 나왔는지 알지 못한다.
# 다음 함수를 사용해 개별 집단 간 평균의 차리르 확인할 수 있다. 

# model.table ----

# default type = 'effects' 를 지정해주면 집단별로 각 집단평균과 전체 평균의 차이를 볼 수 있다.
model.tables(spray.aov, type='mean')


model.tables(spray.aov, type='effects')

# anova로 군집 간 평균의 차이가 있다는 것을 파악했으니, 어떤 군집끼리 연관이 있는지 사후 평가를 한다.

spray.compare <- TukeyHSD(spray.aov)

# p adj : 평균 차이에 대한 p-value
spray.compare

if (!require('multcompView')) install.packages('multcompView')
library(multcompView)

plot(spray.compare , las=1 , col="blue")



spray.compare$spray['D-C', ]


# mulcomp ----
# 조금더 다양한 다중비교를 위한 방법을 제공한다.
# mcp(spray = 'Tukey') spray 변수에 의한 구분되는 집단을 바탕으로 Tukey 다중비교를 수행한다
# cld : glht() 함수의 결과 객체로 부터 모든 범주쌍의 비교결과를 알파벳 문자형식으로 출력한다.
# 그 후 그래프를 그림으로써 다음과 같은 boxplot을 구현해낼 수 있다. 

if (!require('multcomp')) install.packages('multcomp')
library(multcomp)

tuk.hsd <- glht(model=spray.aov, linfct=mcp(spray='Tukey'))
plot(cld(tuk.hsd, level=0.05), col = 'orange')


