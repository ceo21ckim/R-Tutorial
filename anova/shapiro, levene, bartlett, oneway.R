str(InsectSprays)

spray.aov <- aov(count ~ spray, data = InsectSprays)

# car package 안의 qqplot을 통해 정규성 검정을 해야한다. 
library(car)

qqPlot(InsectSprays$count, pch = 20, col  = 'deepskyblue', id = FALSE,
       main = 'Q-Q plot', xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles')


# 위 그래프와 같이 그래프만으로는 명확하게 정규성 검정을 하기 어려울 경우 shapiro-Wilk test를 사용한다.

# shapiro.test ----
shapiro.test(InsectSprays$count)
# p - value가 엄청나게 낮은 것을 확인할 수 있다. 

# outlierTest ----
# car package 의 outlierTest()를 통해 통계적으로 outlier를 검정할 수 있다.
# Bonferroni p 값을 통해 outlier가 있는지 test할 수 있다. 
outlierTest(spray.aov)


# leveneTest ----
# 레벤 검정의 가설은 집단 간 분산이 동일하다는 가정을 하고 분석을 한다.
# p-value가 작게 나왔기 때문에 등분산을 가정할 수 없다.
leveneTest( count ~ spray, data = InsectSprays)


# bartlett.test ----
# 바틀렛 검정 역시 레벤 검정과 동일하게 등분산을 체크하는 분석이다. 
bartlett.test(count ~ spray, data = InsectSprays)

#위 등분산 검정 분석을 통해 등분산의 가정을 충족하지 못할 경우 oneway.test 함수를 이용해 일원분산분석을 수행한다.
# oneway.test와 anova가 차이가 있는 것을 볼 수 있다.
# oneway.test는 등분산성을 가정하지 않는 반면, anova는 등분산임을 가정하고 분석을 진행하기 때문이다.

oneway.test(count ~ spray, data = InsectSprays)

summary(aov(count ~ spray, data = InsectSprays))



