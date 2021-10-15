# car : Prestige 데이터 셋이 있음.
library(car)

# education, income, women, prestige, census, type
str(Prestige)


Prestige.lm <- lm(income ~ education, data = Prestige)

# income = -2853.6 + 898.9*eduction
Prestige.lm

plot(Prestige$income ~ Prestige$education, 
     col = 'cornflowerblue', pch = 19, 
     xlab = 'Education (years)', ylab = 'Income (dollars)',
     main = 'Education and Income')
abline(Prestige.lm, col = 'salmon', lwd=2)

# 교육기간과 소득은 양의 상관관계를 갖는다. 교육기간이 1년 증가할 때마다 소득은 898.8달러 증가한다.
# lm 에 사용할 수 있는 추출 함수는 다음과 같다.
# anova : 분산분석
# coefficient / coef : 회귀계수
# confint : 회귀계수의 신뢰구간
# fitted : 회귀식에 의한 예측값
# residuals / resid : 잔차
# summary : 주요 분석 정보(잔차, 회귀계수, R^2, F-value etc.)


Prestige.lm.summary <- summary(Prestige.lm)

# summary 를 통해서 전반적인 회귀분석 결과를 도출할 수 있지만, 용도별 추출함수를 이용하면
# 조금 더 자세한 결과를 확인할 수 있다. 
coef(Prestige.lm.summary)

# anova에서 필요한 특정 원소만 추출할수도 있다.
anova(Prestige.lm)

rownames(anova(Prestige.lm)); colnames(anova(Prestige.lm))
anova(Prestige.lm)['education', 'Pr(>F)']

anova(Prestige.lm)[1, 4]

# 계수만 출력할수도 있다. 
coef(Prestige.lm)

confint(Prestige.lm)

# 회귀식에 의한 예측값을 벡터로 반환해준다. 
fitted(Prestige.lm)[1:3]

# 잔차를 보여준다.
resid(Prestige.lm)[1:3]

# 예측을 할 때에도 사용을 할 수 있다. 
# Prestige.new == test data 
Prestige.new <- data.frame(education=c(5, 10, 15))
Prestige.new
# 예측을 할 때에는 해당 모델을 넣고 다음에 newdata 라는 인자를 통해 새로운 값을 집어넣어준다.
# predict(lm, newdata = )
predict(Prestige.lm, newdata = Prestige.new)

predict(Prestige.lm, newdata = Prestige.new, interval = 'confidence')


# 데이터 전체가 아닌 일부만 하고싶을 경우 다음과 같이 한다.
mean(Prestige$education)

# education 이 평균보다 큰 데이터셋에 한해서만 분석을 시행한다.
lm(income ~ education, data=Prestige, subset = (education > mean(education)))

lm(income ~ education, data=Prestige, subset = (education <= mean(education)))

# 비교를 해보면 5배나 절편값이 크다 .
# 교육을 더 많이받은 직업은 교육기간이 1년 늘어나면 소득이 급증한다.
# 단일 변수보다는 굴절을 갖는 곡선이 조금 더 성능이 좋을 수 있다고 생각할 수 있다.
