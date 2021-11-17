# poisson regression ----
if (!require('robust')) install.packages('robust')

# robust -> breslow.dat
library(robust)

data(breslow.dat)

str(breslow.dat)

seizure <- breslow.dat[c('Base', 'Age', 'Trt', 'sumY')]

summary(seizure)


hist(seizure$sumY, breaks = 20, col='cornflowerblue', 
     xlab = 'seizure Count', main = 'Distribution of Seizures')

seizure.poisson <- glm(sumY ~ Base + Age + Trt, data = seizure, family = poisson())
summary(seizure.poisson)

coef(seizure.poisson)


exp(coef(seizure.poisson))

# 과산포 ----
# 과산포는 종종 상태의존성이라는 현상으로 인해 발생함.
# 포아송분포는 각 사건이 독립적이고 사건발생률이 일정하다고 가정함.
# 잔차이탈도 대 잔차자유도의 비율이 1을 크게 상회하면 과산포를 의심해야함.
deviance(seizure.poisson) / df.residual(seizure.poisson)

if (!require('qcc')) install.packages('qcc')
library(qcc)

# qcc -> overdispersion.test ----
# 과산포 가능성을 검정하는 패키지.
qcc.overdispersion.test(seizure$sumY, type = 'poisson')

# 과산포 문제가 존재할때에도 포아송 분포를 사용할 수 있다. 
# family = poisson -> family = quasipoisson

seizure.poisson <- glm(sumY ~ Base + Age + Trt, data = seizure, 
                       family = quasipoisson())

summary(seizure.poisson)

# poisson을 가지고 회귀분석을 수행했을 때는 Age, Trt 모두 유의미한 변수라는 결과가 도출되었지만
# quasipoisson을 가지고 회귀분석을 수행했을 때는 Age, Trt 는 유의미한 변수가 아니라는 결과가 도출.